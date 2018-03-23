000100080117      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF)
000200080117      //---------------------------------------------------------------
000300080513      //?INVIO LETTERA PREAVVISO DANNO LT AL CLIENTE
000400080117      //---------------------------------------------------------------
000500080117      // Richiamato da FIDN03R x le C.A. con le segg. caratteristiche:
000600080117      // 1) Fase 100
000700080513      // 2) Liquidazione transattiva
000800130410      // 3) Fase 900 Liquidazione transattiva Avaria
000900130410      //    per clienti abilitati da tabella DKL
001000080117      //---------------------------------------------------------------
001100080117
001200080117     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001300080117
001400080117      //---------------------------------------------------------------
001500080117      //?Dichiarazione file.
001600080117      //---------------------------------------------------------------
001700080118
001800080117     fFNDCT01L  if   e           k disk
001900080117     fFNDET01L  if   e           k disk
002000080117     fFNDCS01L  if   e           k disk
002100080117
002200080117     fFNARB01L  if   e           k disk
002300080117     fFNBLP01L  if   e           k disk    prefix(ARB:3)
002400080117
002500080117     fAZORG01L  if   e           k disk
002600080117
002700080513     fFIDN67P   o    e             printer usropn
002800080117     f                                     OFlind(*in01)
002900080513     fFIDN67PM  o    e             printer usropn
003000080515     f                                     OFlind(*in02)
003100080117
003200080117      //---------------------------------------------------------------
003300080117      //?Definizione costanti.
003400080117      //---------------------------------------------------------------
003500080117
003600080117
003700080515     d I_picpod        c                   const('UFF. INTERNAZIONALE')
003800080515
003900080117      // - Comandi di override al PrtF
004000080117     d C_CmdDltOvr     c                   const('DLTOVR +
004100080515     d                                            file(FIDN67P)')
004200080117
004300080513     d C_CmdDltOvrM    c                   const('DLTOVR +
004400080515     d                                            file(FIDN67PM)')
004500080513
004600080117      //---------------------------------------------------------------
004700080117      //?Definizione schiere.
004800080117      //---------------------------------------------------------------
004900080118
005000080117      // - Descrizione Tipo Anomalia
005100080117     d $TAD            s             35    dim( 3) inz                          descr. tipo anomalia
005200080117
005300080117      // - Note da stampare
005400080117     d $Note           s            105    dim( 5) inz                          $TAD x 5
005500080117
005600080515      // - Comandi x OVRPRTF
005700080515     d $Cmd            s             85    dim( 3) ctdata perrcd(1)
005800080117      //---------------------------------------------------------------
005900080117      //?Definizione aree dati.
006000080117      //---------------------------------------------------------------
006100080118
006200080117      // - Dati utente
006300080117     d §AzUte        e ds                  extname(AZUTE00F)
006400080117     d                                     dtaara
006500080117     d §DatiUte      e ds                  extname(dDatiUte)
006600080117     d                                     dtaara
006700080117
006800080117      //---------------------------------------------------------------
006900080117      //?Definizione strutture dati.
007000080117      //---------------------------------------------------------------
007100080118
007200080117      // - Status
007300080117     d Psds           sds
007400080117     d   SDSpgm          *proc
007500080117
007600080117      // - Parametri ricevuti
007700080117     d KPJBA         e ds
007800080117     d FIDN00ds      e ds
007900080117     d   Wnumca                6     19
008000080117
008100080117      // - Reperimento dati utente
008200080117     d TIBS34ds      e ds
008300080515
008400080117      // - Flag operativi in FNDCT
008500080117     d dDCT01        e ds                  inz
008600080117
008700080117      // - Ricerca/Controllo tabelle
008800080117     d TIBS02ds      e ds                  inz
008900080117      // - tab. MRA/DAN = Bart-Maling - Danni
009000080117     d dMRAdan       e ds
009100080117      // - tab. TAD = Tipi anomalia danni
009200080117     d dTAD          e ds                  inz
009300080515      // - tab. TLD = Testi lettera danni
009400080515     d dTLD          e ds                  inz
009500080117
009600080515      // - Parametri x Reperimento terminal di arrivo/partenza
009700080515     d FNLV55ds      e ds                  inz
009800080515     d   D55tla      e                     inz('L')
009900080515     d   D55tpt      e                     inz('P')
010000080514      // - Parametri x Controllo validità numero fax
010100080514     d TRUL42ds      e ds                  inz
010200080514      // - Parametri x Reperimento USRDFNDTA x comunicazione e-mail
010300080514     d TRUL44ds      e ds                  inz
010400080514      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
010500080514     d TRTCM1ds      e ds                  inz                                  X Invio e-mail
010600090108
010700090108      // - Reperimento dati anagrafici beneficiario
010800090108     d FIDNBEds      e ds
010900080514
011000080117      // - Parametri x Controllo & Inversione date
011100080117     d WLBdat          ds                  inz
011200080117     d  G02dat                 1      8  0 inz
011300080117     d  G02inv                 9     16  0 inz
011400080117     d  G02err                17     17    inz('3')
011500080117     d  G02tgi                18     22  0 inz
011600080513
011700080513      * Ridefinizione sottoconto
011800080516     d wDS_ksc         ds                  inz
011900080513     d  w0030                         3  0 inz
012000080513     d  w0040                         4  0 inz
012100080117
012200080117      //---------------------------------------------------------------
012300080117      //?Definizione variabili globali.
012400080117      //---------------------------------------------------------------
012500080118
012600080117      // - Flags booleani
012700080117     d W_Esito         s               n   inz(*off)
012800080514      //- Campi di comodo x invio e-mail
012900080516     d $Invio          s              1    inz
013000080514     d W_mail          s             60    inz
013100080514     d KSC_mail        s                   inz like(D44ksc)
013200080514     d $English        s              1    inz(*off)
013300080514     d $E_Mail         s              1    inz(*off)
013400080117
013500080117      // - Indici di schiera
013600080118     d YY              s              3  0 inz
013700080516     d XX              s              3  0 inz
013800080117
013900080117
014000080117      // - Campi di comodo
014100080118     d Wtad            s                   like(DCTtad)   inz
014200090108     d Kcli            s                   like(DCTksc)   inz
014300080516     d w§DCTport       s                   like(§DCTport) inz
014400080515     d w§TLDstp        s                   like(§TLDstp1) inz
014500080514
014600080514      // - Parametri x TRULOUTQ
014700080514     d PrmFine         s              1    inz
014800080514     d PrmFil          s              3s 0 inz
014900080514     d PrmTpOutQ       s              1    inz
015000080514     d PrmOutQ         s             10    inz
015100080514     d PrmOutQLib      s             10    inz
015200080514     d PrmEsito        s              1    inz
015300080117
015400080516      // - Campi per QCMDEXC
015500080516     d Qcmd            s            512    inz
015600080516
015700080117      //---------------------------------------------------------------
015800080117      //?Definizione procedure usate.
015900080117      //---------------------------------------------------------------
016000080118
016100080117      // - Reperimento dati utente
016200080117     d tibs34r         pr                  extpgm('TIBS34R')
016300080117     d  tibs34ds                           likeds(TIBS34ds)
016400080117
016500080516      // - Reperimento terminal di arrivo/partenza
016600080516     d fnlv55r         pr                  extpgm('FNLV55R')
016700080516     d  fnlv55ds                           likeds(FNLV55ds)
016800090108
016900090108      // - Reperimento beneficiario
017000090108     d fidnber         pr                  extpgm('FIDNBER')
017100090108     d  fidnbeds                           likeds(FIDNBEDS)
017200080516
017300080117      // - Gestione tabelle
017400080117     d tibs02r         pr                  extpgm('TIBS02R')
017500080117     d  kpjba                              likeds(kpjba)    const
017600080117     d  tibs02ds                           likeds(TIBS02ds)
017700080117
017800080117      // - Controllo & Inversione date
017900080117     d xsrda8          pr                  extpgm('XSRDA8')
018000080117     d  wlbdat                             likeds(WLBdat)
018100080117
018200080117      // - Esecuzione comando di sistema
018300080117     d qCmdExc         pr                  extpgm('QCMDEXC')
018400080117     d  Qcmd                        512    const  options(*varsize)
018500080117     d  Qlen                         15  5 const
018600080117
018700080516      // - Controllo stampanti
018800080516     d TRULOUTQ        pr                  extpgm('TRULOUTQ')
018900080516     d PrmFine                        1
019000080516     d PrmFil                         3s 0
019100080516     d PrmTpOutQ                      1
019200080516     d PrmOutQ                       10
019300080516     d PrmOutQLib                    10
019400080516     d PrmEsito                       1
019500080516
019600080516      // - Controllo validità numero fax
019700080516     d TRUL42R         pr                  extpgm('TRUL42R')
019800080516     d TRUL42ds                            likeds(trul42ds)
019900080516
020000080516      // -  Reperimento USRDFNDTA x comunicazione e-mail
020100080516     d TRUL44R         pr                  extpgm('TRUL44R')
020200080516     d TRUL44ds                            likeds(trul44ds)
020300080516     d TRTCM1ds                            likeds(trtcm1ds)
020400080117      //---------------------------------------------------------------
020500080117      //?Definizione key-list.
020600080117      //---------------------------------------------------------------
020700080118
020800080118      // - File FNDCT01L
020900080117     d k03dct01      e ds                  extname(FNDCT01L:*key)
021000080118     d                                     inz
021100080118      // - File FNDCS01L
021200080515     d k07dcs01      e ds                  extname(FNDCS01L:*key)
021300080515     d                                     inz
021400080118      // - File FNDET01L
021500080118     d k02det01      e ds                  extname(FNDET01L:*key)
021600080118     d                                     inz
021700080125      // - File FNBLP01L / FNARB01L
021800080125     d k04blparb     e ds                  extname(FNBLP01L:*key)
021900080125     d                                     prefix(ARB:3)
022000080125     d                                     inz
022100080117
022200080117      //---------------------------------------------------------------
022300080117      //?Riepilogo indicatori.
022400080117      //---------------------------------------------------------------
022500080117      //---------------------------------------------------------------
022600080118
022700080118      //---------------------------------------------------------------
022800080118      //?M A I N - L I N E
022900080118      //---------------------------------------------------------------
023000080117
023100080117     c     *Entry        plist
023200080117     c                   parm                    KPJBA
023300080117
023400080117      /free
023500080117
023600080117       // Operazioni iniziali
023700080117       exsr RoutInz;
023800080117
023900080513       // Preparazione della lettera o e-mail da inviare
024000080118       if  O00err = *blank;
024100080118         exsr sr_PrintMail;
024200080117       endif;
024300080117
024400080117       // Operazioni finali
024500080117       exsr RoutEnd;
024600080117
024700080117       //--------------------------------------------------------------
024800080117       //?Operazioni iniziali.
024900080117       //--------------------------------------------------------------
025000080117       BEGSR RoutInz;
025100080118
025200080118       // Ricezione parametri
025300080118         FIDN00ds = kpjbu;
025400080118         clear o00f12;
025500080118         clear o00f03;
025600080118         clear o00err;
025700080118         clear o00msg;
025800080118
025900080118       // Reperimento dati job
026000080118         exsr DatiJob;
026100080118
026200080118       // Reperimento tab. "MRA"
026300080118         exsr rep_TabMRA;
026400080118         if  o00err = 'E';
026500080118           leavesr;
026600080118         endif;
026700080117
026800080117       // Verifica parametri ricecvuti
026900080118         if  i00aac = *zero   or
027000080118             i00fil = *zero   or
027100080118             i00nca = *zero;
027200080118           o00err = 'E';
027300080118           o00msg = 'Manca il numero C.A.!!!';
027400080117           leavesr;
027500080117         endif;
027600080117
027700080117       // Reperimento dati testata C.A.
027800080118         exsr rep_FNDCT;
027900080118         if  o00err = 'E';
028000080117           leavesr;
028100080117         endif;
028200080118
028300080118       // Decodifica filiale gestione
028400080118         chain (i00fgs) AZORG;
028500080118         if  NOT  %found(AZORG01L);
028600080118           o00err = 'E';
028700080118           o00msg = 'Filiale gestione errata';
028800080118           leavesr;
028900080118         endif;
029000080118
029100080117       // Reperimento dati spedizione
029200080118         exsr rep_FNBLPARB;
029300080117
029400080514       // reperimento dati del beneficiario
029500080514         exsr rep_Benefic ;
029600080514
029700080117       ENDSR;
029800080118
029900080118       //--------------------------------------------------------------
030000080118       //?Reperimento Dati del job (Utente/Operativi).
030100080118       //--------------------------------------------------------------
030200080118       BEGSR DatiJob;
030300080118
030400080118         in(E) §AzUte;
030500080118         if NOT %error;
030600080118           in(E) §DatiUte;
030700080118         endif;
030800080118         if %error or RSut = *blanks;
030900080118           clear TIBS34ds;
031000080118           tibs34r(tibs34ds);
031100080118           in §AzUte;
031200080118           in §DatiUte;
031300080118         endif;
031400080118
031500080118       ENDSR;
031600080118
031700080118       //--------------------------------------------------------------
031800080118       //?Reperimento dati testata C.A.
031900080118       //--------------------------------------------------------------
032000080118       BEGSR rep_FNDCT;
032100080118
032200080118         DCTaac = i00aac;
032300080118         DCTfil = i00fil;
032400080118         DCTnca = i00nca;
032500080118
032600080118         chain %kds(k03dct01 : 3) FNDCT000;
032700080118
032800080118         if  NOT  %found(FNDCT01L)   or
032900080118             DCTatb <> *blank;
033000080118           o00err = 'E';
033100080118           o00msg = 'C.A. non reperita';
033200080118           leavesr;
033300080514         else;
033400080514           ddct01 = dctflo ;
033500080118         endif;
033600080118
033700080118       // Impostazione numero spedizione tra i parms SE non ricevuto
033800080118         if  i00aas = *zero   or
033900080118             i00lnp = *zero   or
034000080118             i00nsp = *zero;
034100080118           i00aas = DCTaas;
034200080118           i00lnp = DCTlnp;
034300080118           i00nrs = DCTnrs;
034400080118           i00nsp = DCTnsp;
034500080118         endif;
034600080513
034700080118       ENDSR;
034800080118
034900080513       //--------------------------------------------------------------
035000080513       //?Reperimento dati del Beneficiario
035100080513       //--------------------------------------------------------------
035200080513       BEGSR rep_Benefic ;
035300080513
035400080514             $English = *off ;
035500080516             wds_ksc =%editc(Dctksc:'X');
035600090108             clear fidnbeds ;
035700090108             IBEmod = 'B';
035800090108             IBEaac = DCTaac ;
035900090108             IBEfil = DCTfil ;
036000090108             IBEnca = DCTnca ;
036100090108             IBEaac = DCTfil ;
036200090108             IBEaas = DCTaas ;
036300090108             IBElnp = DCTlnp ;
036400090108             IBEnrs = DCTnrs ;
036500090108             IBEnsp = DCTnsp ;
036600090108             IBEksc = DCTksc ;
036700090108             IBEptr = DCTptr ;
036800090108             IBEflo = DCTflo ;
036900090108             IBEtpb = I00tpb ;
037000090108             if %subst(knsif:7:1) = 'P' ;
037100090108                IBEsif = 'P'    ;
037200090108             endif ;
037300090108             callp FIDNBER (fidnbeds);
037400090108
037500090108             If OBErag <> *blanks;
037600080515       // valorizzo i campi del file printer
037700090108               PICrag = OBErag;
037800090108               PICind = OBEvia;
037900090108               PICcap = OBEcap;
038000090108               PICloc = OBEloc;
038100090108               PICpro = OBEprv;
038200090108               PICnaz = OBEnaz;
038300090108               PIXfax = OBEfax;
038400090108               wds_ksc =%editc(OBEksc:'X');
038500090401               kcli =OBEksc;
038600090108               $invio = OBEinv ;
038700090108               if OBEing  <> *blanks;
038800080514               $english = *on;
038900080514               endif;
039000090108               W_mail = obeima ;
039100090108               KSC_mail = obekma ;
039200090108             endif ;
039300080515
039400080514       endsr;
039500080118       //--------------------------------------------------------------
039600080118       //?Reperimento dati spedizione
039700080118       //--------------------------------------------------------------
039800080118       BEGSR rep_FNBLPARB;
039900080118
040000080118         IF  i00tpb = 'P';
040100080118
040200080118           ARBaas = i00aas;
040300080118           ARBlnp = i00lnp;
040400080118           ARBnrs = i00nrs;
040500080118           ARBnsp = i00nsp;
040600080118           chain %kds(k04blparb : 4) FNBLP000;
040700080118
040800080118           if  NOT  %found(FNBLP01L)   or
040900080118               ARBatb <> *blank;
041000080118             o00err = 'E';
041100080118             o00msg = 'Spedizione non reperita';
041200080118             leavesr;
041300080118           endif;
041400080118
041500080118         ELSE;
041600080118
041700080118           chain %kds(k04blparb : 4) FNARB000;
041800080118
041900080118           if  NOT  %found(FNARB01L)   or
042000080118               ARBatb <> *blank;
042100080118             o00err = 'E';
042200080118             o00msg = 'Spedizione non reperita';
042300080118             leavesr;
042400080118           endif;
042500080118
042600080515         endif;
042700080118
042800080515       endsr;
042900080125
043000080118       //--------------------------------------------------------------
043100080118       //?Reperimento tab. MRA = Bart-Maling - Danni
043200080118       //--------------------------------------------------------------
043300080118       BEGSR rep_TabMRA;
043400080118
043500080118         clear dMRAdan;
043600080118         clear TIBS02ds;
043700080118
043800080118         T02mod = 'C';
043900080118         T02sif = knsif;
044000080118         T02cod = 'MRA';
044100080513         T02ke1 = %trimr(SDSpgm);
044200080118
044300080118         tibs02r( kpjba : tibs02ds );
044400080118
044500080118         if T02err = *blanks;
044600080118           dMRAdan = T02uni;
044700080118         else;
044800080118           o00err = 'E';
044900080118           o00msg = 'Manca tab. MRA'
045000080513                  + ' "' + %trimr(T02ke1) + '" ';
045100080118           leavesr;
045200080118         endif;
045300080118
045400080118       ENDSR;
045500080118
045600080118       //--------------------------------------------------------------
045700080118       //?Reperimento tab. TAD = Tipo Anomalia
045800080118       //--------------------------------------------------------------
045900080118       BEGSR rep_TabTAD;
046000080118
046100080118         clear dTAD;
046200080118         clear TIBS02ds;
046300080118
046400080118         T02mod = 'C';
046500080118         T02sif = knsif;
046600080118         T02cod = 'TAD';
046700080118         T02ke1 = Wtad;
046800080118
046900080118         tibs02r( kpjba : tibs02ds );
047000080118
047100080118         if T02err = *blanks;
047200080118           dTAD = T02uni;
047300080118         endif;
047400080118
047500080118       ENDSR;
047600080117
047700080117       //--------------------------------------------------------------
047800080118       //?Stampa della lettera da inviare via e-mail.
047900080117       //--------------------------------------------------------------
048000080118       BEGSR sr_PrintMail;
048100080118
048200080515       // Aggancio tabella TLD = Parametri stampa Testo Lettera Danni
048300080515         exsr Tab_TLD ;
048400080118       // Override al file di stampa ed apertura dello stesso
048500080118         exsr sr_Override;
048600080117
048700080117       // Impostazione dei campi in stampa
048800080515         exsr sr_Stampa ;
048900080117
049000080117       // Impostazione indicatori utilizzati nel PrtF
049100080515         *in41  = (PIXfax <> *blanks);
049200080515         *in42  = (POCda2 <> *blanks);
049300080515         *in43  = (POCqtd <> *blanks);
049400080515         *in46  = (POCdp1 <> *blanks);
049500080117         *in47  = (ARBrma <> *blanks);
049600080515         *in48  = (DCTnev  > *zeros);
049700080117
049800080117       // Stampa
049900080515
050000080515         select ;
050100080515
050200080515       // Lettera in Italiano
050300080515         WHEN  $E_Mail   = *off and  $English  = *off ;
050400080515       // - Intestazione + Oggetto
050500080515               write  DN67T01;
050600080515       // - Testo
050700080515               exsr   sr_PrtTEXT;
050800080515       // - Note (eventuali)
050900080515               exsr   sr_PrtNOTE ;
051000080515       // - Piede
051100080515               exsr   sr_PrtTEXT2 ;
051200080515               write     DN67P01 ;
051300080516       // - Note finali
051400080515       //   ("Per ricevere le prossime comunicazioni via e-mail...")
051500080516                if   PFCem1 <> *blanks ;
051600080515                write     DN67N01 ;
051700080516                endif ;
051800080515
051900080515       // Lettera in Inglese - - - - - - - - - - - - - - - - - - - - - -*
052000080515         WHEN  $E_Mail   = *off and  $English  = *on ;
052100080515
052200080515       // - Intestazione + Oggetto
052300080515               write  DN67T02;
052400080515       // - Testo
052500080515               exsr   sr_PrtTEXT;
052600080515       // - Note (eventuali)
052700080515               exsr   sr_PrtNOTE ;
052800080515       // - Piede
052900080515               exsr   sr_PrtTEXT2 ;
053000080515               write     DN67P02 ;
053100080515       // - Note finali
053200080515       //   ("Per ricevere le prossime comunicazioni via e-mail...")
053300080516                if   PFCem1 <> *blanks ;
053400080515                write     DN67N02 ;
053500080516                endif ;
053600080515
053700080515       // e-mail in Italiano - - - - - - - - - - - - - - - - - - - - - -*
053800080515         WHEN  $E_Mail  = *on  and  $English  = *off;
053900080515
054000080515       // - Intestazione + Oggetto
054100080515               write  DN67T01M;
054200080515       // - Testo
054300080515               exsr   sr_PrtTEXT;
054400080515       // - Note (eventuali)
054500080515               exsr   sr_PrtNOTE ;
054600080515       // - Piede
054700080515               exsr   sr_PrtTEXT2 ;
054800080515               write     DN67P01M ;
054900080515       //  e-mail in Inglese  - - - - - - - - - - - - - - - - - - - - - -*
055000080515         WHEN  $E_Mail  = *on  and  $English  = *on ;
055100080515
055200080515       // - Intestazione + Oggetto
055300080515               write  DN67T02M;
055400080515       // - Testo
055500080515               exsr   sr_PrtTEXT;
055600080515       // - Note (eventuali)
055700080515               exsr   sr_PrtNOTE ;
055800080515       // - Piede
055900080515               exsr   sr_PrtTEXT2 ;
056000080515               write     DN67P02M ;
056100080515
056200080515         Endsl;
056300080515
056400080515       // Chiusura file stampa & Cancellazione override
056500080515         exsr sr_DeleteOvr;
056600080515
056700080515       ENDSR;
056800080515       //--------------------------------------------------------------
056900080515       //?Reperimento parametri di stampa
057000080515       //--------------------------------------------------------------
057100080515       BEGSR Tab_TLD;
057200080515
057300080515         clear dTLD;
057400080515         clear TIBS02ds;
057500080515
057600080515         T02mod = 'C';
057700080515         T02sif = knsif;
057800080515         T02cod = 'TLD';
057900080515         T02ke1 = 'AT1';
058000080515
058100080515         tibs02r( kpjba : tibs02ds );
058200080515
058300080515         if T02err = *blanks;
058400080515           dTLD = T02uni;
058500080515         endif;
058600080515       ENDSR ;
058700080117       //--------------------------------------------------------------
058800080117       //?Esecuzione override ed apertura del file di stampa.
058900080117       //--------------------------------------------------------------
059000080117       BEGSR sr_Override;
059100080117
059200080514       // se invio con il fax
059300080514
059400080515         If    PIXfax  <> *blanks  ;
059500080514
059600080515        //  Controllo N° FAX
059700080514               clear  TRUL42ds;
059800080514               D42fax  = PIXfax;
059900080514               callp  TRUL42R (Trul42ds);
060000080514         If    D42err <> *blanks;
060100080514
060200080514               clear  PIXfax;
060300080514
060400080514         endif ;
060500080514
060600080515        //  Cerco il nome della coda del fax
060700080514               PrmFil = Dutpou;
060800080514               PrmTpOutQ = '2';
060900080514               clear PrmOutQ ;
061000080514               clear PrmOutQLib;
061100080514               clear PrmEsito;
061200080514
061300080514               callp TRULOUTQ (PrmFine:Prmfil:PrmTpOutq:
061400080516               PrmOutQ:PrmOutQLib:PrmEsito);
061500080514
061600080514          if  PrmEsito = *off and  PrmOutq <> *blanks ;
061700080514                w§tldstp = Prmoutq ;
061800080514          else;
061900080514                w§TLDstp = '*SAME     ';
062000080514          endif;
062100080514
062200080514          endif;
062300080514
062400080515         // Imposto le ovveride
062500080514
062600080514         select ;
062700080514
062800080514         when     $invio = 'M' and W_mail <> *blanks ;
062900080514                  clear trul44ds ;
063000080514                  D44pgm    = SDSpgm;
063100080514                  D44po     = ORGfil;
063200080514                  D44Ksc    = Ksc_Mail;
063300080514                  D44Des    = W_Mail;
063400080514                  D44scope  = '*CALLLVL';
063500080514                  D44prtf   = 'FIDN67P';
063600080514
063700080514         If       $English  = *on ;
063800080514                  D44obj    = '*OBJM*'
063900080514                              + 'Damage comunication N. '
064000080514                              + %char(DCTfil)
064100080514                              + '/'
064200080514                              + %trim(%editc( DCTnca:'Z' ))
064300080514                              + ' year '
064400080514                              + %char(DCTaac);
064500080514         Else;
064600080514                  D44obj    = '*OBJM*'
064700080514                              + 'Comunicazione di presunta +
064800080514                                 anomalia N.'
064900080514                              + %char(DCTfil)
065000080514                              + '/'
065100080514                              + %trim( %editc( DCTnca:'Z' ))
065200080514                                             + ' anno '
065300080514                              + %char(DCTaac);
065400080514         Endif;
065500080514                  callp TRUL44R (Trul44ds:trtcm1ds);
065600080515        // - se c'è stato qualche problema faccio invio in altro modo
065700080515         if        D44err  <> *blanks;
065800080514                   clear    $Invio;
065900080515         else;
066000080514                   $E_Mail  = *on;
066100080515                   Qcmd     = $Cmd(1);
066200080515                   %subst(Qcmd:23:10) = D44outq;
066300080515                   Qcmd     = %trim(Qcmd) + D44dta + ''')' ;
066400080515           callp(e) qCmdExc(Qcmd : %size(Qcmd));
066500080515           if  not %error;
066600080515             $Invio = 'M';
066700080515           else ;
066800080515             $invio = ' ' ;
066900080515             $e_mail = *off;
067000080515           endif;
067100080515         endif;
067200080515
067300080515         when §tldtmod <> *blanks and w§tldstp <> *blanks ;
067400080515             Qcmd     = $Cmd(2);
067500080515             %subst(Qcmd:39:10) = w§TLDstp;
067600080515             %subst(Qcmd:60:10) = §TLDtmod;
067700080515
067800080515         other ;
067900080515             Qcmd     = $Cmd(3) ;
068000080515
068100080515         endsl ;
068200080515
068300080515       //  Se ho impostato qualcosa eseguo l'OVRPRTF
068400080515         if  Qcmd <> *blanks and  $Invio    =  *blanks ;
068500080515           callp(e) qCmdExc(Qcmd : %size(Qcmd));
068600080515         endif;
068700080515
068800080515       //  Apro file di stampa
068900080515         If  $E_Mail = *off ;
069000080515             open      FIDN67P;
069100080515         else;
069200080515             open      FIDN67PM;
069300080515         endif;
069400080514
069500080117       ENDSR;
069600080117
069700080117       //--------------------------------------------------------------
069800080117       //?Impostazione dei campi in stampa
069900080117       //--------------------------------------------------------------
070000080515       BEGSR sr_Stampa ;
070100080117
070200080515       //  Determina tipo di invio:
070300080515         select;
070400080515
070500080515       //  M = e-mail
070600080515         when      $E_Mail   = *on;
070700080515                   $Invio    = 'M';
070800080515       //  F = fax
070900080515         when      $E_Mail   = *off and PIXfax  <> *blanks;
071000080515                   $Invio    = 'F';
071100080515       //  L = lettera
071200080515         other ;
071300080515                   $Invio    = 'L';
071400080515         endsl ;
071500080515
071600080515         exsr     sr_INTES ;        // intestazione
071700080515         exsr     sr_OGGE;          // oggetto
071800080515         exsr     sr_NOTE;          // note
071900080515         exsr     sr_FIRMA;         // firma
072000080117
072100080117       ENDSR;
072200080117
072300080515       //--------------------------------------------------------------
072400080515       //?Impostazione dati intestazione
072500080515       //--------------------------------------------------------------
072600080515       Begsr   sr_INTES;
072700080515
072800080515       // Se P.O. che scrive è ESTERO forzo "Uff. Internazionale" come
072900080515       //   descrizione intestazione + dati da AZORG del terminal di
073000080515       //   partenza della linea di partenza della bolla.
073100080515         if orgfl1 = 'E' and orgfva = *blanks ;
073200080515
073300080515             PICpod = I_PICpod ;
073400080515
073500080515             reset FNLV55ds;
073600080515             D55lin  = ORGfil ;
073700080515             reset WLBdat;
073800080515             G02dat = *date ;
073900080515             xsrda8(wlbdat);
074000080515             d55drf = G02inv;
074100080515             callp FNLV55R (fnlv55ds) ;
074200080515           if D55err = *blanks;
074300080515             chain (d55tfp) AZORG;
074400080515             PICpoi  =  ORGind ;
074500080515             PICpoc  =  ORGcpf ;
074600080515             PICpol  =  ORGloc ;
074700080515             PICpop  =  ORGpro ;
074800080515             PICpot  =  ORGtel ;
074900080515             PICpof  =  ORGfax ;
075000080515           endif;
075100080515
075200080515         else;
075300080515
075400080515             PICpod  =  ORGdes ;
075500080515             PICpoi  =  ORGind ;
075600080515             PICpoc  =  ORGcpf ;
075700080515             PICpol  =  ORGloc ;
075800080515             PICpop  =  ORGpro ;
075900080515             PICpot  =  ORGtel ;
076000080515             PICpof  =  ORGfax ;
076100080515         endif ;
076200080515
076300080515       // Recupero l'interlocutore non per la lettera reso avaria
076400080515        exsr  Rep_PerCon  ;
076500080515
076600080515       // Località + data
076700080515        if  ORGfl1 = 'E' and  ORGfva = *blanks ;
076800080515            PICdal   = 'MILANO, ' + %editc(*date:'Y');
076900080515        else;
077000080515            PICdal   = %trim(ORGdes) + ', ' + %editc(*date:'Y') ;
077100080515        endif;
077200080515
077300080515       endsr;
077400080515       //--------------------------------------------------------------
077500080515       //?Reperimento Nominativo dell'interlocutore                    ?*
077600080515       //--------------------------------------------------------------
077700080515       begsr  Rep_percon ;
077800080515
077900080515            DCStpd = 'C';
078000080515            DCSnkt = Wnumca ;
078100080515            DCSstd = 'N';
078200080515
078300080515            chain %kds(k07dcs01 : 3) FNDCS000;
078400080515
078500080515            if  %found(FNDCS01L)  and DCSnot <> *blanks;
078600080515                PICpec =  'C.A. '  +  DCSnot ;
078700080515            endif;
078800080515
078900080515       endsr;
079000080515       //--------------------------------------------------------------
079100080515       //?Impostazioni dati dell'oggetto                               ?*
079200080515       //--------------------------------------------------------------
079300080515       begsr  sr_ogge ;
079400080515
079500080515           POCnca  = %trim( %editw( I00fil:'0   ' ))
079600080515                     + '/'  + %trim( %editc( I00nca:'Z' )) ;
079700080515           POCaac  = I00aac ;
079800080515
079900080515           POClnp  = I00lnp ;
080000080515           POCnrs  = I00nrs ;
080100080515           POCnsp  = I00nsp ;
080200080515           reset WLBdat;
080300080515           G02inv = (I00aas * 10000) + ARBmgs ;
080400080515           G02err  = '3' ;
080500080515           xsrda8(wlbdat);
080600080515           POCdat = G02dat;
080700080515           POCrsm = ARBrsm;
080800080515           POCrmn = ARBrmn;
080900080515           POCrma = ARBrma;
081000080515           POCrsd = ARBrsd;
081100080515           POClod = ARBlod;
081200080515           if  ARBnzd <> *blanks ;
081300080515               POCprn =  ARBnzd ;
081400080515           else ;
081500080515               POCprn =  ARBprd ;
081600080515           endif;
081700080515           POClod = ARBlod;
081800080515           POCncl = ARBncl;
081900080515           POCpkf = ARBpkf;
082000080515
082100080515        //  Evento
082200080515           IF  DCTnev  > *zeros;
082300080515               *in48 = *on;
082400080515               POCkev  = %trim( %editw( DCTaae:'0    ' ))
082500080515                         + '/'  + %trim( %editc( DCTnev:'Z' ));
082600080515           detaae = dctaae ;
082700080515           detnev = dctnev ;
082800080515
082900080515            chain %kds(k02det01 : 2) FNDET000;
083000080515
083100080515           Wtad   = DETtad ;
083200080515           exsr  rep_TabTAD;
083300080515           select;
083400080515           when    $English =  *on and  §TADdesi <> *blanks;
083500080515                  POCtae =  §TADdesi ;
083600080515           when    §TADdest <> *blanks;
083700080515                  POCtae =  §TADdest ;
083800080515           other;
083900080515                  POCtae =  §TADdesc ;
084000080515           endsl;
084100080515           endif;
084200080515
084300080515        //  Decodifico tipo anomalia
084400080515
084500080515           Wtad   = I00tad ;
084600080515           exsr  rep_TabTAD;
084700080515
084800080515           select ;
084900080515           when  $English =  *on and  §TADdesi <> *blanks ;
085000080515                  POCtad =  §TADdesi ;
085100080515           when    §TADdest <> *blanks;
085200080515                  POCtad =  §TADdest ;
085300080515           other;
085400080515                  POCtad =  §TADdesc ;
085500080515           endsl;
085600080515
085700080515        //  Descrizione anomalia
085800080515
085900080515          exsr    Rep_DesTAD ;
086000080515
086100080515        //  Quantità danneggiate
086200080515        //  - Peso
086300080515         if  DCTpkd > *zero;
086400080515           POCqtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
086500080515         endif;
086600080515        // - Colli
086700080515         if  DCTncn > *zero;
086800080515         if  $English = *off ;
086900080515           POCqtd = %trim(POCqtd)
087000080515                  + ' Colli ' + %trim( %editc( DCTncn : 'Z' ));
087100080515         else ;
087200080515           POCqtd = %trim(POCqtd)
087300080515                  + ' Parcels ' + %trim( %editc( DCTncn : 'Z' ));
087400080515         endif;
087500080515         endif;
087600080515        // - Pezzi
087700080515        //   + Descrizione pezzi danneggiati
087800080515         if  DCTnpz > *zero;
087900080515         if  $English = *off ;
088000080515           POCqtd  = %trim(POCqtd)
088100080515                   + ' Pezzi ' + %trim( %editc( DCTnpz : 'Z' ));
088200080515         else ;
088300080515           POCqtd  = %trim(POCqtd)
088400080515                   + ' Pieces ' + %trim( %editc( DCTnpz : 'Z' ));
088500080515         endif;
088600080515           exsr Rep_DesPZD;
088700080515         endif;
088800080515
088900080515       endsr;
089000080515
089100080515       //--------------------------------------------------------------
089200080515       //?Impostazioni note
089300080515       //--------------------------------------------------------------
089400080515       begsr  sr_note ;
089500080515
089600080515         clear  XX;
089700080515         clear  YY;
089800080515         clear  $TAD;
089900080515
090000080515         DCStpd = 'T';
090100080515         DCSnkt = Wnumca;
090200080515         DCSstd = 'D';
090300080515         clear  DCShim ;
090400080515         clear  DCSdim ;
090500080515         DCSnks = 'AT1';
090600080515
090700080515         setll %kds(k07dcs01 : 7) FNDCS000;
090800080515         reade %kds(k07dcs01 : 7) FNDCS000;
090900080515
091000080515         dow  NOT  %eof(FNDCS01L);
091100080515
091200080515           yy += 1;
091300080515           $TAD(yy) = DCSnot;
091400080515
091500080515       // Sposto in $Note ogni 3 records letti
091600080515         if   yy  = %elem($TAD);
091700080515           xx +=1 ;
091800080515           $note(xx) = $tad(1)+$tad(2)+ $tad(3);
091900080515           clear yy ;
092000080515           clear $TAD ;
092100080515           if  xx >= %elem($Note) ;
092200080515           leavesr;
092300080515           endif;
092400080515         endif;
092500080515           reade %kds(k07dcs01 : 7) FNDCS000;
092600080515         enddo;
092700080515
092800080515       // Memorizzo in $Note le note residue
092900080515         if  yy > *zeros;
093000080515           xx +=1 ;
093100080515           $note(xx) = $tad(1)+$tad(2)+ $tad(3);
093200080515         endif  ;
093300080515
093400080515       Endsr;
093500080515
093600080515       //--------------------------------------------------------------
093700080515       //?Impostazioni firma
093800080515       //--------------------------------------------------------------
093900080515       begsr  sr_firma;
094000080515
094100080515
094200080515        //  Se P.O. che scrive è ESTERO forzo "Uff. Internazionale"
094300080515           PFCpod  = PICpod ;
094400080515
094500080515        //  Se non ho inviato via e-mail
094600080515        //  => reperisco l'indirizzo mail per le note finali
094700080515        //     ("Per ricevere le prossime comunicazioni via e-mail...")
094800080515          If $E_Mail  = *off ;
094900090401             wds_ksc =%editc(Kcli:'X');
095000080515        //  - Recupero l'indirizzo del mittente
095100080515                  clear trul44ds ;
095200080515                  D44pgm    = SDSpgm;
095300080515                  D44po     = w0030;
095400080515                  callp TRUL44R (Trul44ds:trtcm1ds);
095500080515                  if   w0040 <> 8888 and  w0040 <> 9999 ;
095600080515                       PFCem1 = D44mit;
095700080515                       PFCcod = Kcli ;
095800080515                  Endif ;
095900080515          Endif;
096000080515
096100080515       Endsr;
096200080515
096300080515       //--------------------------------------------------------------
096400080515       //?Stampa testo lettera
096500080515       //--------------------------------------------------------------
096600080515       begsr  sr_PrtTEXT;
096700080515
096800080515          Select ;
096900130410        // - C.A. chiusa lettera solo italiano
097000130410          when  DCTcch <> *blanks and  $E_Mail   = *off;
097100130410                write     DN67AT11C;
097200130410        // - C.A. chiusa e-mail solo italiano
097300130410          when  DCTcch <> *blanks and  $E_Mail   = *on;
097400130410                write     DN67AT11CM;
097500080515        // - lettera in lingua italiana
097600080515          when  $English  = *off  and  $E_Mail   = *off ;
097700080515                write     DN67AT11A ;
097800080515        // - e-mail in lingua italiana
097900080515          when  $English  = *off  and  $E_Mail   = *on ;
098000080515                write     DN67AT11AM ;
098100080515        // - lettera in lingua inglese
098200080515          when  $English  = *on   and  $E_Mail   = *off ;
098300080515                write     DN67AT12A ;
098400080515        // - e-mail in lingua inglese
098500080515          when  $English  = *on   and  $E_Mail   = *on ;
098600080515                write     DN67AT12AM;
098700080515          endsl ;
098800080515       Endsr;
098900080515
099000080515       //--------------------------------------------------------------
099100080515       //?Stampa parte finale testo lettera
099200080515       //--------------------------------------------------------------
099300080515       begsr  sr_PrtTEXT2;
099400080515
099500080515          Select ;
099600080515        // - lettera in lingua italiana
099700080515          when  $English  = *off  and  $E_Mail   = *off ;
099800080515                write     DN67AT11B ;
099900080515        // - e-mail in lingua italiana
100000080515          when  $English  = *off  and  $E_Mail   = *on ;
100100080515                write     DN67AT11BM ;
100200080515        // - lettera in lingua inglese
100300080515          when  $English  = *on   and  $E_Mail   = *off ;
100400080515                write     DN67AT12B ;
100500080515        // - e-mail in lingua inglese
100600080515          when  $English  = *on   and  $E_Mail   = *on ;
100700080515                write     DN67AT12BM;
100800080515          endsl ;
100900080515       Endsr;
101000080515
101100080515       //--------------------------------------------------------------
101200080515       //?Stampa delle note eventuali
101300080515       //--------------------------------------------------------------
101400080515       begsr  sr_Prtnote ;
101500080515
101600080515         clear  yy;
101700080515
101800080515         DOW  yy <  %elem($Note);
101900080515
102000080515            yy +=1;
102100080515            if  $Note(yy) =  *blanks ;
102200080515                leave ;
102300080515            endif;
102400080515       //  lascia una riga (vuota) prima delle note
102500080515            if  yy = 1 ;
102600080515                clear PDCnot ;
102700080515                if  $E_Mail = *off;
102800080515                    write  DN67N00 ;
102900080515                else;
103000080515                    write  DN67N00M ;
103100080515                endif;
103200080515            endif;
103300080515
103400080515            PDCnot =  $Note(yy);
103500080515            if  $E_Mail   = *off ;
103600080515                write   DN67N00 ;
103700080515            else ;
103800080515                write   DN67N00M ;
103900080515            endif ;
104000080515
104100080515         Enddo ;
104200080515
104300080515       Endsr ;
104400080515
104500080117       //--------------------------------------------------------------
104600080117       //?Chiusura file stampa & Cancellazione override
104700080117       //--------------------------------------------------------------
104800080117       BEGSR sr_DeleteOvr;
104900080117
105000080515         if  %open(FIDN67P);
105100080515           close FIDN67P;
105200080117           callp(e) qCmdExc(C_CmdDltOvr : %size(C_CmdDltOvr));
105300080117         endif;
105400080515
105500080515         if  %open(FIDN67PM);
105600080515           close FIDN67PM;
105700080515           callp(e) qCmdExc(C_CmdDltOvrM : %size(C_CmdDltOvrM));
105800080515         endif;
105900080117
106000080117       ENDSR;
106100080118
106200080118       //--------------------------------------------------------------
106300080118       //?Reperimento descrizione anomalia
106400080118       //--------------------------------------------------------------
106500080118       BEGSR rep_DesTAD;
106600080118
106700080118         clear YY;
106800080118         clear $TAD;
106900080118
107000080118         DCStpd = 'C';
107100080118         DCSnkt = Wnumca;
107200080118         DCSstd = 'A';
107300080118
107400080515         setll %kds(k07dcs01 : 3) FNDCS000;
107500080515         reade %kds(k07dcs01 : 3) FNDCS000;
107600080118
107700080121         dow  NOT  %eof(FNDCS01L)   and
107800080118              YY < %elem($TAD);
107900080118           yy += 1;
108000080118           $TAD(yy) = DCSnot;
108100080515           reade %kds(k07dcs01 : 3) FNDCS000;
108200080121         enddo;
108300080118
108400080515         POCda1 = $TAD(1);
108500080515         POCda2 = $TAD(2) + $TAD(3);
108600080118
108700080118       ENDSR;
108800080118
108900080118       //--------------------------------------------------------------
109000080118       //?Reperimento descrizione pezzi danneggiati/mancanti
109100080118       //--------------------------------------------------------------
109200080118       BEGSR rep_DesPZD;
109300080118
109400080118         clear YY;
109500080118         clear $TAD;
109600080118
109700080118         DCStpd = 'C';
109800080118         DCSnkt = Wnumca;
109900080118         DCSstd = 'P';
110000080118
110100080515         setll %kds(k07dcs01 : 3) FNDCS000;
110200080515         reade %kds(k07dcs01 : 3) FNDCS000;
110300080118
110400080121         dow  NOT  %eof(FNDCS01L)   and
110500080118              YY < %elem($TAD);
110600080118           yy += 1;
110700080118           $TAD(yy) = DCSnot;
110800080515           reade %kds(k07dcs01 : 3) FNDCS000;
110900080121         enddo;
111000080118
111100080515         POCdpz = $TAD(1);
111200080516         POCdp1 = $TAD(2) + $TAD(3);
111300080118
111400080118       ENDSR;
111500080117
111600080117       //--------------------------------------------------------------
111700080117       //?Operazioni finali.
111800080117       //--------------------------------------------------------------
111900080117       BEGSR RoutEnd;
112000080117
112100080118         kpjbu = FIDN00ds;
112200080516
112300080516         clear TIBS02ds;
112400080516         T02tla = 'C';
112500080516         tibs02r( kpjba : tibs02ds );
112600080516
112700080516         PrmFine = 'L';
112800080516         clear PrmFil ;
112900080516         clear PrmTpOutQ ;
113000080516         clear PrmOutQ ;
113100080516         clear PrmOutQlib ;
113200080516         clear PrmEsito ;
113300080516         callp TRULOUTQ (PrmFine:Prmfil:PrmTpOutq:
113400080516         PrmOutQ:PrmOutQLib:PrmEsito);
113500080516
113600080516         *INLR = *ON ;
113700080117         return;
113800080117
113900080117       ENDSR;
114000080117
114100080117      /end-free
114200080515**  $Cmd
114300080515OVRPRTF FIDN67PM OUTQ(          ) USRDFNDTA('                                         1
114400080516OVRPRTF FIDN67P SPLFNAME(LETCOM) OUTQ(xxxxxxxxxx) FORMTYPE(xxxxxxxxxx)                2
114500080515OVRPRTF FIDN67P                                                        SAVE(*YES)     3
