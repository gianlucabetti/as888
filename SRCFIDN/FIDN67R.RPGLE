000100080117      //---------------------------------------------------------------
000200080513      //?INVIO LETTERA PREAVVISO DANNO LT AL CLIENTE
000300080117      //---------------------------------------------------------------
000400080117      // Richiamato da FIDN03R x le C.A. con le segg. caratteristiche:
000500080117      // 1) Fase 100
000600080513      // 2) Liquidazione transattiva
000700130410      // 3) Fase 900 Liquidazione transattiva Avaria
000800130410      //    per clienti abilitati da tabella DKL
000900080117      //---------------------------------------------------------------
001000080117
001100080117     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001200080117
001300080117      //---------------------------------------------------------------
001400080117      //?Dichiarazione file.
001500080117      //---------------------------------------------------------------
001600080118
001700080117     fFNDCT01L  if   e           k disk
001800080117     fFNDET01L  if   e           k disk
001900080117     fFNDCS01L  if   e           k disk
002000080117
002100080117     fFNARB01L  if   e           k disk
002200080117     fFNBLP01L  if   e           k disk    prefix(ARB:3)
002300080117
002400080117     fAZORG01L  if   e           k disk
002500080117
002600080513     fFIDN67P   o    e             printer usropn
002700080117     f                                     OFlind(*in01)
002800080513     fFIDN67PM  o    e             printer usropn
002900080515     f                                     OFlind(*in02)
003000080117
003100080117      //---------------------------------------------------------------
003200080117      //?Definizione costanti.
003300080117      //---------------------------------------------------------------
003400080117
003500080117
003600080515     d I_picpod        c                   const('UFF. INTERNAZIONALE')
003700080515
003800080117      // - Comandi di override al PrtF
003900080117     d C_CmdDltOvr     c                   const('DLTOVR +
004000080515     d                                            file(FIDN67P)')
004100080117
004200080513     d C_CmdDltOvrM    c                   const('DLTOVR +
004300080515     d                                            file(FIDN67PM)')
004400080513
004500080117      //---------------------------------------------------------------
004600080117      //?Definizione schiere.
004700080117      //---------------------------------------------------------------
004800080118
004900080117      // - Descrizione Tipo Anomalia
005000080117     d $TAD            s             35    dim( 3) inz                          descr. tipo anomalia
005100080117
005200080117      // - Note da stampare
005300080117     d $Note           s            105    dim( 5) inz                          $TAD x 5
005400080117
005500080515      // - Comandi x OVRPRTF
005600080515     d $Cmd            s             85    dim( 3) ctdata perrcd(1)
005700080117      //---------------------------------------------------------------
005800080117      //?Definizione aree dati.
005900080117      //---------------------------------------------------------------
006000080118
006100080117      // - Dati utente
006200080117     d §AzUte        e ds                  extname(AZUTE00F)
006300080117     d                                     dtaara
006400080117     d §DatiUte      e ds                  extname(dDatiUte)
006500080117     d                                     dtaara
006600080117
006700080117      //---------------------------------------------------------------
006800080117      //?Definizione strutture dati.
006900080117      //---------------------------------------------------------------
007000080118
007100080117      // - Status
007200080117     d Psds           sds
007300080117     d   SDSpgm          *proc
007400080117
007500080117      // - Parametri ricevuti
007600080117     d KPJBA         e ds
007700080117     d FIDN00ds      e ds
007800080117     d   Wnumca                6     19
007900080117
008000080117      // - Reperimento dati utente
008100080117     d TIBS34ds      e ds
008200080515
008300080117      // - Flag operativi in FNDCT
008400080117     d dDCT01        e ds                  inz
008500080117
008600080117      // - Ricerca/Controllo tabelle
008700080117     d TIBS02ds      e ds                  inz
008800080117      // - tab. MRA/DAN = Bart-Maling - Danni
008900080117     d dMRAdan       e ds
009000080117      // - tab. TAD = Tipi anomalia danni
009100080117     d dTAD          e ds                  inz
009200080515      // - tab. TLD = Testi lettera danni
009300080515     d dTLD          e ds                  inz
009400080117
009500080515      // - Parametri x Reperimento terminal di arrivo/partenza
009600080515     d FNLV55ds      e ds                  inz
009700080515     d   D55tla      e                     inz('L')
009800080515     d   D55tpt      e                     inz('P')
009900080514      // - Parametri x Controllo validità numero fax
010000080514     d TRUL42ds      e ds                  inz
010100080514      // - Parametri x Reperimento USRDFNDTA x comunicazione e-mail
010200080514     d TRUL44ds      e ds                  inz
010300080514      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
010400080514     d TRTCM1ds      e ds                  inz                                  X Invio e-mail
010500090108
010600090108      // - Reperimento dati anagrafici beneficiario
010700090108     d FIDNBEds      e ds
010800080514
010900080117      // - Parametri x Controllo & Inversione date
011000080117     d WLBdat          ds                  inz
011100080117     d  G02dat                 1      8  0 inz
011200080117     d  G02inv                 9     16  0 inz
011300080117     d  G02err                17     17    inz('3')
011400080117     d  G02tgi                18     22  0 inz
011500080513
011600080513      * Ridefinizione sottoconto
011700080516     d wDS_ksc         ds                  inz
011800080513     d  w0030                         3  0 inz
011900080513     d  w0040                         4  0 inz
012000080117
012100080117      //---------------------------------------------------------------
012200080117      //?Definizione variabili globali.
012300080117      //---------------------------------------------------------------
012400080118
012500080117      // - Flags booleani
012600080117     d W_Esito         s               n   inz(*off)
012700080514      //- Campi di comodo x invio e-mail
012800080516     d $Invio          s              1    inz
012900080514     d W_mail          s             60    inz
013000080514     d KSC_mail        s                   inz like(D44ksc)
013100080514     d $English        s              1    inz(*off)
013200080514     d $E_Mail         s              1    inz(*off)
013300080117
013400080117      // - Indici di schiera
013500080118     d YY              s              3  0 inz
013600080516     d XX              s              3  0 inz
013700080117
013800080117
013900080117      // - Campi di comodo
014000080118     d Wtad            s                   like(DCTtad)   inz
014100090108     d Kcli            s                   like(DCTksc)   inz
014200080516     d w§DCTport       s                   like(§DCTport) inz
014300080515     d w§TLDstp        s                   like(§TLDstp1) inz
014400080514
014500080514      // - Parametri x TRULOUTQ
014600080514     d PrmFine         s              1    inz
014700080514     d PrmFil          s              3s 0 inz
014800080514     d PrmTpOutQ       s              1    inz
014900080514     d PrmOutQ         s             10    inz
015000080514     d PrmOutQLib      s             10    inz
015100080514     d PrmEsito        s              1    inz
015200080117
015300080516      // - Campi per QCMDEXC
015400080516     d Qcmd            s            512    inz
015500080516
015600080117      //---------------------------------------------------------------
015700080117      //?Definizione procedure usate.
015800080117      //---------------------------------------------------------------
015900080118
016000080117      // - Reperimento dati utente
016100080117     d tibs34r         pr                  extpgm('TIBS34R')
016200080117     d  tibs34ds                           likeds(TIBS34ds)
016300080117
016400080516      // - Reperimento terminal di arrivo/partenza
016500080516     d fnlv55r         pr                  extpgm('FNLV55R')
016600080516     d  fnlv55ds                           likeds(FNLV55ds)
016700090108
016800090108      // - Reperimento beneficiario
016900090108     d fidnber         pr                  extpgm('FIDNBER')
017000090108     d  fidnbeds                           likeds(FIDNBEDS)
017100080516
017200080117      // - Gestione tabelle
017300080117     d tibs02r         pr                  extpgm('TIBS02R')
017400080117     d  kpjba                              likeds(kpjba)    const
017500080117     d  tibs02ds                           likeds(TIBS02ds)
017600080117
017700080117      // - Controllo & Inversione date
017800080117     d xsrda8          pr                  extpgm('XSRDA8')
017900080117     d  wlbdat                             likeds(WLBdat)
018000080117
018100080117      // - Esecuzione comando di sistema
018200080117     d qCmdExc         pr                  extpgm('QCMDEXC')
018300080117     d  Qcmd                        512    const  options(*varsize)
018400080117     d  Qlen                         15  5 const
018500080117
018600080516      // - Controllo stampanti
018700080516     d TRULOUTQ        pr                  extpgm('TRULOUTQ')
018800080516     d PrmFine                        1
018900080516     d PrmFil                         3s 0
019000080516     d PrmTpOutQ                      1
019100080516     d PrmOutQ                       10
019200080516     d PrmOutQLib                    10
019300080516     d PrmEsito                       1
019400080516
019500080516      // - Controllo validità numero fax
019600080516     d TRUL42R         pr                  extpgm('TRUL42R')
019700080516     d TRUL42ds                            likeds(trul42ds)
019800080516
019900080516      // -  Reperimento USRDFNDTA x comunicazione e-mail
020000080516     d TRUL44R         pr                  extpgm('TRUL44R')
020100080516     d TRUL44ds                            likeds(trul44ds)
020200080516     d TRTCM1ds                            likeds(trtcm1ds)
020300080117      //---------------------------------------------------------------
020400080117      //?Definizione key-list.
020500080117      //---------------------------------------------------------------
020600080118
020700080118      // - File FNDCT01L
020800080117     d k03dct01      e ds                  extname(FNDCT01L:*key)
020900080118     d                                     inz
021000080118      // - File FNDCS01L
021100080515     d k07dcs01      e ds                  extname(FNDCS01L:*key)
021200080515     d                                     inz
021300080118      // - File FNDET01L
021400080118     d k02det01      e ds                  extname(FNDET01L:*key)
021500080118     d                                     inz
021600080125      // - File FNBLP01L / FNARB01L
021700080125     d k04blparb     e ds                  extname(FNBLP01L:*key)
021800080125     d                                     prefix(ARB:3)
021900080125     d                                     inz
022000080117
022100080117      //---------------------------------------------------------------
022200080117      //?Riepilogo indicatori.
022300080117      //---------------------------------------------------------------
022400080117      //---------------------------------------------------------------
022500080118
022600080118      //---------------------------------------------------------------
022700080118      //?M A I N - L I N E
022800080118      //---------------------------------------------------------------
022900080117
023000080117     c     *Entry        plist
023100080117     c                   parm                    KPJBA
023200080117
023300080117      /free
023400080117
023500080117       // Operazioni iniziali
023600080117       exsr RoutInz;
023700080117
023800080513       // Preparazione della lettera o e-mail da inviare
023900080118       if  O00err = *blank;
024000080118         exsr sr_PrintMail;
024100080117       endif;
024200080117
024300080117       // Operazioni finali
024400080117       exsr RoutEnd;
024500080117
024600080117       //--------------------------------------------------------------
024700080117       //?Operazioni iniziali.
024800080117       //--------------------------------------------------------------
024900080117       BEGSR RoutInz;
025000080118
025100080118       // Ricezione parametri
025200080118         FIDN00ds = kpjbu;
025300080118         clear o00f12;
025400080118         clear o00f03;
025500080118         clear o00err;
025600080118         clear o00msg;
025700080118
025800080118       // Reperimento dati job
025900080118         exsr DatiJob;
026000080118
026100080118       // Reperimento tab. "MRA"
026200080118         exsr rep_TabMRA;
026300080118         if  o00err = 'E';
026400080118           leavesr;
026500080118         endif;
026600080117
026700080117       // Verifica parametri ricecvuti
026800080118         if  i00aac = *zero   or
026900080118             i00fil = *zero   or
027000080118             i00nca = *zero;
027100080118           o00err = 'E';
027200080118           o00msg = 'Manca il numero C.A.!!!';
027300080117           leavesr;
027400080117         endif;
027500080117
027600080117       // Reperimento dati testata C.A.
027700080118         exsr rep_FNDCT;
027800080118         if  o00err = 'E';
027900080117           leavesr;
028000080117         endif;
028100080118
028200080118       // Decodifica filiale gestione
028300080118         chain (i00fgs) AZORG;
028400080118         if  NOT  %found(AZORG01L);
028500080118           o00err = 'E';
028600080118           o00msg = 'Filiale gestione errata';
028700080118           leavesr;
028800080118         endif;
028900080118
029000080117       // Reperimento dati spedizione
029100080118         exsr rep_FNBLPARB;
029200080117
029300080514       // reperimento dati del beneficiario
029400080514         exsr rep_Benefic ;
029500080514
029600080117       ENDSR;
029700080118
029800080118       //--------------------------------------------------------------
029900080118       //?Reperimento Dati del job (Utente/Operativi).
030000080118       //--------------------------------------------------------------
030100080118       BEGSR DatiJob;
030200080118
030300080118         in(E) §AzUte;
030400080118         if NOT %error;
030500080118           in(E) §DatiUte;
030600080118         endif;
030700080118         if %error or RSut = *blanks;
030800080118           clear TIBS34ds;
030900080118           tibs34r(tibs34ds);
031000080118           in §AzUte;
031100080118           in §DatiUte;
031200080118         endif;
031300080118
031400080118       ENDSR;
031500080118
031600080118       //--------------------------------------------------------------
031700080118       //?Reperimento dati testata C.A.
031800080118       //--------------------------------------------------------------
031900080118       BEGSR rep_FNDCT;
032000080118
032100080118         DCTaac = i00aac;
032200080118         DCTfil = i00fil;
032300080118         DCTnca = i00nca;
032400080118
032500080118         chain %kds(k03dct01 : 3) FNDCT000;
032600080118
032700080118         if  NOT  %found(FNDCT01L)   or
032800080118             DCTatb <> *blank;
032900080118           o00err = 'E';
033000080118           o00msg = 'C.A. non reperita';
033100080118           leavesr;
033200080514         else;
033300080514           ddct01 = dctflo ;
033400080118         endif;
033500080118
033600080118       // Impostazione numero spedizione tra i parms SE non ricevuto
033700080118         if  i00aas = *zero   or
033800080118             i00lnp = *zero   or
033900080118             i00nsp = *zero;
034000080118           i00aas = DCTaas;
034100080118           i00lnp = DCTlnp;
034200080118           i00nrs = DCTnrs;
034300080118           i00nsp = DCTnsp;
034400080118         endif;
034500080513
034600080118       ENDSR;
034700080118
034800080513       //--------------------------------------------------------------
034900080513       //?Reperimento dati del Beneficiario
035000080513       //--------------------------------------------------------------
035100080513       BEGSR rep_Benefic ;
035200080513
035300080514             $English = *off ;
035400080516             wds_ksc =%editc(Dctksc:'X');
035500090108             clear fidnbeds ;
035600090108             IBEmod = 'B';
035700090108             IBEaac = DCTaac ;
035800090108             IBEfil = DCTfil ;
035900090108             IBEnca = DCTnca ;
036000090108             IBEaac = DCTfil ;
036100090108             IBEaas = DCTaas ;
036200090108             IBElnp = DCTlnp ;
036300090108             IBEnrs = DCTnrs ;
036400090108             IBEnsp = DCTnsp ;
036500090108             IBEksc = DCTksc ;
036600090108             IBEptr = DCTptr ;
036700090108             IBEflo = DCTflo ;
036800090108             IBEtpb = I00tpb ;
036900090108             if %subst(knsif:7:1) = 'P' ;
037000090108                IBEsif = 'P'    ;
037100090108             endif ;
037200090108             callp FIDNBER (fidnbeds);
037300090108
037400090108             If OBErag <> *blanks;
037500080515       // valorizzo i campi del file printer
037600090108               PICrag = OBErag;
037700090108               PICind = OBEvia;
037800090108               PICcap = OBEcap;
037900090108               PICloc = OBEloc;
038000090108               PICpro = OBEprv;
038100090108               PICnaz = OBEnaz;
038200090108               PIXfax = OBEfax;
038300090108               wds_ksc =%editc(OBEksc:'X');
038400090401               kcli =OBEksc;
038500090108               $invio = OBEinv ;
038600090108               if OBEing  <> *blanks;
038700080514               $english = *on;
038800080514               endif;
038900090108               W_mail = obeima ;
039000090108               KSC_mail = obekma ;
039100090108             endif ;
039200080515
039300080514       endsr;
039400080118       //--------------------------------------------------------------
039500080118       //?Reperimento dati spedizione
039600080118       //--------------------------------------------------------------
039700080118       BEGSR rep_FNBLPARB;
039800080118
039900080118         IF  i00tpb = 'P';
040000080118
040100080118           ARBaas = i00aas;
040200080118           ARBlnp = i00lnp;
040300080118           ARBnrs = i00nrs;
040400080118           ARBnsp = i00nsp;
040500080118           chain %kds(k04blparb : 4) FNBLP000;
040600080118
040700080118           if  NOT  %found(FNBLP01L)   or
040800080118               ARBatb <> *blank;
040900080118             o00err = 'E';
041000080118             o00msg = 'Spedizione non reperita';
041100080118             leavesr;
041200080118           endif;
041300080118
041400080118         ELSE;
041500080118
041600080118           chain %kds(k04blparb : 4) FNARB000;
041700080118
041800080118           if  NOT  %found(FNARB01L)   or
041900080118               ARBatb <> *blank;
042000080118             o00err = 'E';
042100080118             o00msg = 'Spedizione non reperita';
042200080118             leavesr;
042300080118           endif;
042400080118
042500080515         endif;
042600080118
042700080515       endsr;
042800080125
042900080118       //--------------------------------------------------------------
043000080118       //?Reperimento tab. MRA = Bart-Maling - Danni
043100080118       //--------------------------------------------------------------
043200080118       BEGSR rep_TabMRA;
043300080118
043400080118         clear dMRAdan;
043500080118         clear TIBS02ds;
043600080118
043700080118         T02mod = 'C';
043800080118         T02sif = knsif;
043900080118         T02cod = 'MRA';
044000080513         T02ke1 = %trimr(SDSpgm);
044100080118
044200080118         tibs02r( kpjba : tibs02ds );
044300080118
044400080118         if T02err = *blanks;
044500080118           dMRAdan = T02uni;
044600080118         else;
044700080118           o00err = 'E';
044800080118           o00msg = 'Manca tab. MRA'
044900080513                  + ' "' + %trimr(T02ke1) + '" ';
045000080118           leavesr;
045100080118         endif;
045200080118
045300080118       ENDSR;
045400080118
045500080118       //--------------------------------------------------------------
045600080118       //?Reperimento tab. TAD = Tipo Anomalia
045700080118       //--------------------------------------------------------------
045800080118       BEGSR rep_TabTAD;
045900080118
046000080118         clear dTAD;
046100080118         clear TIBS02ds;
046200080118
046300080118         T02mod = 'C';
046400080118         T02sif = knsif;
046500080118         T02cod = 'TAD';
046600080118         T02ke1 = Wtad;
046700080118
046800080118         tibs02r( kpjba : tibs02ds );
046900080118
047000080118         if T02err = *blanks;
047100080118           dTAD = T02uni;
047200080118         endif;
047300080118
047400080118       ENDSR;
047500080117
047600080117       //--------------------------------------------------------------
047700080118       //?Stampa della lettera da inviare via e-mail.
047800080117       //--------------------------------------------------------------
047900080118       BEGSR sr_PrintMail;
048000080118
048100080515       // Aggancio tabella TLD = Parametri stampa Testo Lettera Danni
048200080515         exsr Tab_TLD ;
048300080118       // Override al file di stampa ed apertura dello stesso
048400080118         exsr sr_Override;
048500080117
048600080117       // Impostazione dei campi in stampa
048700080515         exsr sr_Stampa ;
048800080117
048900080117       // Impostazione indicatori utilizzati nel PrtF
049000080515         *in41  = (PIXfax <> *blanks);
049100080515         *in42  = (POCda2 <> *blanks);
049200080515         *in43  = (POCqtd <> *blanks);
049300080515         *in46  = (POCdp1 <> *blanks);
049400080117         *in47  = (ARBrma <> *blanks);
049500080515         *in48  = (DCTnev  > *zeros);
049600080117
049700080117       // Stampa
049800080515
049900080515         select ;
050000080515
050100080515       // Lettera in Italiano
050200080515         WHEN  $E_Mail   = *off and  $English  = *off ;
050300080515       // - Intestazione + Oggetto
050400080515               write  DN67T01;
050500080515       // - Testo
050600080515               exsr   sr_PrtTEXT;
050700080515       // - Note (eventuali)
050800080515               exsr   sr_PrtNOTE ;
050900080515       // - Piede
051000080515               exsr   sr_PrtTEXT2 ;
051100080515               write     DN67P01 ;
051200080516       // - Note finali
051300080515       //   ("Per ricevere le prossime comunicazioni via e-mail...")
051400080516                if   PFCem1 <> *blanks ;
051500080515                write     DN67N01 ;
051600080516                endif ;
051700080515
051800080515       // Lettera in Inglese - - - - - - - - - - - - - - - - - - - - - -*
051900080515         WHEN  $E_Mail   = *off and  $English  = *on ;
052000080515
052100080515       // - Intestazione + Oggetto
052200080515               write  DN67T02;
052300080515       // - Testo
052400080515               exsr   sr_PrtTEXT;
052500080515       // - Note (eventuali)
052600080515               exsr   sr_PrtNOTE ;
052700080515       // - Piede
052800080515               exsr   sr_PrtTEXT2 ;
052900080515               write     DN67P02 ;
053000080515       // - Note finali
053100080515       //   ("Per ricevere le prossime comunicazioni via e-mail...")
053200080516                if   PFCem1 <> *blanks ;
053300080515                write     DN67N02 ;
053400080516                endif ;
053500080515
053600080515       // e-mail in Italiano - - - - - - - - - - - - - - - - - - - - - -*
053700080515         WHEN  $E_Mail  = *on  and  $English  = *off;
053800080515
053900080515       // - Intestazione + Oggetto
054000080515               write  DN67T01M;
054100080515       // - Testo
054200080515               exsr   sr_PrtTEXT;
054300080515       // - Note (eventuali)
054400080515               exsr   sr_PrtNOTE ;
054500080515       // - Piede
054600080515               exsr   sr_PrtTEXT2 ;
054700080515               write     DN67P01M ;
054800080515       //  e-mail in Inglese  - - - - - - - - - - - - - - - - - - - - - -*
054900080515         WHEN  $E_Mail  = *on  and  $English  = *on ;
055000080515
055100080515       // - Intestazione + Oggetto
055200080515               write  DN67T02M;
055300080515       // - Testo
055400080515               exsr   sr_PrtTEXT;
055500080515       // - Note (eventuali)
055600080515               exsr   sr_PrtNOTE ;
055700080515       // - Piede
055800080515               exsr   sr_PrtTEXT2 ;
055900080515               write     DN67P02M ;
056000080515
056100080515         Endsl;
056200080515
056300080515       // Chiusura file stampa & Cancellazione override
056400080515         exsr sr_DeleteOvr;
056500080515
056600080515       ENDSR;
056700080515       //--------------------------------------------------------------
056800080515       //?Reperimento parametri di stampa
056900080515       //--------------------------------------------------------------
057000080515       BEGSR Tab_TLD;
057100080515
057200080515         clear dTLD;
057300080515         clear TIBS02ds;
057400080515
057500080515         T02mod = 'C';
057600080515         T02sif = knsif;
057700080515         T02cod = 'TLD';
057800080515         T02ke1 = 'AT1';
057900080515
058000080515         tibs02r( kpjba : tibs02ds );
058100080515
058200080515         if T02err = *blanks;
058300080515           dTLD = T02uni;
058400080515         endif;
058500080515       ENDSR ;
058600080117       //--------------------------------------------------------------
058700080117       //?Esecuzione override ed apertura del file di stampa.
058800080117       //--------------------------------------------------------------
058900080117       BEGSR sr_Override;
059000080117
059100080514       // se invio con il fax
059200080514
059300080515         If    PIXfax  <> *blanks  ;
059400080514
059500080515        //  Controllo N° FAX
059600080514               clear  TRUL42ds;
059700080514               D42fax  = PIXfax;
059800080514               callp  TRUL42R (Trul42ds);
059900080514         If    D42err <> *blanks;
060000080514
060100080514               clear  PIXfax;
060200080514
060300080514         endif ;
060400080514
060500080515        //  Cerco il nome della coda del fax
060600080514               PrmFil = Dutpou;
060700080514               PrmTpOutQ = '2';
060800080514               clear PrmOutQ ;
060900080514               clear PrmOutQLib;
061000080514               clear PrmEsito;
061100080514
061200080514               callp TRULOUTQ (PrmFine:Prmfil:PrmTpOutq:
061300080516               PrmOutQ:PrmOutQLib:PrmEsito);
061400080514
061500080514          if  PrmEsito = *off and  PrmOutq <> *blanks ;
061600080514                w§tldstp = Prmoutq ;
061700080514          else;
061800080514                w§TLDstp = '*SAME     ';
061900080514          endif;
062000080514
062100080514          endif;
062200080514
062300080515         // Imposto le ovveride
062400080514
062500080514         select ;
062600080514
062700080514         when     $invio = 'M' and W_mail <> *blanks ;
062800080514                  clear trul44ds ;
062900080514                  D44pgm    = SDSpgm;
063000080514                  D44po     = ORGfil;
063100080514                  D44Ksc    = Ksc_Mail;
063200080514                  D44Des    = W_Mail;
063300080514                  D44scope  = '*CALLLVL';
063400080514                  D44prtf   = 'FIDN67P';
063500080514
063600080514         If       $English  = *on ;
063700080514                  D44obj    = '*OBJM*'
063800080514                              + 'Damage comunication N. '
063900080514                              + %char(DCTfil)
064000080514                              + '/'
064100080514                              + %trim(%editc( DCTnca:'Z' ))
064200080514                              + ' year '
064300080514                              + %char(DCTaac);
064400080514         Else;
064500080514                  D44obj    = '*OBJM*'
064600080514                              + 'Comunicazione di presunta +
064700080514                                 anomalia N.'
064800080514                              + %char(DCTfil)
064900080514                              + '/'
065000080514                              + %trim( %editc( DCTnca:'Z' ))
065100080514                                             + ' anno '
065200080514                              + %char(DCTaac);
065300080514         Endif;
065400080514                  callp TRUL44R (Trul44ds:trtcm1ds);
065500080515        // - se c'è stato qualche problema faccio invio in altro modo
065600080515         if        D44err  <> *blanks;
065700080514                   clear    $Invio;
065800080515         else;
065900080514                   $E_Mail  = *on;
066000080515                   Qcmd     = $Cmd(1);
066100080515                   %subst(Qcmd:23:10) = D44outq;
066200080515                   Qcmd     = %trim(Qcmd) + D44dta + ''')' ;
066300080515           callp(e) qCmdExc(Qcmd : %size(Qcmd));
066400080515           if  not %error;
066500080515             $Invio = 'M';
066600080515           else ;
066700080515             $invio = ' ' ;
066800080515             $e_mail = *off;
066900080515           endif;
067000080515         endif;
067100080515
067200080515         when §tldtmod <> *blanks and w§tldstp <> *blanks ;
067300080515             Qcmd     = $Cmd(2);
067400080515             %subst(Qcmd:39:10) = w§TLDstp;
067500080515             %subst(Qcmd:60:10) = §TLDtmod;
067600080515
067700080515         other ;
067800080515             Qcmd     = $Cmd(3) ;
067900080515
068000080515         endsl ;
068100080515
068200080515       //  Se ho impostato qualcosa eseguo l'OVRPRTF
068300080515         if  Qcmd <> *blanks and  $Invio    =  *blanks ;
068400080515           callp(e) qCmdExc(Qcmd : %size(Qcmd));
068500080515         endif;
068600080515
068700080515       //  Apro file di stampa
068800080515         If  $E_Mail = *off ;
068900080515             open      FIDN67P;
069000080515         else;
069100080515             open      FIDN67PM;
069200080515         endif;
069300080514
069400080117       ENDSR;
069500080117
069600080117       //--------------------------------------------------------------
069700080117       //?Impostazione dei campi in stampa
069800080117       //--------------------------------------------------------------
069900080515       BEGSR sr_Stampa ;
070000080117
070100080515       //  Determina tipo di invio:
070200080515         select;
070300080515
070400080515       //  M = e-mail
070500080515         when      $E_Mail   = *on;
070600080515                   $Invio    = 'M';
070700080515       //  F = fax
070800080515         when      $E_Mail   = *off and PIXfax  <> *blanks;
070900080515                   $Invio    = 'F';
071000080515       //  L = lettera
071100080515         other ;
071200080515                   $Invio    = 'L';
071300080515         endsl ;
071400080515
071500080515         exsr     sr_INTES ;        // intestazione
071600080515         exsr     sr_OGGE;          // oggetto
071700080515         exsr     sr_NOTE;          // note
071800080515         exsr     sr_FIRMA;         // firma
071900080117
072000080117       ENDSR;
072100080117
072200080515       //--------------------------------------------------------------
072300080515       //?Impostazione dati intestazione
072400080515       //--------------------------------------------------------------
072500080515       Begsr   sr_INTES;
072600080515
072700080515       // Se P.O. che scrive è ESTERO forzo "Uff. Internazionale" come
072800080515       //   descrizione intestazione + dati da AZORG del terminal di
072900080515       //   partenza della linea di partenza della bolla.
073000080515         if orgfl1 = 'E' and orgfva = *blanks ;
073100080515
073200080515             PICpod = I_PICpod ;
073300080515
073400080515             reset FNLV55ds;
073500080515             D55lin  = ORGfil ;
073600080515             reset WLBdat;
073700080515             G02dat = *date ;
073800080515             xsrda8(wlbdat);
073900080515             d55drf = G02inv;
074000080515             callp FNLV55R (fnlv55ds) ;
074100080515           if D55err = *blanks;
074200080515             chain (d55tfp) AZORG;
074300080515             PICpoi  =  ORGind ;
074400080515             PICpoc  =  ORGcpf ;
074500080515             PICpol  =  ORGloc ;
074600080515             PICpop  =  ORGpro ;
074700080515             PICpot  =  ORGtel ;
074800080515             PICpof  =  ORGfax ;
074900080515           endif;
075000080515
075100080515         else;
075200080515
075300080515             PICpod  =  ORGdes ;
075400080515             PICpoi  =  ORGind ;
075500080515             PICpoc  =  ORGcpf ;
075600080515             PICpol  =  ORGloc ;
075700080515             PICpop  =  ORGpro ;
075800080515             PICpot  =  ORGtel ;
075900080515             PICpof  =  ORGfax ;
076000080515         endif ;
076100080515
076200080515       // Recupero l'interlocutore non per la lettera reso avaria
076300080515        exsr  Rep_PerCon  ;
076400080515
076500080515       // Località + data
076600080515        if  ORGfl1 = 'E' and  ORGfva = *blanks ;
076700080515            PICdal   = 'MILANO, ' + %editc(*date:'Y');
076800080515        else;
076900080515            PICdal   = %trim(ORGdes) + ', ' + %editc(*date:'Y') ;
077000080515        endif;
077100080515
077200080515       endsr;
077300080515       //--------------------------------------------------------------
077400080515       //?Reperimento Nominativo dell'interlocutore                    ?*
077500080515       //--------------------------------------------------------------
077600080515       begsr  Rep_percon ;
077700080515
077800080515            DCStpd = 'C';
077900080515            DCSnkt = Wnumca ;
078000080515            DCSstd = 'N';
078100080515
078200080515            chain %kds(k07dcs01 : 3) FNDCS000;
078300080515
078400080515            if  %found(FNDCS01L)  and DCSnot <> *blanks;
078500080515                PICpec =  'C.A. '  +  DCSnot ;
078600080515            endif;
078700080515
078800080515       endsr;
078900080515       //--------------------------------------------------------------
079000080515       //?Impostazioni dati dell'oggetto                               ?*
079100080515       //--------------------------------------------------------------
079200080515       begsr  sr_ogge ;
079300080515
079400080515           POCnca  = %trim( %editw( I00fil:'0   ' ))
079500080515                     + '/'  + %trim( %editc( I00nca:'Z' )) ;
079600080515           POCaac  = I00aac ;
079700080515
079800080515           POClnp  = I00lnp ;
079900080515           POCnrs  = I00nrs ;
080000080515           POCnsp  = I00nsp ;
080100080515           reset WLBdat;
080200080515           G02inv = (I00aas * 10000) + ARBmgs ;
080300080515           G02err  = '3' ;
080400080515           xsrda8(wlbdat);
080500080515           POCdat = G02dat;
080600080515           POCrsm = ARBrsm;
080700080515           POCrmn = ARBrmn;
080800080515           POCrma = ARBrma;
080900080515           POCrsd = ARBrsd;
081000080515           POClod = ARBlod;
081100080515           if  ARBnzd <> *blanks ;
081200080515               POCprn =  ARBnzd ;
081300080515           else ;
081400080515               POCprn =  ARBprd ;
081500080515           endif;
081600080515           POClod = ARBlod;
081700080515           POCncl = ARBncl;
081800080515           POCpkf = ARBpkf;
081900080515
082000080515        //  Evento
082100080515           IF  DCTnev  > *zeros;
082200080515               *in48 = *on;
082300080515               POCkev  = %trim( %editw( DCTaae:'0    ' ))
082400080515                         + '/'  + %trim( %editc( DCTnev:'Z' ));
082500080515           detaae = dctaae ;
082600080515           detnev = dctnev ;
082700080515
082800080515            chain %kds(k02det01 : 2) FNDET000;
082900080515
083000080515           Wtad   = DETtad ;
083100080515           exsr  rep_TabTAD;
083200080515           select;
083300080515           when    $English =  *on and  §TADdesi <> *blanks;
083400080515                  POCtae =  §TADdesi ;
083500080515           when    §TADdest <> *blanks;
083600080515                  POCtae =  §TADdest ;
083700080515           other;
083800080515                  POCtae =  §TADdesc ;
083900080515           endsl;
084000080515           endif;
084100080515
084200080515        //  Decodifico tipo anomalia
084300080515
084400080515           Wtad   = I00tad ;
084500080515           exsr  rep_TabTAD;
084600080515
084700080515           select ;
084800080515           when  $English =  *on and  §TADdesi <> *blanks ;
084900080515                  POCtad =  §TADdesi ;
085000080515           when    §TADdest <> *blanks;
085100080515                  POCtad =  §TADdest ;
085200080515           other;
085300080515                  POCtad =  §TADdesc ;
085400080515           endsl;
085500080515
085600080515        //  Descrizione anomalia
085700080515
085800080515          exsr    Rep_DesTAD ;
085900080515
086000080515        //  Quantità danneggiate
086100080515        //  - Peso
086200080515         if  DCTpkd > *zero;
086300080515           POCqtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
086400080515         endif;
086500080515        // - Colli
086600080515         if  DCTncn > *zero;
086700080515         if  $English = *off ;
086800080515           POCqtd = %trim(POCqtd)
086900080515                  + ' Colli ' + %trim( %editc( DCTncn : 'Z' ));
087000080515         else ;
087100080515           POCqtd = %trim(POCqtd)
087200080515                  + ' Parcels ' + %trim( %editc( DCTncn : 'Z' ));
087300080515         endif;
087400080515         endif;
087500080515        // - Pezzi
087600080515        //   + Descrizione pezzi danneggiati
087700080515         if  DCTnpz > *zero;
087800080515         if  $English = *off ;
087900080515           POCqtd  = %trim(POCqtd)
088000080515                   + ' Pezzi ' + %trim( %editc( DCTnpz : 'Z' ));
088100080515         else ;
088200080515           POCqtd  = %trim(POCqtd)
088300080515                   + ' Pieces ' + %trim( %editc( DCTnpz : 'Z' ));
088400080515         endif;
088500080515           exsr Rep_DesPZD;
088600080515         endif;
088700080515
088800080515       endsr;
088900080515
089000080515       //--------------------------------------------------------------
089100080515       //?Impostazioni note
089200080515       //--------------------------------------------------------------
089300080515       begsr  sr_note ;
089400080515
089500080515         clear  XX;
089600080515         clear  YY;
089700080515         clear  $TAD;
089800080515
089900080515         DCStpd = 'T';
090000080515         DCSnkt = Wnumca;
090100080515         DCSstd = 'D';
090200080515         clear  DCShim ;
090300080515         clear  DCSdim ;
090400080515         DCSnks = 'AT1';
090500080515
090600080515         setll %kds(k07dcs01 : 7) FNDCS000;
090700080515         reade %kds(k07dcs01 : 7) FNDCS000;
090800080515
090900080515         dow  NOT  %eof(FNDCS01L);
091000080515
091100080515           yy += 1;
091200080515           $TAD(yy) = DCSnot;
091300080515
091400080515       // Sposto in $Note ogni 3 records letti
091500080515         if   yy  = %elem($TAD);
091600080515           xx +=1 ;
091700080515           $note(xx) = $tad(1)+$tad(2)+ $tad(3);
091800080515           clear yy ;
091900080515           clear $TAD ;
092000080515           if  xx >= %elem($Note) ;
092100080515           leavesr;
092200080515           endif;
092300080515         endif;
092400080515           reade %kds(k07dcs01 : 7) FNDCS000;
092500080515         enddo;
092600080515
092700080515       // Memorizzo in $Note le note residue
092800080515         if  yy > *zeros;
092900080515           xx +=1 ;
093000080515           $note(xx) = $tad(1)+$tad(2)+ $tad(3);
093100080515         endif  ;
093200080515
093300080515       Endsr;
093400080515
093500080515       //--------------------------------------------------------------
093600080515       //?Impostazioni firma
093700080515       //--------------------------------------------------------------
093800080515       begsr  sr_firma;
093900080515
094000080515
094100080515        //  Se P.O. che scrive è ESTERO forzo "Uff. Internazionale"
094200080515           PFCpod  = PICpod ;
094300080515
094400080515        //  Se non ho inviato via e-mail
094500080515        //  => reperisco l'indirizzo mail per le note finali
094600080515        //     ("Per ricevere le prossime comunicazioni via e-mail...")
094700080515          If $E_Mail  = *off ;
094800090401             wds_ksc =%editc(Kcli:'X');
094900080515        //  - Recupero l'indirizzo del mittente
095000080515                  clear trul44ds ;
095100080515                  D44pgm    = SDSpgm;
095200080515                  D44po     = w0030;
095300080515                  callp TRUL44R (Trul44ds:trtcm1ds);
095400080515                  if   w0040 <> 8888 and  w0040 <> 9999 ;
095500080515                       PFCem1 = D44mit;
095600080515                       PFCcod = Kcli ;
095700080515                  Endif ;
095800080515          Endif;
095900080515
096000080515       Endsr;
096100080515
096200080515       //--------------------------------------------------------------
096300080515       //?Stampa testo lettera
096400080515       //--------------------------------------------------------------
096500080515       begsr  sr_PrtTEXT;
096600080515
096700080515          Select ;
096800130410        // - C.A. chiusa lettera solo italiano
096900130410          when  DCTcch <> *blanks and  $E_Mail   = *off;
097000130410                write     DN67AT11C;
097100130410        // - C.A. chiusa e-mail solo italiano
097200130410          when  DCTcch <> *blanks and  $E_Mail   = *on;
097300130410                write     DN67AT11CM;
097400080515        // - lettera in lingua italiana
097500080515          when  $English  = *off  and  $E_Mail   = *off ;
097600080515                write     DN67AT11A ;
097700080515        // - e-mail in lingua italiana
097800080515          when  $English  = *off  and  $E_Mail   = *on ;
097900080515                write     DN67AT11AM ;
098000080515        // - lettera in lingua inglese
098100080515          when  $English  = *on   and  $E_Mail   = *off ;
098200080515                write     DN67AT12A ;
098300080515        // - e-mail in lingua inglese
098400080515          when  $English  = *on   and  $E_Mail   = *on ;
098500080515                write     DN67AT12AM;
098600080515          endsl ;
098700080515       Endsr;
098800080515
098900080515       //--------------------------------------------------------------
099000080515       //?Stampa parte finale testo lettera
099100080515       //--------------------------------------------------------------
099200080515       begsr  sr_PrtTEXT2;
099300080515
099400080515          Select ;
099500080515        // - lettera in lingua italiana
099600080515          when  $English  = *off  and  $E_Mail   = *off ;
099700080515                write     DN67AT11B ;
099800080515        // - e-mail in lingua italiana
099900080515          when  $English  = *off  and  $E_Mail   = *on ;
100000080515                write     DN67AT11BM ;
100100080515        // - lettera in lingua inglese
100200080515          when  $English  = *on   and  $E_Mail   = *off ;
100300080515                write     DN67AT12B ;
100400080515        // - e-mail in lingua inglese
100500080515          when  $English  = *on   and  $E_Mail   = *on ;
100600080515                write     DN67AT12BM;
100700080515          endsl ;
100800080515       Endsr;
100900080515
101000080515       //--------------------------------------------------------------
101100080515       //?Stampa delle note eventuali
101200080515       //--------------------------------------------------------------
101300080515       begsr  sr_Prtnote ;
101400080515
101500080515         clear  yy;
101600080515
101700080515         DOW  yy <  %elem($Note);
101800080515
101900080515            yy +=1;
102000080515            if  $Note(yy) =  *blanks ;
102100080515                leave ;
102200080515            endif;
102300080515       //  lascia una riga (vuota) prima delle note
102400080515            if  yy = 1 ;
102500080515                clear PDCnot ;
102600080515                if  $E_Mail = *off;
102700080515                    write  DN67N00 ;
102800080515                else;
102900080515                    write  DN67N00M ;
103000080515                endif;
103100080515            endif;
103200080515
103300080515            PDCnot =  $Note(yy);
103400080515            if  $E_Mail   = *off ;
103500080515                write   DN67N00 ;
103600080515            else ;
103700080515                write   DN67N00M ;
103800080515            endif ;
103900080515
104000080515         Enddo ;
104100080515
104200080515       Endsr ;
104300080515
104400080117       //--------------------------------------------------------------
104500080117       //?Chiusura file stampa & Cancellazione override
104600080117       //--------------------------------------------------------------
104700080117       BEGSR sr_DeleteOvr;
104800080117
104900080515         if  %open(FIDN67P);
105000080515           close FIDN67P;
105100080117           callp(e) qCmdExc(C_CmdDltOvr : %size(C_CmdDltOvr));
105200080117         endif;
105300080515
105400080515         if  %open(FIDN67PM);
105500080515           close FIDN67PM;
105600080515           callp(e) qCmdExc(C_CmdDltOvrM : %size(C_CmdDltOvrM));
105700080515         endif;
105800080117
105900080117       ENDSR;
106000080118
106100080118       //--------------------------------------------------------------
106200080118       //?Reperimento descrizione anomalia
106300080118       //--------------------------------------------------------------
106400080118       BEGSR rep_DesTAD;
106500080118
106600080118         clear YY;
106700080118         clear $TAD;
106800080118
106900080118         DCStpd = 'C';
107000080118         DCSnkt = Wnumca;
107100080118         DCSstd = 'A';
107200080118
107300080515         setll %kds(k07dcs01 : 3) FNDCS000;
107400080515         reade %kds(k07dcs01 : 3) FNDCS000;
107500080118
107600080121         dow  NOT  %eof(FNDCS01L)   and
107700080118              YY < %elem($TAD);
107800080118           yy += 1;
107900080118           $TAD(yy) = DCSnot;
108000080515           reade %kds(k07dcs01 : 3) FNDCS000;
108100080121         enddo;
108200080118
108300080515         POCda1 = $TAD(1);
108400080515         POCda2 = $TAD(2) + $TAD(3);
108500080118
108600080118       ENDSR;
108700080118
108800080118       //--------------------------------------------------------------
108900080118       //?Reperimento descrizione pezzi danneggiati/mancanti
109000080118       //--------------------------------------------------------------
109100080118       BEGSR rep_DesPZD;
109200080118
109300080118         clear YY;
109400080118         clear $TAD;
109500080118
109600080118         DCStpd = 'C';
109700080118         DCSnkt = Wnumca;
109800080118         DCSstd = 'P';
109900080118
110000080515         setll %kds(k07dcs01 : 3) FNDCS000;
110100080515         reade %kds(k07dcs01 : 3) FNDCS000;
110200080118
110300080121         dow  NOT  %eof(FNDCS01L)   and
110400080118              YY < %elem($TAD);
110500080118           yy += 1;
110600080118           $TAD(yy) = DCSnot;
110700080515           reade %kds(k07dcs01 : 3) FNDCS000;
110800080121         enddo;
110900080118
111000080515         POCdpz = $TAD(1);
111100080516         POCdp1 = $TAD(2) + $TAD(3);
111200080118
111300080118       ENDSR;
111400080117
111500080117       //--------------------------------------------------------------
111600080117       //?Operazioni finali.
111700080117       //--------------------------------------------------------------
111800080117       BEGSR RoutEnd;
111900080117
112000080118         kpjbu = FIDN00ds;
112100080516
112200080516         clear TIBS02ds;
112300080516         T02tla = 'C';
112400080516         tibs02r( kpjba : tibs02ds );
112500080516
112600080516         PrmFine = 'L';
112700080516         clear PrmFil ;
112800080516         clear PrmTpOutQ ;
112900080516         clear PrmOutQ ;
113000080516         clear PrmOutQlib ;
113100080516         clear PrmEsito ;
113200080516         callp TRULOUTQ (PrmFine:Prmfil:PrmTpOutq:
113300080516         PrmOutQ:PrmOutQLib:PrmEsito);
113400080516
113500080516         *INLR = *ON ;
113600080117         return;
113700080117
113800080117       ENDSR;
113900080117
114000080117      /end-free
114100080515**  $Cmd
114200080515OVRPRTF FIDN67PM OUTQ(          ) USRDFNDTA('                                         1
114300080516OVRPRTF FIDN67P SPLFNAME(LETCOM) OUTQ(xxxxxxxxxx) FORMTYPE(xxxxxxxxxx)                2
114400080515OVRPRTF FIDN67P                                                        SAVE(*YES)     3
