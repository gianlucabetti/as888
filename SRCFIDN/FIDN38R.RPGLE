000100080117      //---------------------------------------------------------------
000200080117      //?INVIO E-MAIL DI CONTESTAZIONE A FEDEX
000300080117      //---------------------------------------------------------------
000400080117      // Richiamato da FIDN03R x le C.A. con le segg. caratteristiche:
000500080117      // 1) Fase 100
000600080117      // 2) FedEx (§DCTEURO di DCTFLO = "F")
000700080117      // 3) Responsabilità "PARTNER"
000800080117      //---------------------------------------------------------------
000900080117
001000080117     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001100080117
001200080117      //---------------------------------------------------------------
001300080117      //?Dichiarazione file.
001400080117      //---------------------------------------------------------------
001500080118
001600080117     fFNDCT01L  if   e           k disk
001700080117     fFNDET01L  if   e           k disk
001800080117     fFNDCS01L  if   e           k disk
001900080117
002000080117     fFNARB01L  if   e           k disk
002100080117     fFNBLP01L  if   e           k disk    prefix(ARB:3)
002200080125     fFIAR401L  if   e           k disk
002300080117
002400080117     fAZORG01L  if   e           k disk
002500080117
002600080117     fFIDN38P   o    e             printer usropn
002700080117     f                                     OFlind(*in01)
002800080117
002900080117      //---------------------------------------------------------------
003000080117      //?Definizione costanti.
003100080117      //---------------------------------------------------------------
003200080118
003300080117      // - Fase "Conferma C.A."
003400080117     d C_fca_con       c                   const(100)
003500080117
003600080117      // - Dati di default
003700080117
003800080117      // - Comandi di override al PrtF
003900080117     d C_CmdOvrPrtF    c                   const('OVRPRTF +
004000080117     d                                           file(FIDN38P) +
004100080122     d***                                        pagesize(66 120) +
004200080122     d***                                        lpi(6) cpi(15) +
004300080118     d***                                        pagrtt(0) +
004400080117     d                                           ovrscope(*actgrpdfn) +
004500080117     d                                           ')
004600080117     d C_CmdDltOvr     c                   const('DLTOVR +
004700080117     d                                            file(FIDN38P) +
004800080117     d                                            lvl(*actgrpdfn)')
004900080117
005000080117      //---------------------------------------------------------------
005100080117      //?Definizione schiere.
005200080117      //---------------------------------------------------------------
005300080118
005400080117      // - Descrizione Tipo Anomalia
005500080117     d $TAD            s             35    dim( 3) inz                          descr. tipo anomalia
005600080117
005700080117      // - Note da stampare
005800080117     d $Note           s            105    dim( 5) inz                          $TAD x 5
005900080117
006000080117      //---------------------------------------------------------------
006100080117      //?Definizione aree dati.
006200080117      //---------------------------------------------------------------
006300080118
006400080117      // - Dati utente
006500080117     d §AzUte        e ds                  extname(AZUTE00F)
006600080117     d                                     dtaara
006700080117     d §DatiUte      e ds                  extname(dDatiUte)
006800080117     d                                     dtaara
006900080117
007000080117      //---------------------------------------------------------------
007100080117      //?Definizione strutture dati.
007200080117      //---------------------------------------------------------------
007300080118
007400080117      // - Status
007500080117     d Psds           sds
007600080117     d   SDSpgm          *proc
007700080117
007800080117      // - Parametri ricevuti
007900080117     d KPJBA         e ds
008000080117     d FIDN00ds      e ds
008100080117     d   Wnumca                6     19
008200080117
008300080117      // - Reperimento dati utente
008400080117     d TIBS34ds      e ds
008500080117
008600080117      // - Flag operativi in FNDCT
008700080117     d dDCT01        e ds                  inz
008800080125
008900080125      // - Estensione blp/arb (campo AR4NOT file FIAR400F) tipo rec 'E'
009000080125     d dsBL4e        e ds                  inz
009100080117
009200080117      // - Ricerca/Controllo tabelle
009300080117     d TIBS02ds      e ds                  inz
009400080117      // - tab. MRA/DAN = Bart-Maling - Danni
009500080117     d dMRAdan       e ds
009600080118      // - tab. STD = Dati Standard - Danni
009700080125      //d dSTD2         e ds
009800080117      // - tab. TAD = Tipi anomalia danni
009900080117     d dTAD          e ds                  inz
010000080117
010100080117      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
010200080117     d TRTCM1ds      e ds                  inz
010300080118     d   §CM1sts     e                     inz(*off)
010400080117
010500080117      // - Parametri x Controllo & Inversione date
010600080117     d WLBdat          ds                  inz
010700080117     d  G02dat                 1      8  0 inz
010800080117     d  G02inv                 9     16  0 inz
010900080117     d  G02err                17     17    inz('3')
011000080117     d  G02tgi                18     22  0 inz
011100080117
011200080117      //---------------------------------------------------------------
011300080117      //?Definizione variabili globali.
011400080117      //---------------------------------------------------------------
011500080118
011600080117      // - Flags booleani
011700080117     d W_Esito         s               n   inz(*off)
011800080117     d $Invio          s               n   inz(*off)
011900080117
012000080117      // - Indici di schiera
012100080118     d YY              s              3  0 inz
012200080117
012300080117      // - Campi per QCMDEXC
012400080117     d Qcmd            s            512    inz
012500080117
012600080117      // - Campi di comodo
012700080118     d Wtad            s                   like(DCTtad)   inz
012800080117
012900080117      //---------------------------------------------------------------
013000080117      //?Definizione procedure usate.
013100080117      //---------------------------------------------------------------
013200080118
013300080117      // - Reperimento dati utente
013400080117     d tibs34r         pr                  extpgm('TIBS34R')
013500080117     d  tibs34ds                           likeds(TIBS34ds)
013600080117
013700080117      // - Gestione tabelle
013800080117     d tibs02r         pr                  extpgm('TIBS02R')
013900080117     d  kpjba                              likeds(kpjba)    const
014000080117     d  tibs02ds                           likeds(TIBS02ds)
014100080117
014200080117      // - Controllo & Inversione date
014300080117     d xsrda8          pr                  extpgm('XSRDA8')
014400080117     d  wlbdat                             likeds(WLBdat)
014500080117
014600080117      // - Esecuzione comando di sistema
014700080117     d qCmdExc         pr                  extpgm('QCMDEXC')
014800080117     d  Qcmd                        512    const  options(*varsize)
014900080117     d  Qlen                         15  5 const
015000080117
015100080117      //---------------------------------------------------------------
015200080117      //?Definizione key-list.
015300080117      //---------------------------------------------------------------
015400080118
015500080118      // - File FNDCT01L
015600080117     d k03dct01      e ds                  extname(FNDCT01L:*key)
015700080118     d                                     inz
015800080118      // - File FNDCS01L
015900080118     d k03dcs01      e ds                  extname(FNDCS01L:*key)
016000080118     d                                     inz
016100080118      // - File FNDET01L
016200080118     d k02det01      e ds                  extname(FNDET01L:*key)
016300080118     d                                     inz
016400080125      // - File FNBLP01L / FNARB01L
016500080125     d k04blparb     e ds                  extname(FNBLP01L:*key)
016600080125     d                                     prefix(ARB:3)
016700080125     d                                     inz
016800080125      // - File FIAR401L
016900080125     d k05ar401      e ds                  extname(FIAR401L:*key)
017000080125     d                                     inz
017100080117
017200080117      //---------------------------------------------------------------
017300080117      //?Riepilogo indicatori.
017400080117      //---------------------------------------------------------------
017500080117      //---------------------------------------------------------------
017600080118
017700080118      //---------------------------------------------------------------
017800080118      //?M A I N - L I N E
017900080118      //---------------------------------------------------------------
018000080117
018100080117     c     *Entry        plist
018200080117     c                   parm                    KPJBA
018300080117
018400080117      /free
018500080117
018600080117       // Operazioni iniziali
018700080117       exsr RoutInz;
018800080117
018900080118       // Preparazione della e-mail da inviare
019000080118       if  O00err = *blank;
019100080118         exsr sr_PrintMail;
019200080117       endif;
019300080117
019400080117       // Operazioni finali
019500080117       exsr RoutEnd;
019600080117
019700080117       //--------------------------------------------------------------
019800080117       //?Operazioni iniziali.
019900080117       //--------------------------------------------------------------
020000080117       BEGSR RoutInz;
020100080118
020200080118       // Ricezione parametri
020300080118         FIDN00ds = kpjbu;
020400080118         clear o00f12;
020500080118         clear o00f03;
020600080118         clear o00err;
020700080118         clear o00msg;
020800080118
020900080118       // Reperimento dati job
021000080118         exsr DatiJob;
021100080118
021200080118       // Reperimento tab. "MRA"
021300080118         exsr rep_TabMRA;
021400080118         if  o00err = 'E';
021500080118           leavesr;
021600080118         endif;
021700080118
021800080118       // Reperimento tab. "STD2"
021900080125       //exsr rep_TabSTD2;
022000080125       //if  o00err = 'E';
022100080125       //  leavesr;
022200080125       //endif;
022300080117
022400080117       // Verifica parametri ricecvuti
022500080118         if  i00aac = *zero   or
022600080118             i00fil = *zero   or
022700080118             i00nca = *zero;
022800080118           o00err = 'E';
022900080118           o00msg = 'Manca il numero C.A.!!!';
023000080117           leavesr;
023100080117         endif;
023200080117
023300080117       // Reperimento dati testata C.A.
023400080118         exsr rep_FNDCT;
023500080118         if  o00err = 'E';
023600080117           leavesr;
023700080117         endif;
023800080118
023900080118       // Decodifica filiale gestione
024000080118         chain (i00fgs) AZORG;
024100080118         if  NOT  %found(AZORG01L);
024200080118           o00err = 'E';
024300080118           o00msg = 'Filiale gestione errata';
024400080118           leavesr;
024500080118         endif;
024600080118
024700080117       // Reperimento dati spedizione
024800080118         exsr rep_FNBLPARB;
024900080117
025000080117       ENDSR;
025100080118
025200080118       //--------------------------------------------------------------
025300080118       //?Reperimento Dati del job (Utente/Operativi).
025400080118       //--------------------------------------------------------------
025500080118       BEGSR DatiJob;
025600080118
025700080118         in(E) §AzUte;
025800080118         if NOT %error;
025900080118           in(E) §DatiUte;
026000080118         endif;
026100080118         if %error or RSut = *blanks;
026200080118           clear TIBS34ds;
026300080118           tibs34r(tibs34ds);
026400080118           in §AzUte;
026500080118           in §DatiUte;
026600080118         endif;
026700080118
026800080118       ENDSR;
026900080118
027000080118       //--------------------------------------------------------------
027100080118       //?Reperimento dati testata C.A.
027200080118       //--------------------------------------------------------------
027300080118       BEGSR rep_FNDCT;
027400080118
027500080118         DCTaac = i00aac;
027600080118         DCTfil = i00fil;
027700080118         DCTnca = i00nca;
027800080118
027900080118         chain %kds(k03dct01 : 3) FNDCT000;
028000080118
028100080118         if  NOT  %found(FNDCT01L)   or
028200080118             DCTatb <> *blank;
028300080118           o00err = 'E';
028400080118           o00msg = 'C.A. non reperita';
028500080118           leavesr;
028600080118         endif;
028700080118
028800080118         dDCT01 = DCTflo;
028900080118
029000080118         if  DCTfca   <> C_fca_con   or
029100080118             §DCTeuro <> 'F'         or
029200080118             DCTres   <> 'P';
029300080118           o00err = 'E';
029400080118           o00msg = 'C.A. non contestabile a FedEx';
029500080118           leavesr;
029600080118         endif;
029700080118
029800080118       // Impostazione numero spedizione tra i parms SE non ricevuto
029900080118         if  i00aas = *zero   or
030000080118             i00lnp = *zero   or
030100080121           //i00nrs = *zero   or
030200080118             i00nsp = *zero;
030300080118           i00aas = DCTaas;
030400080118           i00lnp = DCTlnp;
030500080118           i00nrs = DCTnrs;
030600080118           i00nsp = DCTnsp;
030700080118         endif;
030800080118
030900080118       // Impostazione altri dati tra i parms SE non ricevuti
031000080118         if  i00fca = *zero;
031100080118           i00fca = DCTfca;
031200080118         endif;
031300080118         if  i00fgs = *zero;
031400080118           i00fgs = DCTgfc;
031500080118         endif;
031600080118         if  i00tad = *blank;
031700080118           i00tad = DCTtad;
031800080118         endif;
031900080118
032000080118       ENDSR;
032100080118
032200080118       //--------------------------------------------------------------
032300080118       //?Reperimento dati spedizione
032400080118       //--------------------------------------------------------------
032500080118       BEGSR rep_FNBLPARB;
032600080118
032700080118         IF  i00tpb = 'P';
032800080118
032900080118           ARBaas = i00aas;
033000080118           ARBlnp = i00lnp;
033100080118           ARBnrs = i00nrs;
033200080118           ARBnsp = i00nsp;
033300080118           chain %kds(k04blparb : 4) FNBLP000;
033400080118
033500080118           if  NOT  %found(FNBLP01L)   or
033600080118               ARBatb <> *blank;
033700080118             o00err = 'E';
033800080118             o00msg = 'Spedizione non reperita';
033900080118             leavesr;
034000080118           endif;
034100080118
034200080118         ELSE;
034300080118
034400080118           chain %kds(k04blparb : 4) FNARB000;
034500080118
034600080118           if  NOT  %found(FNARB01L)   or
034700080118               ARBatb <> *blank;
034800080118             o00err = 'E';
034900080118             o00msg = 'Spedizione non reperita';
035000080118             leavesr;
035100080118           endif;
035200080118
035300080118         ENDIF;
035400080125
035500080125         exsr rep_FIAR4;
035600080118
035700080118       ENDSR;
035800080125
035900080125       //--------------------------------------------------------------
036000080125       //?Reperimento dati spedizione
036100080125       //--------------------------------------------------------------
036200080125       BEGSR rep_FIAR4;
036300080125
036400080125         AR4aas = i00aas;
036500080125         AR4lnp = i00lnp;
036600080125         AR4nrs = i00nrs;
036700080125         AR4nsp = i00nsp;
036800080125         AR4trc = 'E';
036900080125
037000080125         chain %kds(k05ar401 : 5) FIAR4000;
037100080125
037200080206       //if  NOT  %found(FIAR401L);
037300080206       //  o00err = 'E';
037400080206       //  o00msg = 'AWB FedEx non reperita';
037500080206       //  leavesr;
037600080206       //endif;
037700080125
037800080206         if  %found(FIAR401L);
037900080206           dsBL4e = AR4not;
038000080206         else;
038100080206           clear dsBL4e;
038200080206         endif;
038300080125
038400080125       ENDSR;
038500080118
038600080118       //--------------------------------------------------------------
038700080118       //?Reperimento tab. MRA = Dati standard Danni 2
038800080118       //--------------------------------------------------------------
038900080125       //BEGSR rep_TabSTD2;
039000080125       //
039100080125       //  clear dSTD2;
039200080125       //  clear TIBS02ds;
039300080125       //
039400080125       //  T02mod = 'C';
039500080125       //  T02sif = knsif;
039600080125       //  T02cod = 'STD';
039700080125       //  T02ke1 = 'FEDEX';
039800080125       //
039900080125       //  tibs02r( kpjba : tibs02ds );
040000080125       //
040100080125       //  if T02err = *blanks;
040200080125       //    dSTD2   = T02uni;
040300080125       //  else;
040400080125       //    O00err = 'E';
040500080125       //    O00msg = 'Manca tab. STD'
040600080125       //           + ' "' + %trimr(T02ke1) + '"';
040700080125       //    leavesr;
040800080125       //  endif;
040900080125       //
041000080125       //ENDSR;
041100080118
041200080118       //--------------------------------------------------------------
041300080118       //?Reperimento tab. MRA = Bart-Maling - Danni
041400080118       //--------------------------------------------------------------
041500080118       BEGSR rep_TabMRA;
041600080118
041700080118         clear dMRAdan;
041800080118         clear TIBS02ds;
041900080118
042000080118         T02mod = 'C';
042100080118         T02sif = knsif;
042200080118         T02cod = 'MRA';
042300080118         T02ke1 = %trimr(SDSpgm) + '-FEDEX';
042400080118         T02ke2 = 'DAN';
042500080118
042600080118         tibs02r( kpjba : tibs02ds );
042700080118
042800080118         if T02err = *blanks;
042900080118           dMRAdan = T02uni;
043000080118         else;
043100080118           o00err = 'E';
043200080118           o00msg = 'Manca tab. MRA'
043300080118                  + ' "' + %trimr(T02ke1) + '" /'
043400080118                  + ' "' + %trimr(T02ke2) + '"';
043500080118           leavesr;
043600080118         endif;
043700080118
043800080118       ENDSR;
043900080118
044000080118       //--------------------------------------------------------------
044100080118       //?Reperimento tab. TAD = Tipo Anomalia
044200080118       //--------------------------------------------------------------
044300080118       BEGSR rep_TabTAD;
044400080118
044500080118         clear dTAD;
044600080118         clear TIBS02ds;
044700080118
044800080118         T02mod = 'C';
044900080118         T02sif = knsif;
045000080118         T02cod = 'TAD';
045100080118         T02ke1 = Wtad;
045200080118
045300080118         tibs02r( kpjba : tibs02ds );
045400080118
045500080118         if T02err = *blanks;
045600080118           dTAD = T02uni;
045700080118         endif;
045800080118
045900080118       ENDSR;
046000080117
046100080117       //--------------------------------------------------------------
046200080118       //?Stampa della lettera da inviare via e-mail.
046300080117       //--------------------------------------------------------------
046400080118       BEGSR sr_PrintMail;
046500080118
046600080118       // Override al file di stampa ed apertura dello stesso
046700080118         exsr sr_Override;
046800080117
046900080117       // Impostazione dei campi in stampa
047000080117         exsr sr_RieField;
047100080117
047200080117       // Impostazione indicatori utilizzati nel PrtF
047300080117         *in42  = (PT1da2 <> *blanks);
047400080117         *in43  = (PT1qtd <> *blanks);
047500080117         *in44  = (PT1dp2 <> *blanks);
047600080117         *in47  = (ARBrma <> *blanks);
047700080117       //*in48  = (DCTnev  > *zeros);
047800080117
047900080117       // Stampa
048000080117         write DN38T01;
048100080117
048200080117       // Chiusura file stampa & Cancellazione override
048300080117         exsr sr_DeleteOvr;
048400080117
048500080117       ENDSR;
048600080117
048700080117       //--------------------------------------------------------------
048800080117       //?Esecuzione override ed apertura del file di stampa.
048900080117       //--------------------------------------------------------------
049000080117       BEGSR sr_Override;
049100080117
049200080117         reset $Invio;
049300080117         reset TRTCM1ds;
049400080117
049500080118         if  §MRADreg <> *blank;
049600080121           §CM1mitt = %trim(§MRADmitt);
049700080117           §CM1dst  = %trim(§MRADdest);
049800080117           §CM1tips = §MRADreg;
049900080118           §CM1po   = %trim( %editw( DUTpou : '0   ' ));
050000080125         //§CM1var  = '*OBJM*' + §MRADdes;
050100080125           §CM1var  = '*OBJM*'
050200080125                    + 'Contestazione danno - AWB '
050300080125                    + %trimr(§B4erp)
050400080125                    + ' - CA '
050500080125                    + %editw(I00fil : '0   ')
050600080125                    + '/'
050700080125                    + %trim( %editc( I00nca : 'Z' ))
050800080125                    + ' '
050900080125                    + %subst( %char( I00aac ) : 3 : 2 );
051000080117           §CM1idp  = §MRADidpro;
051100080117           Qcmd = C_CmdOvrPrtF
051200080117                + ' outq(' + %trim(§MRADoutqi) + ')'
051300080117                + ' usrdfndta(''' + TRTCM1ds + ''')';
051400080122           callp(e) qCmdExc(Qcmd : %size(Qcmd));
051500080122           if  not %error;
051600080122             $Invio = *on;
051700080122           endif;
051800080118         endif;
051900080117
052000080117         open FIDN38P;
052100080117
052200080117       ENDSR;
052300080117
052400080117       //--------------------------------------------------------------
052500080117       //?Impostazione dei campi in stampa
052600080117       //--------------------------------------------------------------
052700080117       BEGSR sr_RieField;
052800080117
052900080117         clear DN38T01;
053000080118
053100080118       // Intestazione destinatario
053200080125       //PT1rgs = §STD2rsc;
053300080125       //PT1ind = §STD2ind;
053400080125       //PT1cap = §STD2cap;
053500080125       //PT1loc = §STD2loc;
053600080125       //PT1pro = §STD2prv;
053700080125         PT1rgs = 'Federal Express Europe';
053800080118
053900080118       // Località + data
054000080121         if  ORGfl1 = 'E'  and  ORGfva = *blank;
054100080118           PT1dal = 'MILANO, ' + %editc(*date : 'Y');
054200080121         else;
054300080118           PT1dal = %trim(ORGdes) + ', ' + %editc(*date : 'Y');
054400080121         endif;
054500080118
054600080118       // Oggetto
054700080118         PT1nca = %trim( %editw( I00fil:'0   ' )) + '/'
054800080118                + %trim( %editc( I00nca:'Z' ));
054900080118         PT1aac = I00aac;
055000080118
055100080118       // Dati relativi alla spedizione
055200080118         PT1lnp = I00lnp;
055300080118         PT1nrs = I00nrs;
055400080118         PT1nsp = I00nsp;
055500080118         reset WLBdat;
055600080118         G02inv = (I00aas * 10000) + ARBmgs;
055700080118         xsrda8(wlbdat);
055800080118         PT1dat = G02dat;
055900080118         PT1ncl = ARBncl;
056000080118         PT1pkf = ARBpkf;
056100080118
056200080118       // Dati relativi al mittente
056300080118         PT1rsm = ARBrsm;
056400080118         PT1rmn = ARBrmn;
056500080118         PT1rma = ARBrma;
056600080118
056700080118       // dati relativi al destinatario
056800080118         PT1rsd = ARBrsd;
056900080118         PT1lod = ARBlod;
057000080118         if  ARBnzd <> *blank;
057100080118           PT1prn = ARBnzd;
057200080118         else;
057300080118           PT1prn = ARBprd;
057400080118         endif;
057500080118
057600080118       // Anomalia
057700080118         Wtad = I00tad;
057800080118         exsr rep_TabTAD;
057900080121         if  §TADdest <> *blank;
058000080121           PT1tad = §TADdest;
058100080121         else;
058200080121           PT1tad = §TADdesc;
058300080121         endif;
058400080121       // Descrizione anomalia
058500080121         exsr Rep_DesTAD;
058600080118
058700080118       // Evento
058800080121         IF  DCTnev > *zero;
058900080118           PT1kev = %trim( %editw( DCTaae : '0    ' )) + '/'
059000080118                  + %trim( %editc( DCTnev : 'Z' ));
059100080118           chain (DCTaae : DCTnev) FNDET000;
059200080118           if  %found(FNDET01L);
059300080118             Wtad   = DETtad;
059400080118             exsr rep_TabTAD;
059500080121             if  §TADdest <> *blank;
059600080118               PT1tae = §TADdest;
059700080121             else;
059800080118               PT1tae = §TADdesc;
059900080121             endif;
060000080121           endif;
060100080121         ENDIF;
060200080118
060300080118       // Quantità danneggiate
060400080118       // - Peso
060500080121         if  DCTpkd > *zero;
060600080118           PT1qtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
060700080121         endif;
060800080118       // - Colli
060900080121         if  DCTncn > *zero;
061000080118           PT1qtd = %trim(PT1qtd)
061100080118                  + ' Colli ' + %trim( %editc( DCTncn : 'Z' ));
061200080121         endif;
061300080118       // - Pezzi
061400080118       //   + Descrizione pezzi danneggiati
061500080121         if  DCTnpz > *zero;
061600080118           PT1qtd  = %trim(PT1qtd)
061700080118                   + ' Pezzi ' + %trim( %editc( DCTnpz : 'Z' ));
061800080118           exsr Rep_DesPZD;
061900080121         endif;
062000080117
062100080117       ENDSR;
062200080117
062300080117       //--------------------------------------------------------------
062400080117       //?Chiusura file stampa & Cancellazione override
062500080117       //--------------------------------------------------------------
062600080117       BEGSR sr_DeleteOvr;
062700080117
062800080118         if  %open(FIDN38P);
062900080117           close FIDN38P;
063000080117           callp(e) qCmdExc(C_CmdDltOvr : %size(C_CmdDltOvr));
063100080117         endif;
063200080117
063300080117       ENDSR;
063400080118
063500080118       //--------------------------------------------------------------
063600080118       //?Reperimento descrizione anomalia
063700080118       //--------------------------------------------------------------
063800080118       BEGSR rep_DesTAD;
063900080118
064000080118         clear YY;
064100080118         clear $TAD;
064200080118
064300080118         DCStpd = 'C';
064400080118         DCSnkt = Wnumca;
064500080118         DCSstd = 'A';
064600080118
064700080118         setll %kds(k03dcs01 : 3) FNDCS000;
064800080118         reade %kds(k03dcs01 : 3) FNDCS000;
064900080118
065000080121         dow  NOT  %eof(FNDCS01L)   and
065100080118              YY < %elem($TAD);
065200080118           yy += 1;
065300080118           $TAD(yy) = DCSnot;
065400080118           reade %kds(k03dcs01 : 3) FNDCS000;
065500080121         enddo;
065600080118
065700080118         PT1da1 = $TAD(1);
065800080118         PT1da2 = $TAD(2) + $TAD(3);
065900080118
066000080118       ENDSR;
066100080118
066200080118       //--------------------------------------------------------------
066300080118       //?Reperimento descrizione pezzi danneggiati/mancanti
066400080118       //--------------------------------------------------------------
066500080118       BEGSR rep_DesPZD;
066600080118
066700080118         clear YY;
066800080118         clear $TAD;
066900080118
067000080118         DCStpd = 'C';
067100080118         DCSnkt = Wnumca;
067200080118         DCSstd = 'P';
067300080118
067400080118         setll %kds(k03dcs01 : 3) FNDCS000;
067500080118         reade %kds(k03dcs01 : 3) FNDCS000;
067600080118
067700080121         dow  NOT  %eof(FNDCS01L)   and
067800080118              YY < %elem($TAD);
067900080118           yy += 1;
068000080118           $TAD(yy) = DCSnot;
068100080118           reade %kds(k03dcs01 : 3) FNDCS000;
068200080121         enddo;
068300080118
068400080118         PT1dpz = $TAD(1);
068500080118         PT1dp2 = $TAD(2) + $TAD(3);
068600080118
068700080118       ENDSR;
068800080117
068900080117       //--------------------------------------------------------------
069000080117       //?Operazioni finali.
069100080117       //--------------------------------------------------------------
069200080117       BEGSR RoutEnd;
069300080117
069400080118         kpjbu = FIDN00ds;
069500080117         return;
069600080117
069700080117       ENDSR;
069800080117
069900080117      /end-free
