000100080117      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF)
000200080117      //---------------------------------------------------------------
000300080117      //?INVIO E-MAIL DI CONTESTAZIONE A FEDEX
000400080117      //---------------------------------------------------------------
000500080117      // Richiamato da FIDN03R x le C.A. con le segg. caratteristiche:
000600080117      // 1) Fase 100
000700080117      // 2) FedEx (§DCTEURO di DCTFLO = "F")
000800080117      // 3) Responsabilità "PARTNER"
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
002300080125     fFIAR401L  if   e           k disk
002400080117
002500080117     fAZORG01L  if   e           k disk
002600080117
002700080117     fFIDN38P   o    e             printer usropn
002800080117     f                                     OFlind(*in01)
002900080117
003000080117      //---------------------------------------------------------------
003100080117      //?Definizione costanti.
003200080117      //---------------------------------------------------------------
003300080118
003400080117      // - Fase "Conferma C.A."
003500080117     d C_fca_con       c                   const(100)
003600080117
003700080117      // - Dati di default
003800080117
003900080117      // - Comandi di override al PrtF
004000080117     d C_CmdOvrPrtF    c                   const('OVRPRTF +
004100080117     d                                           file(FIDN38P) +
004200080122     d***                                        pagesize(66 120) +
004300080122     d***                                        lpi(6) cpi(15) +
004400080118     d***                                        pagrtt(0) +
004500080117     d                                           ovrscope(*actgrpdfn) +
004600080117     d                                           ')
004700080117     d C_CmdDltOvr     c                   const('DLTOVR +
004800080117     d                                            file(FIDN38P) +
004900080117     d                                            lvl(*actgrpdfn)')
005000080117
005100080117      //---------------------------------------------------------------
005200080117      //?Definizione schiere.
005300080117      //---------------------------------------------------------------
005400080118
005500080117      // - Descrizione Tipo Anomalia
005600080117     d $TAD            s             35    dim( 3) inz                          descr. tipo anomalia
005700080117
005800080117      // - Note da stampare
005900080117     d $Note           s            105    dim( 5) inz                          $TAD x 5
006000080117
006100080117      //---------------------------------------------------------------
006200080117      //?Definizione aree dati.
006300080117      //---------------------------------------------------------------
006400080118
006500080117      // - Dati utente
006600080117     d §AzUte        e ds                  extname(AZUTE00F)
006700080117     d                                     dtaara
006800080117     d §DatiUte      e ds                  extname(dDatiUte)
006900080117     d                                     dtaara
007000080117
007100080117      //---------------------------------------------------------------
007200080117      //?Definizione strutture dati.
007300080117      //---------------------------------------------------------------
007400080118
007500080117      // - Status
007600080117     d Psds           sds
007700080117     d   SDSpgm          *proc
007800080117
007900080117      // - Parametri ricevuti
008000080117     d KPJBA         e ds
008100080117     d FIDN00ds      e ds
008200080117     d   Wnumca                6     19
008300080117
008400080117      // - Reperimento dati utente
008500080117     d TIBS34ds      e ds
008600080117
008700080117      // - Flag operativi in FNDCT
008800080117     d dDCT01        e ds                  inz
008900080125
009000080125      // - Estensione blp/arb (campo AR4NOT file FIAR400F) tipo rec 'E'
009100080125     d dsBL4e        e ds                  inz
009200080117
009300080117      // - Ricerca/Controllo tabelle
009400080117     d TIBS02ds      e ds                  inz
009500080117      // - tab. MRA/DAN = Bart-Maling - Danni
009600080117     d dMRAdan       e ds
009700080118      // - tab. STD = Dati Standard - Danni
009800080125      //d dSTD2         e ds
009900080117      // - tab. TAD = Tipi anomalia danni
010000080117     d dTAD          e ds                  inz
010100080117
010200080117      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
010300080117     d TRTCM1ds      e ds                  inz
010400080118     d   §CM1sts     e                     inz(*off)
010500080117
010600080117      // - Parametri x Controllo & Inversione date
010700080117     d WLBdat          ds                  inz
010800080117     d  G02dat                 1      8  0 inz
010900080117     d  G02inv                 9     16  0 inz
011000080117     d  G02err                17     17    inz('3')
011100080117     d  G02tgi                18     22  0 inz
011200080117
011300080117      //---------------------------------------------------------------
011400080117      //?Definizione variabili globali.
011500080117      //---------------------------------------------------------------
011600080118
011700080117      // - Flags booleani
011800080117     d W_Esito         s               n   inz(*off)
011900080117     d $Invio          s               n   inz(*off)
012000080117
012100080117      // - Indici di schiera
012200080118     d YY              s              3  0 inz
012300080117
012400080117      // - Campi per QCMDEXC
012500080117     d Qcmd            s            512    inz
012600080117
012700080117      // - Campi di comodo
012800080118     d Wtad            s                   like(DCTtad)   inz
012900080117
013000080117      //---------------------------------------------------------------
013100080117      //?Definizione procedure usate.
013200080117      //---------------------------------------------------------------
013300080118
013400080117      // - Reperimento dati utente
013500080117     d tibs34r         pr                  extpgm('TIBS34R')
013600080117     d  tibs34ds                           likeds(TIBS34ds)
013700080117
013800080117      // - Gestione tabelle
013900080117     d tibs02r         pr                  extpgm('TIBS02R')
014000080117     d  kpjba                              likeds(kpjba)    const
014100080117     d  tibs02ds                           likeds(TIBS02ds)
014200080117
014300080117      // - Controllo & Inversione date
014400080117     d xsrda8          pr                  extpgm('XSRDA8')
014500080117     d  wlbdat                             likeds(WLBdat)
014600080117
014700080117      // - Esecuzione comando di sistema
014800080117     d qCmdExc         pr                  extpgm('QCMDEXC')
014900080117     d  Qcmd                        512    const  options(*varsize)
015000080117     d  Qlen                         15  5 const
015100080117
015200080117      //---------------------------------------------------------------
015300080117      //?Definizione key-list.
015400080117      //---------------------------------------------------------------
015500080118
015600080118      // - File FNDCT01L
015700080117     d k03dct01      e ds                  extname(FNDCT01L:*key)
015800080118     d                                     inz
015900080118      // - File FNDCS01L
016000080118     d k03dcs01      e ds                  extname(FNDCS01L:*key)
016100080118     d                                     inz
016200080118      // - File FNDET01L
016300080118     d k02det01      e ds                  extname(FNDET01L:*key)
016400080118     d                                     inz
016500080125      // - File FNBLP01L / FNARB01L
016600080125     d k04blparb     e ds                  extname(FNBLP01L:*key)
016700080125     d                                     prefix(ARB:3)
016800080125     d                                     inz
016900080125      // - File FIAR401L
017000080125     d k05ar401      e ds                  extname(FIAR401L:*key)
017100080125     d                                     inz
017200080117
017300080117      //---------------------------------------------------------------
017400080117      //?Riepilogo indicatori.
017500080117      //---------------------------------------------------------------
017600080117      //---------------------------------------------------------------
017700080118
017800080118      //---------------------------------------------------------------
017900080118      //?M A I N - L I N E
018000080118      //---------------------------------------------------------------
018100080117
018200080117     c     *Entry        plist
018300080117     c                   parm                    KPJBA
018400080117
018500080117      /free
018600080117
018700080117       // Operazioni iniziali
018800080117       exsr RoutInz;
018900080117
019000080118       // Preparazione della e-mail da inviare
019100080118       if  O00err = *blank;
019200080118         exsr sr_PrintMail;
019300080117       endif;
019400080117
019500080117       // Operazioni finali
019600080117       exsr RoutEnd;
019700080117
019800080117       //--------------------------------------------------------------
019900080117       //?Operazioni iniziali.
020000080117       //--------------------------------------------------------------
020100080117       BEGSR RoutInz;
020200080118
020300080118       // Ricezione parametri
020400080118         FIDN00ds = kpjbu;
020500080118         clear o00f12;
020600080118         clear o00f03;
020700080118         clear o00err;
020800080118         clear o00msg;
020900080118
021000080118       // Reperimento dati job
021100080118         exsr DatiJob;
021200080118
021300080118       // Reperimento tab. "MRA"
021400080118         exsr rep_TabMRA;
021500080118         if  o00err = 'E';
021600080118           leavesr;
021700080118         endif;
021800080118
021900080118       // Reperimento tab. "STD2"
022000080125       //exsr rep_TabSTD2;
022100080125       //if  o00err = 'E';
022200080125       //  leavesr;
022300080125       //endif;
022400080117
022500080117       // Verifica parametri ricecvuti
022600080118         if  i00aac = *zero   or
022700080118             i00fil = *zero   or
022800080118             i00nca = *zero;
022900080118           o00err = 'E';
023000080118           o00msg = 'Manca il numero C.A.!!!';
023100080117           leavesr;
023200080117         endif;
023300080117
023400080117       // Reperimento dati testata C.A.
023500080118         exsr rep_FNDCT;
023600080118         if  o00err = 'E';
023700080117           leavesr;
023800080117         endif;
023900080118
024000080118       // Decodifica filiale gestione
024100080118         chain (i00fgs) AZORG;
024200080118         if  NOT  %found(AZORG01L);
024300080118           o00err = 'E';
024400080118           o00msg = 'Filiale gestione errata';
024500080118           leavesr;
024600080118         endif;
024700080118
024800080117       // Reperimento dati spedizione
024900080118         exsr rep_FNBLPARB;
025000080117
025100080117       ENDSR;
025200080118
025300080118       //--------------------------------------------------------------
025400080118       //?Reperimento Dati del job (Utente/Operativi).
025500080118       //--------------------------------------------------------------
025600080118       BEGSR DatiJob;
025700080118
025800080118         in(E) §AzUte;
025900080118         if NOT %error;
026000080118           in(E) §DatiUte;
026100080118         endif;
026200080118         if %error or RSut = *blanks;
026300080118           clear TIBS34ds;
026400080118           tibs34r(tibs34ds);
026500080118           in §AzUte;
026600080118           in §DatiUte;
026700080118         endif;
026800080118
026900080118       ENDSR;
027000080118
027100080118       //--------------------------------------------------------------
027200080118       //?Reperimento dati testata C.A.
027300080118       //--------------------------------------------------------------
027400080118       BEGSR rep_FNDCT;
027500080118
027600080118         DCTaac = i00aac;
027700080118         DCTfil = i00fil;
027800080118         DCTnca = i00nca;
027900080118
028000080118         chain %kds(k03dct01 : 3) FNDCT000;
028100080118
028200080118         if  NOT  %found(FNDCT01L)   or
028300080118             DCTatb <> *blank;
028400080118           o00err = 'E';
028500080118           o00msg = 'C.A. non reperita';
028600080118           leavesr;
028700080118         endif;
028800080118
028900080118         dDCT01 = DCTflo;
029000080118
029100080118         if  DCTfca   <> C_fca_con   or
029200080118             §DCTeuro <> 'F'         or
029300080118             DCTres   <> 'P';
029400080118           o00err = 'E';
029500080118           o00msg = 'C.A. non contestabile a FedEx';
029600080118           leavesr;
029700080118         endif;
029800080118
029900080118       // Impostazione numero spedizione tra i parms SE non ricevuto
030000080118         if  i00aas = *zero   or
030100080118             i00lnp = *zero   or
030200080121           //i00nrs = *zero   or
030300080118             i00nsp = *zero;
030400080118           i00aas = DCTaas;
030500080118           i00lnp = DCTlnp;
030600080118           i00nrs = DCTnrs;
030700080118           i00nsp = DCTnsp;
030800080118         endif;
030900080118
031000080118       // Impostazione altri dati tra i parms SE non ricevuti
031100080118         if  i00fca = *zero;
031200080118           i00fca = DCTfca;
031300080118         endif;
031400080118         if  i00fgs = *zero;
031500080118           i00fgs = DCTgfc;
031600080118         endif;
031700080118         if  i00tad = *blank;
031800080118           i00tad = DCTtad;
031900080118         endif;
032000080118
032100080118       ENDSR;
032200080118
032300080118       //--------------------------------------------------------------
032400080118       //?Reperimento dati spedizione
032500080118       //--------------------------------------------------------------
032600080118       BEGSR rep_FNBLPARB;
032700080118
032800080118         IF  i00tpb = 'P';
032900080118
033000080118           ARBaas = i00aas;
033100080118           ARBlnp = i00lnp;
033200080118           ARBnrs = i00nrs;
033300080118           ARBnsp = i00nsp;
033400080118           chain %kds(k04blparb : 4) FNBLP000;
033500080118
033600080118           if  NOT  %found(FNBLP01L)   or
033700080118               ARBatb <> *blank;
033800080118             o00err = 'E';
033900080118             o00msg = 'Spedizione non reperita';
034000080118             leavesr;
034100080118           endif;
034200080118
034300080118         ELSE;
034400080118
034500080118           chain %kds(k04blparb : 4) FNARB000;
034600080118
034700080118           if  NOT  %found(FNARB01L)   or
034800080118               ARBatb <> *blank;
034900080118             o00err = 'E';
035000080118             o00msg = 'Spedizione non reperita';
035100080118             leavesr;
035200080118           endif;
035300080118
035400080118         ENDIF;
035500080125
035600080125         exsr rep_FIAR4;
035700080118
035800080118       ENDSR;
035900080125
036000080125       //--------------------------------------------------------------
036100080125       //?Reperimento dati spedizione
036200080125       //--------------------------------------------------------------
036300080125       BEGSR rep_FIAR4;
036400080125
036500080125         AR4aas = i00aas;
036600080125         AR4lnp = i00lnp;
036700080125         AR4nrs = i00nrs;
036800080125         AR4nsp = i00nsp;
036900080125         AR4trc = 'E';
037000080125
037100080125         chain %kds(k05ar401 : 5) FIAR4000;
037200080125
037300080206       //if  NOT  %found(FIAR401L);
037400080206       //  o00err = 'E';
037500080206       //  o00msg = 'AWB FedEx non reperita';
037600080206       //  leavesr;
037700080206       //endif;
037800080125
037900080206         if  %found(FIAR401L);
038000080206           dsBL4e = AR4not;
038100080206         else;
038200080206           clear dsBL4e;
038300080206         endif;
038400080125
038500080125       ENDSR;
038600080118
038700080118       //--------------------------------------------------------------
038800080118       //?Reperimento tab. MRA = Dati standard Danni 2
038900080118       //--------------------------------------------------------------
039000080125       //BEGSR rep_TabSTD2;
039100080125       //
039200080125       //  clear dSTD2;
039300080125       //  clear TIBS02ds;
039400080125       //
039500080125       //  T02mod = 'C';
039600080125       //  T02sif = knsif;
039700080125       //  T02cod = 'STD';
039800080125       //  T02ke1 = 'FEDEX';
039900080125       //
040000080125       //  tibs02r( kpjba : tibs02ds );
040100080125       //
040200080125       //  if T02err = *blanks;
040300080125       //    dSTD2   = T02uni;
040400080125       //  else;
040500080125       //    O00err = 'E';
040600080125       //    O00msg = 'Manca tab. STD'
040700080125       //           + ' "' + %trimr(T02ke1) + '"';
040800080125       //    leavesr;
040900080125       //  endif;
041000080125       //
041100080125       //ENDSR;
041200080118
041300080118       //--------------------------------------------------------------
041400080118       //?Reperimento tab. MRA = Bart-Maling - Danni
041500080118       //--------------------------------------------------------------
041600080118       BEGSR rep_TabMRA;
041700080118
041800080118         clear dMRAdan;
041900080118         clear TIBS02ds;
042000080118
042100080118         T02mod = 'C';
042200080118         T02sif = knsif;
042300080118         T02cod = 'MRA';
042400080118         T02ke1 = %trimr(SDSpgm) + '-FEDEX';
042500080118         T02ke2 = 'DAN';
042600080118
042700080118         tibs02r( kpjba : tibs02ds );
042800080118
042900080118         if T02err = *blanks;
043000080118           dMRAdan = T02uni;
043100080118         else;
043200080118           o00err = 'E';
043300080118           o00msg = 'Manca tab. MRA'
043400080118                  + ' "' + %trimr(T02ke1) + '" /'
043500080118                  + ' "' + %trimr(T02ke2) + '"';
043600080118           leavesr;
043700080118         endif;
043800080118
043900080118       ENDSR;
044000080118
044100080118       //--------------------------------------------------------------
044200080118       //?Reperimento tab. TAD = Tipo Anomalia
044300080118       //--------------------------------------------------------------
044400080118       BEGSR rep_TabTAD;
044500080118
044600080118         clear dTAD;
044700080118         clear TIBS02ds;
044800080118
044900080118         T02mod = 'C';
045000080118         T02sif = knsif;
045100080118         T02cod = 'TAD';
045200080118         T02ke1 = Wtad;
045300080118
045400080118         tibs02r( kpjba : tibs02ds );
045500080118
045600080118         if T02err = *blanks;
045700080118           dTAD = T02uni;
045800080118         endif;
045900080118
046000080118       ENDSR;
046100080117
046200080117       //--------------------------------------------------------------
046300080118       //?Stampa della lettera da inviare via e-mail.
046400080117       //--------------------------------------------------------------
046500080118       BEGSR sr_PrintMail;
046600080118
046700080118       // Override al file di stampa ed apertura dello stesso
046800080118         exsr sr_Override;
046900080117
047000080117       // Impostazione dei campi in stampa
047100080117         exsr sr_RieField;
047200080117
047300080117       // Impostazione indicatori utilizzati nel PrtF
047400080117         *in42  = (PT1da2 <> *blanks);
047500080117         *in43  = (PT1qtd <> *blanks);
047600080117         *in44  = (PT1dp2 <> *blanks);
047700080117         *in47  = (ARBrma <> *blanks);
047800080117       //*in48  = (DCTnev  > *zeros);
047900080117
048000080117       // Stampa
048100080117         write DN38T01;
048200080117
048300080117       // Chiusura file stampa & Cancellazione override
048400080117         exsr sr_DeleteOvr;
048500080117
048600080117       ENDSR;
048700080117
048800080117       //--------------------------------------------------------------
048900080117       //?Esecuzione override ed apertura del file di stampa.
049000080117       //--------------------------------------------------------------
049100080117       BEGSR sr_Override;
049200080117
049300080117         reset $Invio;
049400080117         reset TRTCM1ds;
049500080117
049600080118         if  §MRADreg <> *blank;
049700080121           §CM1mitt = %trim(§MRADmitt);
049800080117           §CM1dst  = %trim(§MRADdest);
049900080117           §CM1tips = §MRADreg;
050000080118           §CM1po   = %trim( %editw( DUTpou : '0   ' ));
050100080125         //§CM1var  = '*OBJM*' + §MRADdes;
050200080125           §CM1var  = '*OBJM*'
050300080125                    + 'Contestazione danno - AWB '
050400080125                    + %trimr(§B4erp)
050500080125                    + ' - CA '
050600080125                    + %editw(I00fil : '0   ')
050700080125                    + '/'
050800080125                    + %trim( %editc( I00nca : 'Z' ))
050900080125                    + ' '
051000080125                    + %subst( %char( I00aac ) : 3 : 2 );
051100080117           §CM1idp  = §MRADidpro;
051200080117           Qcmd = C_CmdOvrPrtF
051300080117                + ' outq(' + %trim(§MRADoutqi) + ')'
051400080117                + ' usrdfndta(''' + TRTCM1ds + ''')';
051500080122           callp(e) qCmdExc(Qcmd : %size(Qcmd));
051600080122           if  not %error;
051700080122             $Invio = *on;
051800080122           endif;
051900080118         endif;
052000080117
052100080117         open FIDN38P;
052200080117
052300080117       ENDSR;
052400080117
052500080117       //--------------------------------------------------------------
052600080117       //?Impostazione dei campi in stampa
052700080117       //--------------------------------------------------------------
052800080117       BEGSR sr_RieField;
052900080117
053000080117         clear DN38T01;
053100080118
053200080118       // Intestazione destinatario
053300080125       //PT1rgs = §STD2rsc;
053400080125       //PT1ind = §STD2ind;
053500080125       //PT1cap = §STD2cap;
053600080125       //PT1loc = §STD2loc;
053700080125       //PT1pro = §STD2prv;
053800080125         PT1rgs = 'Federal Express Europe';
053900080118
054000080118       // Località + data
054100080121         if  ORGfl1 = 'E'  and  ORGfva = *blank;
054200080118           PT1dal = 'MILANO, ' + %editc(*date : 'Y');
054300080121         else;
054400080118           PT1dal = %trim(ORGdes) + ', ' + %editc(*date : 'Y');
054500080121         endif;
054600080118
054700080118       // Oggetto
054800080118         PT1nca = %trim( %editw( I00fil:'0   ' )) + '/'
054900080118                + %trim( %editc( I00nca:'Z' ));
055000080118         PT1aac = I00aac;
055100080118
055200080118       // Dati relativi alla spedizione
055300080118         PT1lnp = I00lnp;
055400080118         PT1nrs = I00nrs;
055500080118         PT1nsp = I00nsp;
055600080118         reset WLBdat;
055700080118         G02inv = (I00aas * 10000) + ARBmgs;
055800080118         xsrda8(wlbdat);
055900080118         PT1dat = G02dat;
056000080118         PT1ncl = ARBncl;
056100080118         PT1pkf = ARBpkf;
056200080118
056300080118       // Dati relativi al mittente
056400080118         PT1rsm = ARBrsm;
056500080118         PT1rmn = ARBrmn;
056600080118         PT1rma = ARBrma;
056700080118
056800080118       // dati relativi al destinatario
056900080118         PT1rsd = ARBrsd;
057000080118         PT1lod = ARBlod;
057100080118         if  ARBnzd <> *blank;
057200080118           PT1prn = ARBnzd;
057300080118         else;
057400080118           PT1prn = ARBprd;
057500080118         endif;
057600080118
057700080118       // Anomalia
057800080118         Wtad = I00tad;
057900080118         exsr rep_TabTAD;
058000080121         if  §TADdest <> *blank;
058100080121           PT1tad = §TADdest;
058200080121         else;
058300080121           PT1tad = §TADdesc;
058400080121         endif;
058500080121       // Descrizione anomalia
058600080121         exsr Rep_DesTAD;
058700080118
058800080118       // Evento
058900080121         IF  DCTnev > *zero;
059000080118           PT1kev = %trim( %editw( DCTaae : '0    ' )) + '/'
059100080118                  + %trim( %editc( DCTnev : 'Z' ));
059200080118           chain (DCTaae : DCTnev) FNDET000;
059300080118           if  %found(FNDET01L);
059400080118             Wtad   = DETtad;
059500080118             exsr rep_TabTAD;
059600080121             if  §TADdest <> *blank;
059700080118               PT1tae = §TADdest;
059800080121             else;
059900080118               PT1tae = §TADdesc;
060000080121             endif;
060100080121           endif;
060200080121         ENDIF;
060300080118
060400080118       // Quantità danneggiate
060500080118       // - Peso
060600080121         if  DCTpkd > *zero;
060700080118           PT1qtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
060800080121         endif;
060900080118       // - Colli
061000080121         if  DCTncn > *zero;
061100080118           PT1qtd = %trim(PT1qtd)
061200080118                  + ' Colli ' + %trim( %editc( DCTncn : 'Z' ));
061300080121         endif;
061400080118       // - Pezzi
061500080118       //   + Descrizione pezzi danneggiati
061600080121         if  DCTnpz > *zero;
061700080118           PT1qtd  = %trim(PT1qtd)
061800080118                   + ' Pezzi ' + %trim( %editc( DCTnpz : 'Z' ));
061900080118           exsr Rep_DesPZD;
062000080121         endif;
062100080117
062200080117       ENDSR;
062300080117
062400080117       //--------------------------------------------------------------
062500080117       //?Chiusura file stampa & Cancellazione override
062600080117       //--------------------------------------------------------------
062700080117       BEGSR sr_DeleteOvr;
062800080117
062900080118         if  %open(FIDN38P);
063000080117           close FIDN38P;
063100080117           callp(e) qCmdExc(C_CmdDltOvr : %size(C_CmdDltOvr));
063200080117         endif;
063300080117
063400080117       ENDSR;
063500080118
063600080118       //--------------------------------------------------------------
063700080118       //?Reperimento descrizione anomalia
063800080118       //--------------------------------------------------------------
063900080118       BEGSR rep_DesTAD;
064000080118
064100080118         clear YY;
064200080118         clear $TAD;
064300080118
064400080118         DCStpd = 'C';
064500080118         DCSnkt = Wnumca;
064600080118         DCSstd = 'A';
064700080118
064800080118         setll %kds(k03dcs01 : 3) FNDCS000;
064900080118         reade %kds(k03dcs01 : 3) FNDCS000;
065000080118
065100080121         dow  NOT  %eof(FNDCS01L)   and
065200080118              YY < %elem($TAD);
065300080118           yy += 1;
065400080118           $TAD(yy) = DCSnot;
065500080118           reade %kds(k03dcs01 : 3) FNDCS000;
065600080121         enddo;
065700080118
065800080118         PT1da1 = $TAD(1);
065900080118         PT1da2 = $TAD(2) + $TAD(3);
066000080118
066100080118       ENDSR;
066200080118
066300080118       //--------------------------------------------------------------
066400080118       //?Reperimento descrizione pezzi danneggiati/mancanti
066500080118       //--------------------------------------------------------------
066600080118       BEGSR rep_DesPZD;
066700080118
066800080118         clear YY;
066900080118         clear $TAD;
067000080118
067100080118         DCStpd = 'C';
067200080118         DCSnkt = Wnumca;
067300080118         DCSstd = 'P';
067400080118
067500080118         setll %kds(k03dcs01 : 3) FNDCS000;
067600080118         reade %kds(k03dcs01 : 3) FNDCS000;
067700080118
067800080121         dow  NOT  %eof(FNDCS01L)   and
067900080118              YY < %elem($TAD);
068000080118           yy += 1;
068100080118           $TAD(yy) = DCSnot;
068200080118           reade %kds(k03dcs01 : 3) FNDCS000;
068300080121         enddo;
068400080118
068500080118         PT1dpz = $TAD(1);
068600080118         PT1dp2 = $TAD(2) + $TAD(3);
068700080118
068800080118       ENDSR;
068900080117
069000080117       //--------------------------------------------------------------
069100080117       //?Operazioni finali.
069200080117       //--------------------------------------------------------------
069300080117       BEGSR RoutEnd;
069400080117
069500080118         kpjbu = FIDN00ds;
069600080117         return;
069700080117
069800080117       ENDSR;
069900080117
070000080117      /end-free
