000100080117      //---------------------------------------------------------------
000200130802      //?INVIO E-MAIL DI CONTESTAZIONE A VANDUUREN
000300080117      //---------------------------------------------------------------
000400080117      // Richiamato da FIDN03R x le C.A. con le segg. caratteristiche:
000500080117      // 1) Fase 100
000600130802      // 2) Bolla Export(§DCTTISP di DCTFLO = "E")
000700130802      // 3) EuroExpress (§DCTEURO di DCTFLO = "X")
000800130802      // 4) Responsabilità "PARTNER"
000900130802      // 5) LNA 320 e 325
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
002700130802     fFIDN39P   o    e             printer usropn
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
004100130802     d                                           file(FIDN39P) +
004200080117     d                                           ovrscope(*actgrpdfn) +
004300080117     d                                           ')
004400080117     d C_CmdDltOvr     c                   const('DLTOVR +
004500130802     d                                            file(FIDN39P) +
004600080117     d                                            lvl(*actgrpdfn)')
004700080117
004800080117      //---------------------------------------------------------------
004900080117      //?Definizione schiere.
005000080117      //---------------------------------------------------------------
005100080118
005200080117      // - Descrizione Tipo Anomalia
005300080117     d $TAD            s             35    dim( 3) inz                          descr. tipo anomalia
005400080117
005500080117      // - Note da stampare
005600080117     d $Note           s            105    dim( 5) inz                          $TAD x 5
005700080117
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
008300080117
008400080117      // - Flag operativi in FNDCT
008500080117     d dDCT01        e ds                  inz
008600080117
008700080117      // - Ricerca/Controllo tabelle
008800080117     d TIBS02ds      e ds                  inz
008900080117      // - tab. MRA/DAN = Bart-Maling - Danni
009000080117     d dMRAdan       e ds
009100080118      // - tab. STD = Dati Standard - Danni
009200080125      //d dSTD2         e ds
009300080117      // - tab. TAD = Tipi anomalia danni
009400080117     d dTAD          e ds                  inz
009500080117
009600080117      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
009700080117     d TRTCM1ds      e ds                  inz
009800080118     d   §CM1sts     e                     inz(*off)
009900080117
010000080117      // - Parametri x Controllo & Inversione date
010100080117     d WLBdat          ds                  inz
010200080117     d  G02dat                 1      8  0 inz
010300080117     d  G02inv                 9     16  0 inz
010400080117     d  G02err                17     17    inz('3')
010500080117     d  G02tgi                18     22  0 inz
010600080117
010700080117      //---------------------------------------------------------------
010800080117      //?Definizione variabili globali.
010900080117      //---------------------------------------------------------------
011000080118
011100080117      // - Flags booleani
011200080117     d W_Esito         s               n   inz(*off)
011300080117     d $Invio          s               n   inz(*off)
011400080117
011500080117      // - Indici di schiera
011600080118     d YY              s              3  0 inz
011700080117
011800080117      // - Campi per QCMDEXC
011900080117     d Qcmd            s            512    inz
012000080117
012100080117      // - Campi di comodo
012200080118     d Wtad            s                   like(DCTtad)   inz
012300080117
012400080117      //---------------------------------------------------------------
012500080117      //?Definizione procedure usate.
012600080117      //---------------------------------------------------------------
012700080118
012800080117      // - Reperimento dati utente
012900080117     d tibs34r         pr                  extpgm('TIBS34R')
013000080117     d  tibs34ds                           likeds(TIBS34ds)
013100080117
013200080117      // - Gestione tabelle
013300080117     d tibs02r         pr                  extpgm('TIBS02R')
013400080117     d  kpjba                              likeds(kpjba)    const
013500080117     d  tibs02ds                           likeds(TIBS02ds)
013600080117
013700080117      // - Controllo & Inversione date
013800080117     d xsrda8          pr                  extpgm('XSRDA8')
013900080117     d  wlbdat                             likeds(WLBdat)
014000080117
014100080117      // - Esecuzione comando di sistema
014200080117     d qCmdExc         pr                  extpgm('QCMDEXC')
014300080117     d  Qcmd                        512    const  options(*varsize)
014400080117     d  Qlen                         15  5 const
014500080117
014600080117      //---------------------------------------------------------------
014700080117      //?Definizione key-list.
014800080117      //---------------------------------------------------------------
014900080118
015000080118      // - File FNDCT01L
015100080117     d k03dct01      e ds                  extname(FNDCT01L:*key)
015200080118     d                                     inz
015300080118      // - File FNDCS01L
015400080118     d k03dcs01      e ds                  extname(FNDCS01L:*key)
015500080118     d                                     inz
015600080118      // - File FNDET01L
015700080118     d k02det01      e ds                  extname(FNDET01L:*key)
015800080118     d                                     inz
015900080125      // - File FNBLP01L / FNARB01L
016000080125     d k04blparb     e ds                  extname(FNBLP01L:*key)
016100080125     d                                     prefix(ARB:3)
016200080125     d                                     inz
016300080117
016400080117      //---------------------------------------------------------------
016500080117      //?Riepilogo indicatori.
016600080117      //---------------------------------------------------------------
016700080117      //---------------------------------------------------------------
016800080118
016900080118      //---------------------------------------------------------------
017000080118      //?M A I N - L I N E
017100080118      //---------------------------------------------------------------
017200080117
017300080117     c     *Entry        plist
017400080117     c                   parm                    KPJBA
017500080117
017600080117      /free
017700080117
017800080117       // Operazioni iniziali
017900080117       exsr RoutInz;
018000080117
018100080118       // Preparazione della e-mail da inviare
018200080118       if  O00err = *blank;
018300080118         exsr sr_PrintMail;
018400080117       endif;
018500080117
018600080117       // Operazioni finali
018700080117       exsr RoutEnd;
018800080117
018900080117       //--------------------------------------------------------------
019000080117       //?Operazioni iniziali.
019100080117       //--------------------------------------------------------------
019200080117       BEGSR RoutInz;
019300080118
019400080118       // Ricezione parametri
019500080118         FIDN00ds = kpjbu;
019600080118         clear o00f12;
019700080118         clear o00f03;
019800080118         clear o00err;
019900080118         clear o00msg;
020000080118
020100080118       // Reperimento dati job
020200080118         exsr DatiJob;
020300080118
020400080118       // Reperimento tab. "MRA"
020500080118         exsr rep_TabMRA;
020600080118         if  o00err = 'E';
020700080118           leavesr;
020800080118         endif;
020900080118
021000080118       // Reperimento tab. "STD2"
021100080125       //exsr rep_TabSTD2;
021200080125       //if  o00err = 'E';
021300080125       //  leavesr;
021400080125       //endif;
021500080117
021600080117       // Verifica parametri ricecvuti
021700080118         if  i00aac = *zero   or
021800080118             i00fil = *zero   or
021900080118             i00nca = *zero;
022000080118           o00err = 'E';
022100080118           o00msg = 'Manca il numero C.A.!!!';
022200080117           leavesr;
022300080117         endif;
022400080117
022500080117       // Reperimento dati testata C.A.
022600080118         exsr rep_FNDCT;
022700080118         if  o00err = 'E';
022800080117           leavesr;
022900080117         endif;
023000080118
023100080118       // Decodifica filiale gestione
023200080118         chain (i00fgs) AZORG;
023300080118         if  NOT  %found(AZORG01L);
023400080118           o00err = 'E';
023500080118           o00msg = 'Filiale gestione errata';
023600080118           leavesr;
023700080118         endif;
023800080118
023900080117       // Reperimento dati spedizione
024000080118         exsr rep_FNBLPARB;
024100080117
024200080117       ENDSR;
024300080118
024400080118       //--------------------------------------------------------------
024500080118       //?Reperimento Dati del job (Utente/Operativi).
024600080118       //--------------------------------------------------------------
024700080118       BEGSR DatiJob;
024800080118
024900080118         in(E) §AzUte;
025000080118         if NOT %error;
025100080118           in(E) §DatiUte;
025200080118         endif;
025300080118         if %error or RSut = *blanks;
025400080118           clear TIBS34ds;
025500080118           tibs34r(tibs34ds);
025600080118           in §AzUte;
025700080118           in §DatiUte;
025800080118         endif;
025900080118
026000080118       ENDSR;
026100080118
026200080118       //--------------------------------------------------------------
026300080118       //?Reperimento dati testata C.A.
026400080118       //--------------------------------------------------------------
026500080118       BEGSR rep_FNDCT;
026600080118
026700080118         DCTaac = i00aac;
026800080118         DCTfil = i00fil;
026900080118         DCTnca = i00nca;
027000080118
027100080118         chain %kds(k03dct01 : 3) FNDCT000;
027200080118
027300080118         if  NOT  %found(FNDCT01L)   or
027400080118             DCTatb <> *blank;
027500080118           o00err = 'E';
027600080118           o00msg = 'C.A. non reperita';
027700080118           leavesr;
027800080118         endif;
027900080118
028000080118         dDCT01 = DCTflo;
028100080118
028200080118         if  DCTfca   <> C_fca_con   or
028300130802             §DCTtisp <> 'E'         or
028400130802             §DCTeuro <> 'X'         or
028500130802             (DCTlna  <> 320 and DCTlna <> 325) or
028600080118             DCTres   <> 'P';
028700080118           o00err = 'E';
028800130802           o00msg = 'C.A. non contestabile a VanDuuren';
028900080118           leavesr;
029000080118         endif;
029100080118
029200080118       // Impostazione numero spedizione tra i parms SE non ricevuto
029300080118         if  i00aas = *zero   or
029400080118             i00lnp = *zero   or
029500080118             i00nsp = *zero;
029600080118           i00aas = DCTaas;
029700080118           i00lnp = DCTlnp;
029800080118           i00nrs = DCTnrs;
029900080118           i00nsp = DCTnsp;
030000080118         endif;
030100080118
030200080118       // Impostazione altri dati tra i parms SE non ricevuti
030300080118         if  i00fca = *zero;
030400080118           i00fca = DCTfca;
030500080118         endif;
030600080118         if  i00fgs = *zero;
030700080118           i00fgs = DCTgfc;
030800080118         endif;
030900080118         if  i00tad = *blank;
031000080118           i00tad = DCTtad;
031100080118         endif;
031200080118
031300080118       ENDSR;
031400080118
031500080118       //--------------------------------------------------------------
031600080118       //?Reperimento dati spedizione
031700080118       //--------------------------------------------------------------
031800080118       BEGSR rep_FNBLPARB;
031900080118
032000080118         IF  i00tpb = 'P';
032100080118
032200080118           ARBaas = i00aas;
032300080118           ARBlnp = i00lnp;
032400080118           ARBnrs = i00nrs;
032500080118           ARBnsp = i00nsp;
032600080118           chain %kds(k04blparb : 4) FNBLP000;
032700080118
032800080118           if  NOT  %found(FNBLP01L)   or
032900080118               ARBatb <> *blank;
033000080118             o00err = 'E';
033100080118             o00msg = 'Spedizione non reperita';
033200080118             leavesr;
033300080118           endif;
033400080118
033500080118         ELSE;
033600080118
033700080118           chain %kds(k04blparb : 4) FNARB000;
033800080118
033900080118           if  NOT  %found(FNARB01L)   or
034000080118               ARBatb <> *blank;
034100080118             o00err = 'E';
034200080118             o00msg = 'Spedizione non reperita';
034300080118             leavesr;
034400080118           endif;
034500080118
034600080118         ENDIF;
034700080118
034800080118       ENDSR;
034900080118
035000080118       //--------------------------------------------------------------
035100080118       //?Reperimento tab. MRA = Bart-Maling - Danni
035200080118       //--------------------------------------------------------------
035300080118       BEGSR rep_TabMRA;
035400080118
035500080118         clear dMRAdan;
035600080118         clear TIBS02ds;
035700080118
035800080118         T02mod = 'C';
035900080118         T02sif = knsif;
036000080118         T02cod = 'MRA';
036100130802         T02ke1 = %trimr(SDSpgm);
036200080118         T02ke2 = 'DAN';
036300080118
036400080118         tibs02r( kpjba : tibs02ds );
036500080118
036600080118         if T02err = *blanks;
036700080118           dMRAdan = T02uni;
036800080118         else;
036900080118           o00err = 'E';
037000080118           o00msg = 'Manca tab. MRA'
037100080118                  + ' "' + %trimr(T02ke1) + '" /'
037200080118                  + ' "' + %trimr(T02ke2) + '"';
037300080118           leavesr;
037400080118         endif;
037500080118
037600080118       ENDSR;
037700080118
037800080118       //--------------------------------------------------------------
037900080118       //?Reperimento tab. TAD = Tipo Anomalia
038000080118       //--------------------------------------------------------------
038100080118       BEGSR rep_TabTAD;
038200080118
038300080118         clear dTAD;
038400080118         clear TIBS02ds;
038500080118
038600080118         T02mod = 'C';
038700080118         T02sif = knsif;
038800080118         T02cod = 'TAD';
038900080118         T02ke1 = Wtad;
039000080118
039100080118         tibs02r( kpjba : tibs02ds );
039200080118
039300080118         if T02err = *blanks;
039400080118           dTAD = T02uni;
039500080118         endif;
039600080118
039700080118       ENDSR;
039800080117
039900080117       //--------------------------------------------------------------
040000080118       //?Stampa della lettera da inviare via e-mail.
040100080117       //--------------------------------------------------------------
040200080118       BEGSR sr_PrintMail;
040300080118
040400080118       // Override al file di stampa ed apertura dello stesso
040500080118         exsr sr_Override;
040600080117
040700080117       // Impostazione dei campi in stampa
040800080117         exsr sr_RieField;
040900080117
041000080117       // Impostazione indicatori utilizzati nel PrtF
041100080117         *in42  = (PT1da2 <> *blanks);
041200080117         *in43  = (PT1qtd <> *blanks);
041300080117         *in44  = (PT1dp2 <> *blanks);
041400080117         *in47  = (ARBrma <> *blanks);
041500080117
041600080117       // Stampa
041700130802         write DN39T01;
041800080117
041900080117       // Chiusura file stampa & Cancellazione override
042000080117         exsr sr_DeleteOvr;
042100080117
042200080117       ENDSR;
042300080117
042400080117       //--------------------------------------------------------------
042500080117       //?Esecuzione override ed apertura del file di stampa.
042600080117       //--------------------------------------------------------------
042700080117       BEGSR sr_Override;
042800080117
042900080117         reset $Invio;
043000080117         reset TRTCM1ds;
043100080117
043200080118         if  §MRADreg <> *blank;
043300080121           §CM1mitt = %trim(§MRADmitt);
043400080117           §CM1dst  = %trim(§MRADdest);
043500080117           §CM1tips = §MRADreg;
043600080118           §CM1po   = %trim( %editw( DUTpou : '0   ' ));
043700080125           §CM1var  = '*OBJM*'
043800130802                    + 'Damage comunication n. '
043900080125                    + %editw(I00fil : '0   ')
044000080125                    + '/'
044100080125                    + %trim( %editc( I00nca : 'Z' ))
044200080125                    + ' '
044300080125                    + %subst( %char( I00aac ) : 3 : 2 );
044400080117           §CM1idp  = §MRADidpro;
044500080117           Qcmd = C_CmdOvrPrtF
044600080117                + ' outq(' + %trim(§MRADoutqi) + ')'
044700080117                + ' usrdfndta(''' + TRTCM1ds + ''')';
044800080122           callp(e) qCmdExc(Qcmd : %size(Qcmd));
044900080122           if  not %error;
045000080122             $Invio = *on;
045100080122           endif;
045200080118         endif;
045300080117
045400130802         open FIDN39P;
045500080117
045600080117       ENDSR;
045700080117
045800080117       //--------------------------------------------------------------
045900080117       //?Impostazione dei campi in stampa
046000080117       //--------------------------------------------------------------
046100080117       BEGSR sr_RieField;
046200080117
046300130802         clear DN39T01;
046400080118
046500080118       // Intestazione destinatario
046600130806         PT1rgs = 'Van Duuren Districenters';
046700080118
046800080118       // Località + data
046900080121         if  ORGfl1 = 'E'  and  ORGfva = *blank;
047000080118           PT1dal = 'MILANO, ' + %editc(*date : 'Y');
047100080121         else;
047200080118           PT1dal = %trim(ORGdes) + ', ' + %editc(*date : 'Y');
047300080121         endif;
047400080118
047500080118       // Oggetto
047600080118         PT1nca = %trim( %editw( I00fil:'0   ' )) + '/'
047700080118                + %trim( %editc( I00nca:'Z' ));
047800080118         PT1aac = I00aac;
047900080118
048000080118       // Dati relativi alla spedizione
048100080118         PT1lnp = I00lnp;
048200080118         PT1nrs = I00nrs;
048300080118         PT1nsp = I00nsp;
048400080118         reset WLBdat;
048500080118         G02inv = (I00aas * 10000) + ARBmgs;
048600080118         xsrda8(wlbdat);
048700080118         PT1dat = G02dat;
048800080118         PT1ncl = ARBncl;
048900080118         PT1pkf = ARBpkf;
049000080118
049100080118       // Dati relativi al mittente
049200080118         PT1rsm = ARBrsm;
049300080118         PT1rmn = ARBrmn;
049400080118         PT1rma = ARBrma;
049500080118
049600080118       // dati relativi al destinatario
049700080118         PT1rsd = ARBrsd;
049800080118         PT1lod = ARBlod;
049900080118         if  ARBnzd <> *blank;
050000080118           PT1prn = ARBnzd;
050100080118         else;
050200080118           PT1prn = ARBprd;
050300080118         endif;
050400080118
050500080118       // Anomalia
050600080118         Wtad = I00tad;
050700080118         exsr rep_TabTAD;
050800130805         SELECT;
050900130805         WHEN §TADdesi <> *blank;
051000130805           PT1tad = §TADdesi;
051100130805         WHEN §TADdest <> *blank;
051200080121           PT1tad = §TADdest;
051300130805         OTHER;
051400080121           PT1tad = §TADdesc;
051500130805         ENDSL;
051600080121       // Descrizione anomalia
051700080121         exsr Rep_DesTAD;
051800080118
051900080118       // Evento
052000080121         IF  DCTnev > *zero;
052100080118           PT1kev = %trim( %editw( DCTaae : '0    ' )) + '/'
052200080118                  + %trim( %editc( DCTnev : 'Z' ));
052300080118           chain (DCTaae : DCTnev) FNDET000;
052400080118           if  %found(FNDET01L);
052500080118             Wtad   = DETtad;
052600080118             exsr rep_TabTAD;
052700080121             if  §TADdest <> *blank;
052800080118               PT1tae = §TADdest;
052900080121             else;
053000080118               PT1tae = §TADdesc;
053100080121             endif;
053200080121           endif;
053300080121         ENDIF;
053400080118
053500080118       // Quantità danneggiate
053600080118       // - Peso
053700080121         if  DCTpkd > *zero;
053800080118           PT1qtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
053900080121         endif;
054000080118       // - Colli
054100080121         if  DCTncn > *zero;
054200080118           PT1qtd = %trim(PT1qtd)
054300130805                  + ' Parcels ' + %trim( %editc( DCTncn : 'Z' ));
054400080121         endif;
054500080118       // - Pezzi
054600080118       //   + Descrizione pezzi danneggiati
054700080121         if  DCTnpz > *zero;
054800080118           PT1qtd  = %trim(PT1qtd)
054900130805                   + ' Pieces ' + %trim( %editc( DCTnpz : 'Z' ));
055000080118           exsr Rep_DesPZD;
055100080121         endif;
055200080117
055300080117       ENDSR;
055400080117
055500080117       //--------------------------------------------------------------
055600080117       //?Chiusura file stampa & Cancellazione override
055700080117       //--------------------------------------------------------------
055800080117       BEGSR sr_DeleteOvr;
055900080117
056000130802         if  %open(FIDN39P);
056100130802           close FIDN39P;
056200080117           callp(e) qCmdExc(C_CmdDltOvr : %size(C_CmdDltOvr));
056300080117         endif;
056400080117
056500080117       ENDSR;
056600080118
056700080118       //--------------------------------------------------------------
056800080118       //?Reperimento descrizione anomalia
056900080118       //--------------------------------------------------------------
057000080118       BEGSR rep_DesTAD;
057100080118
057200080118         clear YY;
057300080118         clear $TAD;
057400080118
057500080118         DCStpd = 'C';
057600080118         DCSnkt = Wnumca;
057700080118         DCSstd = 'A';
057800080118
057900080118         setll %kds(k03dcs01 : 3) FNDCS000;
058000080118         reade %kds(k03dcs01 : 3) FNDCS000;
058100080118
058200080121         dow  NOT  %eof(FNDCS01L)   and
058300080118              YY < %elem($TAD);
058400080118           yy += 1;
058500080118           $TAD(yy) = DCSnot;
058600080118           reade %kds(k03dcs01 : 3) FNDCS000;
058700080121         enddo;
058800080118
058900080118         PT1da1 = $TAD(1);
059000080118         PT1da2 = $TAD(2) + $TAD(3);
059100080118
059200080118       ENDSR;
059300080118
059400080118       //--------------------------------------------------------------
059500080118       //?Reperimento descrizione pezzi danneggiati/mancanti
059600080118       //--------------------------------------------------------------
059700080118       BEGSR rep_DesPZD;
059800080118
059900080118         clear YY;
060000080118         clear $TAD;
060100080118
060200080118         DCStpd = 'C';
060300080118         DCSnkt = Wnumca;
060400080118         DCSstd = 'P';
060500080118
060600080118         setll %kds(k03dcs01 : 3) FNDCS000;
060700080118         reade %kds(k03dcs01 : 3) FNDCS000;
060800080118
060900080121         dow  NOT  %eof(FNDCS01L)   and
061000080118              YY < %elem($TAD);
061100080118           yy += 1;
061200080118           $TAD(yy) = DCSnot;
061300080118           reade %kds(k03dcs01 : 3) FNDCS000;
061400080121         enddo;
061500080118
061600080118         PT1dpz = $TAD(1);
061700080118         PT1dp2 = $TAD(2) + $TAD(3);
061800080118
061900080118       ENDSR;
062000080117
062100080117       //--------------------------------------------------------------
062200080117       //?Operazioni finali.
062300080117       //--------------------------------------------------------------
062400080117       BEGSR RoutEnd;
062500080117
062600080118         kpjbu = FIDN00ds;
062700080117         return;
062800080117
062900080117       ENDSR;
063000080117
063100080117      /end-free
