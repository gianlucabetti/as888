000100080117      //---------------------------------------------------------------
000200130802      //?INVIO E-MAIL DI CONTESTAZIONE A VANDUUREN
000300080117      //---------------------------------------------------------------
000400160817      //?Richiamato da FIDN03R & FIDN78R x le C.A. con le seguenti    ?
000500160817      //?caratteristiche:                                             ?
000600160817      //?1) Fase 100                                                  ?
000700160817      //?2) Bolla Export(§DCTTISP di DCTFLO = "E")                    ?
000800160817      //?3) EuroExpress (§DCTEURO di DCTFLO = "X")                    ?
000900160817      //?4) Responsabilità "PARTNER"                                  ?
001000160817      //?5) LNA 320 e 325                                             ?
001100080117      //---------------------------------------------------------------
001200160831
001300160831       //--------------------------------------------------------------
001400160831       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001500160831       //--------------------------------------------------------------
001600160831
001700160831     /*PRM  dbgview(*source)
001800160831     /*CMD  ovrdbf file(FNARB01L) tofile(FILTRAPRD/FNARB01L) +
001900160831     /*CMD         ovrscope(*calllvl)
002000160831     /*CMD  ovrdbf file(FNBLP01L) tofile(FILTRAPRD/FNBLP01L) +
002100160831     /*CMD         ovrscope(*calllvl)
002200160831     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
002300160831     /*CMD         ovrscope(*calllvl)
002400160831     /*CMD  ovrdbf file(TITAA30C) tofile(GAITRAGRPS/TITAA30C) +
002500160831     /*CMD         ovrscope(*calllvl)
002600160831     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
002700160831     /*CMD         ovrscope(*calllvl)
002800160831     /*END  dltovr file(*all) lvl(*)
002900160831     /*END
003000160831
003100160831       //--------------------------------------------------------------
003200160831       //?Specifiche di controllo.                                     ?
003300160831       //--------------------------------------------------------------
003400080117
003500080117     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003600080117
003700080117      //---------------------------------------------------------------
003800080117      //?Dichiarazione file.
003900080117      //---------------------------------------------------------------
004000080118
004100080117     fFNDCT01L  if   e           k disk
004200080117     fFNDET01L  if   e           k disk
004300080117     fFNDCS01L  if   e           k disk
004400080117
004500160831     fFNARB01L  if   e           k disk    usropn
004600160831     fFNBLP01L  if   e           k disk    usropn
004700160831     f                                     prefix(ARB:3)
004800160831     fTITAS30C  if   e           k disk    usropn
004900160831     fTITAA30C  if   e           k disk    usropn
005000160831     fTITA430C  if   e           k disk    usropn
005100080117
005200080117     fAZORG01L  if   e           k disk
005300080117
005400130802     fFIDN39P   o    e             printer usropn
005500080117     f                                     OFlind(*in01)
005600080117
005700080117      //---------------------------------------------------------------
005800080117      //?Definizione costanti.
005900080117      //---------------------------------------------------------------
006000080118
006100080117      // - Fase "Conferma C.A."
006200080117     d C_fca_con       c                   const(100)
006300080117
006400080117      // - Dati di default
006500080117
006600080117      // - Comandi di override al PrtF
006700080117     d C_CmdOvrPrtF    c                   const('OVRPRTF +
006800130802     d                                           file(FIDN39P) +
006900080117     d                                           ovrscope(*actgrpdfn) +
007000080117     d                                           ')
007100080117     d C_CmdDltOvr     c                   const('DLTOVR +
007200130802     d                                            file(FIDN39P) +
007300080117     d                                            lvl(*actgrpdfn)')
007400080117
007500080117      //---------------------------------------------------------------
007600080117      //?Definizione schiere.
007700080117      //---------------------------------------------------------------
007800080118
007900080117      // - Descrizione Tipo Anomalia
008000080117     d $TAD            s             35    dim( 3) inz                          descr. tipo anomalia
008100080117
008200080117      // - Note da stampare
008300080117     d $Note           s            105    dim( 5) inz                          $TAD x 5
008400080117
008500080117      //---------------------------------------------------------------
008600080117      //?Definizione aree dati.
008700080117      //---------------------------------------------------------------
008800080118
008900080117      // - Dati utente
009000080117     d §AzUte        e ds                  extname(AZUTE00F)
009100080117     d                                     dtaara
009200080117     d §DatiUte      e ds                  extname(dDatiUte)
009300080117     d                                     dtaara
009400080117
009500080117      //---------------------------------------------------------------
009600080117      //?Definizione strutture dati.
009700080117      //---------------------------------------------------------------
009800080118
009900080117      // - Status
010000080117     d Psds           sds
010100080117     d   SDSpgm          *proc
010200080117
010300080117      // - Parametri ricevuti
010400080117     d KPJBA         e ds
010500080117     d FIDN00ds      e ds
010600080117     d   Wnumca                6     19
010700080117
010800080117      // - Reperimento dati utente
010900080117     d TIBS34ds      e ds
011000080117
011100080117      // - Flag operativi in FNDCT
011200080117     d dDCT01        e ds                  inz
011300160831
011400160831       // -?TITA4 tipo record "A"?
011500160831     d dTA4a         e ds                  inz   qualified
011600080117
011700080117      // - Ricerca/Controllo tabelle
011800080117     d TIBS02ds      e ds                  inz
011900080117      // - tab. MRA/DAN = Bart-Maling - Danni
012000080117     d dMRAdan       e ds
012100080118      // - tab. STD = Dati Standard - Danni
012200080125      //d dSTD2         e ds
012300080117      // - tab. TAD = Tipi anomalia danni
012400080117     d dTAD          e ds                  inz
012500080117
012600080117      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
012700080117     d TRTCM1ds      e ds                  inz
012800080118     d   §CM1sts     e                     inz(*off)
012900080117
013000080117      // - Parametri x Controllo & Inversione date
013100080117     d WLBdat          ds                  inz
013200080117     d  G02dat                 1      8  0 inz
013300080117     d  G02inv                 9     16  0 inz
013400080117     d  G02err                17     17    inz('3')
013500080117     d  G02tgi                18     22  0 inz
013600080117
013700080117      //---------------------------------------------------------------
013800080117      //?Definizione variabili globali.
013900080117      //---------------------------------------------------------------
014000080118
014100080117      // - Flags booleani
014200080117     d W_Esito         s               n   inz(*off)
014300080117     d $Invio          s               n   inz(*off)
014400080117
014500080117      // - Indici di schiera
014600080118     d YY              s              3  0 inz
014700080117
014800080117      // - Campi per QCMDEXC
014900080117     d Qcmd            s            512    inz
015000080117
015100080117      // - Campi di comodo
015200080118     d Wtad            s                   like(DCTtad)   inz
015300080117
015400080117      //---------------------------------------------------------------
015500080117      //?Definizione procedure usate.
015600080117      //---------------------------------------------------------------
015700080118
015800080117      // - Reperimento dati utente
015900080117     d tibs34r         pr                  extpgm('TIBS34R')
016000080117     d  tibs34ds                           likeds(TIBS34ds)
016100080117
016200080117      // - Gestione tabelle
016300080117     d tibs02r         pr                  extpgm('TIBS02R')
016400080117     d  kpjba                              likeds(kpjba)    const
016500080117     d  tibs02ds                           likeds(TIBS02ds)
016600080117
016700080117      // - Controllo & Inversione date
016800080117     d xsrda8          pr                  extpgm('XSRDA8')
016900080117     d  wlbdat                             likeds(WLBdat)
017000080117
017100080117      // - Esecuzione comando di sistema
017200080117     d qCmdExc         pr                  extpgm('QCMDEXC')
017300080117     d  Qcmd                        512    const  options(*varsize)
017400080117     d  Qlen                         15  5 const
017500160831
017600160831       // -?Controllo/Decodifica cliente?
017700160831      /copy gaitrasrc/srcProtoPI,TIBS69R
017800160831      /copy gaitrasrc/srcProtoPR,TIBS69R
017900080117
018000080117      //---------------------------------------------------------------
018100080117      //?Definizione key-list.
018200080117      //---------------------------------------------------------------
018300080118
018400080118      // - File FNDCT01L
018500080117     d k03dct01      e ds                  extname(FNDCT01L:*key)
018600080118     d                                     inz
018700080118      // - File FNDCS01L
018800080118     d k03dcs01      e ds                  extname(FNDCS01L:*key)
018900080118     d                                     inz
019000080118      // - File FNDET01L
019100080118     d k02det01      e ds                  extname(FNDET01L:*key)
019200080118     d                                     inz
019300080125      // - File FNBLP01L / FNARB01L
019400080125     d k04blparb     e ds                  extname(FNBLP01L:*key)
019500080125     d                                     prefix(ARB:3)
019600080125     d                                     inz
019700160831
019800160831       // -?File TITAS30C?
019900160831     d keyTITAS30    e ds                  extname( TITAS30C : *key )
020000160831     d                                     prefix( k_ )  inz
020100160831       // -?File TITAA30C?
020200160831     d keyTITAA30    e ds                  extname( TITAA30C : *key )
020300160831     d                                     prefix( k_ )  inz
020400160831       // -?File TITA430C?
020500160831     d keyTITA430    e ds                  extname( TITA430C : *key )
020600160831     d                                     prefix( k_ )  inz
020700080117
020800080117      //---------------------------------------------------------------
020900080117      //?Riepilogo indicatori.
021000080117      //---------------------------------------------------------------
021100080117      //---------------------------------------------------------------
021200080118
021300080118      //---------------------------------------------------------------
021400080118      //?M A I N - L I N E
021500080118      //---------------------------------------------------------------
021600080117
021700080117     c     *Entry        plist
021800080117     c                   parm                    KPJBA
021900080117
022000080117      /free
022100080117
022200080117       // Operazioni iniziali
022300080117       exsr RoutInz;
022400080117
022500080118       // Preparazione della e-mail da inviare
022600080118       if  O00err = *blank;
022700080118         exsr sr_PrintMail;
022800080117       endif;
022900080117
023000080117       // Operazioni finali
023100080117       exsr RoutEnd;
023200080117
023300080117       //--------------------------------------------------------------
023400080117       //?Operazioni iniziali.
023500080117       //--------------------------------------------------------------
023600080117       BEGSR RoutInz;
023700080118
023800080118       // Ricezione parametri
023900080118         FIDN00ds = kpjbu;
024000080118         clear o00f12;
024100080118         clear o00f03;
024200080118         clear o00err;
024300080118         clear o00msg;
024400080118
024500080118       // Reperimento dati job
024600080118         exsr DatiJob;
024700080118
024800080118       // Reperimento tab. "MRA"
024900080118         exsr rep_TabMRA;
025000080118         if  o00err = 'E';
025100080118           leavesr;
025200080118         endif;
025300080118
025400080118       // Reperimento tab. "STD2"
025500080125       //exsr rep_TabSTD2;
025600080125       //if  o00err = 'E';
025700080125       //  leavesr;
025800080125       //endif;
025900080117
026000080117       // Verifica parametri ricecvuti
026100080118         if  i00aac = *zero   or
026200080118             i00fil = *zero   or
026300080118             i00nca = *zero;
026400080118           o00err = 'E';
026500080118           o00msg = 'Manca il numero C.A.!!!';
026600080117           leavesr;
026700080117         endif;
026800080117
026900080117       // Reperimento dati testata C.A.
027000080118         exsr rep_FNDCT;
027100080118         if  o00err = 'E';
027200080117           leavesr;
027300080117         endif;
027400080118
027500080118       // Decodifica filiale gestione
027600080118         chain (i00fgs) AZORG;
027700080118         if  NOT  %found(AZORG01L);
027800080118           o00err = 'E';
027900080118           o00msg = 'Filiale gestione errata';
028000080118           leavesr;
028100080118         endif;
028200080118
028300080117       // Reperimento dati spedizione
028400160831         if  I00tpb = 'S';
028500160831           open  TITAS30C;
028600160831           open  TITAA30C;
028700160831           open  TITA430C;
028800160831           exsr  sr_rep_TITAS;
028900160831         else;
029000160831           open  FNARB01L;
029100160831           open  FNBLP01L;
029200160831           exsr rep_FNBLPARB;
029300160831         endif;
029400080117
029500080117       ENDSR;
029600080118
029700080118       //--------------------------------------------------------------
029800080118       //?Reperimento Dati del job (Utente/Operativi).
029900080118       //--------------------------------------------------------------
030000080118       BEGSR DatiJob;
030100080118
030200080118         in(E) §AzUte;
030300080118         if NOT %error;
030400080118           in(E) §DatiUte;
030500080118         endif;
030600080118         if %error or RSut = *blanks;
030700080118           clear TIBS34ds;
030800080118           tibs34r(tibs34ds);
030900080118           in §AzUte;
031000080118           in §DatiUte;
031100080118         endif;
031200080118
031300080118       ENDSR;
031400080118
031500080118       //--------------------------------------------------------------
031600080118       //?Reperimento dati testata C.A.
031700080118       //--------------------------------------------------------------
031800080118       BEGSR rep_FNDCT;
031900080118
032000080118         DCTaac = i00aac;
032100080118         DCTfil = i00fil;
032200080118         DCTnca = i00nca;
032300080118
032400080118         chain %kds(k03dct01 : 3) FNDCT000;
032500080118
032600080118         if  NOT  %found(FNDCT01L)   or
032700080118             DCTatb <> *blank;
032800080118           o00err = 'E';
032900080118           o00msg = 'C.A. non reperita';
033000080118           leavesr;
033100080118         endif;
033200080118
033300080118         dDCT01 = DCTflo;
033400080118
033500160830         if  ( DCTfca <> c_FCA_Con   and
033600160830               I00dsb <= *zero )     or
033700130802             §DCTtisp <> 'E'         or
033800130802             §DCTeuro <> 'X'         or
033900130802             (DCTlna  <> 320 and DCTlna <> 325) or
034000080118             DCTres   <> 'P';
034100080118           o00err = 'E';
034200130802           o00msg = 'C.A. non contestabile a VanDuuren';
034300080118           leavesr;
034400080118         endif;
034500080118
034600080118       // Impostazione numero spedizione tra i parms SE non ricevuto
034700080118         if  i00aas = *zero   or
034800080118             i00lnp = *zero   or
034900080118             i00nsp = *zero;
035000080118           i00aas = DCTaas;
035100080118           i00lnp = DCTlnp;
035200080118           i00nrs = DCTnrs;
035300080118           i00nsp = DCTnsp;
035400080118         endif;
035500080118
035600080118       // Impostazione altri dati tra i parms SE non ricevuti
035700080118         if  i00fca = *zero;
035800080118           i00fca = DCTfca;
035900080118         endif;
036000080118         if  i00fgs = *zero;
036100080118           i00fgs = DCTgfc;
036200080118         endif;
036300080118         if  i00tad = *blank;
036400080118           i00tad = DCTtad;
036500080118         endif;
036600080118
036700080118       ENDSR;
036800160831
036900160831       //--------------------------------------------------------------
037000160831       //?Reperimento dati spedizione di Sede.                         ?
037100160831       //--------------------------------------------------------------
037200160831       BEGSR  sr_Rep_TITAS;
037300160831
037400160831         clear  keyTITAS30;
037500160831         k_TASaas = I00aas;
037600160831         k_TASlnp = I00lnp;
037700160831         k_TASnrs = I00nrs;
037800160831         k_TASnsp = I00nsp;
037900160831         chain  %kds( keyTITAS30 : 4 )  TITAS30C;
038000160831         if  NOT  %found( TITAS30C );
038100160831           o00err = 'E';
038200160831           o00msg = 'Spedizione non reperita';
038300160831           leavesr;
038400160831         endif;
038500160831
038600160831         // -?Decodifica Mittente?
038700160831         // ·?SE non codificato: aggancia TITAA?
038800160831         IF  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
038900160831             %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
039000160831
039100160831           clear  keyTITAA30;
039200160831           k_TAAaas = I00aas;
039300160831           k_TAAlnp = I00lnp;
039400160831           k_TAAnrs = I00nrs;
039500160831           k_TAAnsp = I00nsp;
039600160831           k_TAAtrc = 'M';
039700160831           chain  %kds( keyTITAA30 )  TITAA30C;
039800160831           if  NOT %found( TITAA30C );
039900160831             o00err = 'E';
040000160831             o00msg = 'Mittente non reperito';
040100160831             leavesr;
040200160831           endif;
040300160831
040400160831         // ·?SE codificato: aggancia Anagrafica?
040500160831         ELSE;
040600160831
040700160831           clear  TIBS69ds;
040800160831           I69sif = knsif;
040900160831           I69kcc = DUTkci;
041000160831           I69kac = TASccm;
041100160831           tibs69r( TIBS69ds :
041200160831                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
041300160831           If  O69err <> *blank;
041400160831             o00err = 'E';
041500160831             o00msg = 'Mittente non reperito';
041600160831             leavesr;
041700160831           EndIf;
041800160831
041900160831         ENDIF;
042000160831
042100160831         // -?Reperimento dati dal *file TITA4 / trk "A"?
042200160831         //  ?(Rif.Mitt.Alfanum.)?
042300160831         clear  dTA4a;
042400160831         clear  keyTITA430;
042500160831         k_TA4aas = I00aas;
042600160831         k_TA4lnp = I00lnp;
042700160831         k_TA4nrs = I00nrs;
042800160831         k_TA4nsp = I00nsp;
042900160831         k_TA4trc = 'A';
043000160831         chain  %kds( keyTITA430 )  TITA430C;
043100160831         if  %found( TITA430C );
043200160831           dTA4a   = TA4not;
043300160831         endif;
043400160831
043500160831       ENDSR;
043600080118
043700080118       //--------------------------------------------------------------
043800160831       //?Reperimento dati spedizione di filiale
043900080118       //--------------------------------------------------------------
044000080118       BEGSR rep_FNBLPARB;
044100080118
044200080118         IF  i00tpb = 'P';
044300080118
044400080118           ARBaas = i00aas;
044500080118           ARBlnp = i00lnp;
044600080118           ARBnrs = i00nrs;
044700080118           ARBnsp = i00nsp;
044800080118           chain %kds(k04blparb : 4) FNBLP000;
044900080118
045000080118           if  NOT  %found(FNBLP01L)   or
045100080118               ARBatb <> *blank;
045200080118             o00err = 'E';
045300080118             o00msg = 'Spedizione non reperita';
045400080118             leavesr;
045500080118           endif;
045600080118
045700080118         ELSE;
045800080118
045900080118           chain %kds(k04blparb : 4) FNARB000;
046000080118
046100080118           if  NOT  %found(FNARB01L)   or
046200080118               ARBatb <> *blank;
046300080118             o00err = 'E';
046400080118             o00msg = 'Spedizione non reperita';
046500080118             leavesr;
046600080118           endif;
046700080118
046800080118         ENDIF;
046900080118
047000080118       ENDSR;
047100080118
047200080118       //--------------------------------------------------------------
047300080118       //?Reperimento tab. MRA = Bart-Maling - Danni
047400080118       //--------------------------------------------------------------
047500080118       BEGSR rep_TabMRA;
047600080118
047700080118         clear dMRAdan;
047800080118         clear TIBS02ds;
047900080118
048000080118         T02mod = 'C';
048100080118         T02sif = knsif;
048200080118         T02cod = 'MRA';
048300130802         T02ke1 = %trimr(SDSpgm);
048400080118         T02ke2 = 'DAN';
048500080118
048600080118         tibs02r( kpjba : tibs02ds );
048700080118
048800080118         if T02err = *blanks;
048900080118           dMRAdan = T02uni;
049000080118         else;
049100080118           o00err = 'E';
049200080118           o00msg = 'Manca tab. MRA'
049300080118                  + ' "' + %trimr(T02ke1) + '" /'
049400080118                  + ' "' + %trimr(T02ke2) + '"';
049500080118           leavesr;
049600080118         endif;
049700080118
049800080118       ENDSR;
049900080118
050000080118       //--------------------------------------------------------------
050100080118       //?Reperimento tab. TAD = Tipo Anomalia
050200080118       //--------------------------------------------------------------
050300080118       BEGSR rep_TabTAD;
050400080118
050500080118         clear dTAD;
050600080118         clear TIBS02ds;
050700080118
050800080118         T02mod = 'C';
050900080118         T02sif = knsif;
051000080118         T02cod = 'TAD';
051100080118         T02ke1 = Wtad;
051200080118
051300080118         tibs02r( kpjba : tibs02ds );
051400080118
051500080118         if T02err = *blanks;
051600080118           dTAD = T02uni;
051700080118         endif;
051800080118
051900080118       ENDSR;
052000080117
052100080117       //--------------------------------------------------------------
052200080118       //?Stampa della lettera da inviare via e-mail.
052300080117       //--------------------------------------------------------------
052400080118       BEGSR sr_PrintMail;
052500080118
052600080118       // Override al file di stampa ed apertura dello stesso
052700080118         exsr sr_Override;
052800080117
052900080117       // Impostazione dei campi in stampa
053000080117         exsr sr_RieField;
053100080117
053200080117       // Impostazione indicatori utilizzati nel PrtF
053300080117         *in42  = (PT1da2 <> *blanks);
053400080117         *in43  = (PT1qtd <> *blanks);
053500080117         *in44  = (PT1dp2 <> *blanks);
053600080117         *in47  = (ARBrma <> *blanks);
053700080117
053800080117       // Stampa
053900130802         write DN39T01;
054000080117
054100080117       // Chiusura file stampa & Cancellazione override
054200080117         exsr sr_DeleteOvr;
054300080117
054400080117       ENDSR;
054500080117
054600080117       //--------------------------------------------------------------
054700080117       //?Esecuzione override ed apertura del file di stampa.
054800080117       //--------------------------------------------------------------
054900080117       BEGSR sr_Override;
055000080117
055100080117         reset $Invio;
055200080117         reset TRTCM1ds;
055300080117
055400160817         if  I00mod   <> 'R'     and
055500160817             §MRADreg <> *blank;
055600080121           §CM1mitt = %trim(§MRADmitt);
055700080117           §CM1dst  = %trim(§MRADdest);
055800080117           §CM1tips = §MRADreg;
055900080118           §CM1po   = %trim( %editw( DUTpou : '0   ' ));
056000080125           §CM1var  = '*OBJM*'
056100130802                    + 'Damage comunication n. '
056200080125                    + %editw(I00fil : '0   ')
056300080125                    + '/'
056400080125                    + %trim( %editc( I00nca : 'Z' ))
056500080125                    + ' '
056600080125                    + %subst( %char( I00aac ) : 3 : 2 );
056700080117           §CM1idp  = §MRADidpro;
056800080117           Qcmd = C_CmdOvrPrtF
056900080117                + ' outq(' + %trim(§MRADoutqi) + ')'
057000080117                + ' usrdfndta(''' + TRTCM1ds + ''')';
057100080122           callp(e) qCmdExc(Qcmd : %size(Qcmd));
057200080122           if  not %error;
057300080122             $Invio = *on;
057400080122           endif;
057500080118         endif;
057600080117
057700130802         open FIDN39P;
057800080117
057900080117       ENDSR;
058000080117
058100080117       //--------------------------------------------------------------
058200080117       //?Impostazione dei campi in stampa
058300080117       //--------------------------------------------------------------
058400080117       BEGSR sr_RieField;
058500080117
058600130802         clear DN39T01;
058700080118
058800080118       // Intestazione destinatario
058900130806         PT1rgs = 'Van Duuren Districenters';
059000160817
059100160817         // -?Data (in formato gg/mm/aaaa)?
059200160830         //*·if  I00dsb <= *zero;
059300160830         //*·  I00dsb = %int( %subst( %char( %dec( %timestamp() ) )
059400160830         //*·                         : 1 : 8 ) );
059500160830         //*·endif;
059600160817         reset WLBdat;
059700160830         if  I00dsb > *zero;
059800160830           G02inv = I00dsb;
059900160830           xsrda8(wlbdat);
060000160830         endif;
060100160817         if  G02dat = *zero;
060200160817           G02dat = *date;
060300160817         endif;
060400080118
060500080118       // Località + data
060600080121         if  ORGfl1 = 'E'  and  ORGfva = *blank;
060700160817           PT1dal = 'MILANO, ' + %editc(G02dat : 'Y');
060800080121         else;
060900160817           PT1dal = %trim(ORGdes) + ', ' + %editc(G02dat : 'Y');
061000080121         endif;
061100080118
061200080118       // Oggetto
061300080118         PT1nca = %trim( %editw( I00fil:'0   ' )) + '/'
061400080118                + %trim( %editc( I00nca:'Z' ));
061500080118         PT1aac = I00aac;
061600080118
061700080118       // Dati relativi alla spedizione
061800080118         PT1lnp = I00lnp;
061900080118         PT1nrs = I00nrs;
062000080118         PT1nsp = I00nsp;
062100080118         reset WLBdat;
062200160831         if  I00tpb = 'S';
062300160831           G02inv = ( I00aas * 10000 ) + TASmgs;
062400160831         else;
062500160831           G02inv = (I00aas * 10000) + ARBmgs;
062600160831         endif;
062700080118         xsrda8(wlbdat);
062800080118         PT1dat = G02dat;
062900160831         if  I00tpb = 'S';
063000160831           PT1ncl = TASncl;
063100160831           PT1pkf = TASpkf;
063200160831         else;
063300160831           PT1ncl = ARBncl;
063400160831           PT1pkf = ARBpkf;
063500160831         endif;
063600080118
063700080118       // Dati relativi al mittente
063800160831         if  I00tpb = 'S';
063900160831           if  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
064000160831               %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
064100160831             PT1rsm = TAArsc;
064200160831           else;
064300160831             PT1rsm = ACOrag;
064400160831           endif;
064500160831           PT1rmn = TASrmn;
064600160831           PT1rma = dTA4a.§TA4Arma;
064700160831         else;
064800160831           PT1rsm = ARBrsm;
064900160831           PT1rmn = ARBrmn;
065000160831           PT1rma = ARBrma;
065100160831         endif;
065200080118
065300080118       // dati relativi al destinatario
065400160831         if  I00tpb = 'S';
065500160831           PT1rsd = TASrsd;
065600160831           PT1lod = TASlod;
065700160831           if  TASnzd <> *blank;
065800160831             PT1prn = TASnzd;
065900160831           else;
066000160831             PT1prn = TASprd;
066100160831           endif;
066200160831         else;
066300160831           PT1rsd = ARBrsd;
066400160831           PT1lod = ARBlod;
066500160831           if  ARBnzd <> *blank;
066600160831             PT1prn = ARBnzd;
066700160831           else;
066800160831             PT1prn = ARBprd;
066900160831           endif;
067000160831         endif;
067100080118
067200080118       // Anomalia
067300080118         Wtad = I00tad;
067400080118         exsr rep_TabTAD;
067500130805         SELECT;
067600130805         WHEN §TADdesi <> *blank;
067700130805           PT1tad = §TADdesi;
067800130805         WHEN §TADdest <> *blank;
067900080121           PT1tad = §TADdest;
068000130805         OTHER;
068100080121           PT1tad = §TADdesc;
068200130805         ENDSL;
068300080121       // Descrizione anomalia
068400080121         exsr Rep_DesTAD;
068500080118
068600080118       // Evento
068700080121         IF  DCTnev > *zero;
068800080118           PT1kev = %trim( %editw( DCTaae : '0    ' )) + '/'
068900080118                  + %trim( %editc( DCTnev : 'Z' ));
069000080118           chain (DCTaae : DCTnev) FNDET000;
069100080118           if  %found(FNDET01L);
069200080118             Wtad   = DETtad;
069300080118             exsr rep_TabTAD;
069400080121             if  §TADdest <> *blank;
069500080118               PT1tae = §TADdest;
069600080121             else;
069700080118               PT1tae = §TADdesc;
069800080121             endif;
069900080121           endif;
070000080121         ENDIF;
070100080118
070200080118       // Quantità danneggiate
070300080118       // - Peso
070400080121         if  DCTpkd > *zero;
070500080118           PT1qtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
070600080121         endif;
070700080118       // - Colli
070800080121         if  DCTncn > *zero;
070900080118           PT1qtd = %trim(PT1qtd)
071000130805                  + ' Parcels ' + %trim( %editc( DCTncn : 'Z' ));
071100080121         endif;
071200080118       // - Pezzi
071300080118       //   + Descrizione pezzi danneggiati
071400080121         if  DCTnpz > *zero;
071500080118           PT1qtd  = %trim(PT1qtd)
071600130805                   + ' Pieces ' + %trim( %editc( DCTnpz : 'Z' ));
071700080118           exsr Rep_DesPZD;
071800080121         endif;
071900080117
072000080117       ENDSR;
072100080117
072200080117       //--------------------------------------------------------------
072300080117       //?Chiusura file stampa & Cancellazione override
072400080117       //--------------------------------------------------------------
072500080117       BEGSR sr_DeleteOvr;
072600080117
072700130802         if  %open(FIDN39P);
072800130802           close FIDN39P;
072900160817           if  I00mod   <> 'R'     and
073000160817               §MRADreg <> *blank;
073100160817             callp(e) qCmdExc(C_CmdDltOvr : %size(C_CmdDltOvr));
073200160817           endif;
073300080117         endif;
073400080117
073500080117       ENDSR;
073600080118
073700080118       //--------------------------------------------------------------
073800080118       //?Reperimento descrizione anomalia
073900080118       //--------------------------------------------------------------
074000080118       BEGSR rep_DesTAD;
074100080118
074200080118         clear YY;
074300080118         clear $TAD;
074400080118
074500080118         DCStpd = 'C';
074600080118         DCSnkt = Wnumca;
074700080118         DCSstd = 'A';
074800080118
074900080118         setll %kds(k03dcs01 : 3) FNDCS000;
075000080118         reade %kds(k03dcs01 : 3) FNDCS000;
075100080118
075200080121         dow  NOT  %eof(FNDCS01L)   and
075300080118              YY < %elem($TAD);
075400080118           yy += 1;
075500080118           $TAD(yy) = DCSnot;
075600080118           reade %kds(k03dcs01 : 3) FNDCS000;
075700080121         enddo;
075800080118
075900080118         PT1da1 = $TAD(1);
076000080118         PT1da2 = $TAD(2) + $TAD(3);
076100080118
076200080118       ENDSR;
076300080118
076400080118       //--------------------------------------------------------------
076500080118       //?Reperimento descrizione pezzi danneggiati/mancanti
076600080118       //--------------------------------------------------------------
076700080118       BEGSR rep_DesPZD;
076800080118
076900080118         clear YY;
077000080118         clear $TAD;
077100080118
077200080118         DCStpd = 'C';
077300080118         DCSnkt = Wnumca;
077400080118         DCSstd = 'P';
077500080118
077600080118         setll %kds(k03dcs01 : 3) FNDCS000;
077700080118         reade %kds(k03dcs01 : 3) FNDCS000;
077800080118
077900080121         dow  NOT  %eof(FNDCS01L)   and
078000080118              YY < %elem($TAD);
078100080118           yy += 1;
078200080118           $TAD(yy) = DCSnot;
078300080118           reade %kds(k03dcs01 : 3) FNDCS000;
078400080121         enddo;
078500080118
078600080118         PT1dpz = $TAD(1);
078700080118         PT1dp2 = $TAD(2) + $TAD(3);
078800080118
078900080118       ENDSR;
079000080117
079100080117       //--------------------------------------------------------------
079200080117       //?Operazioni finali.
079300080117       //--------------------------------------------------------------
079400080117       BEGSR RoutEnd;
079500080117
079600080118         kpjbu = FIDN00ds;
079700080117         return;
079800080117
079900080117       ENDSR;
080000080117
080100080117      /end-free
