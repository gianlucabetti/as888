000100141007       //==============================================================
000200160517       //?INVIO E-MAIL DI CONTESTAZIONE - PREAVVISO DANNO PARTNER EEX  ?
000300141007       //==============================================================
000400141007       //?Richiamato da FIDN03R x le C.A. con le segg. caratteristiche:?
000500160523       //?1) Fase 100                                                  ?
000600141007       //?2) Bolla Export (§DCTTISP di DCTFLO = "E")                   ?
000700141007       //?3) EuroExpress  (§DCTEURO di DCTFLO = "X")                   ?
000800141007       //?4) Responsabilità "PARTNER"                                  ?
000900160518       //?5) LNA nel *file TIPEX (v. *pgm FIDN40R)                     ?
001000160518       //?   (e con il relativo codice partner PEXCOD reperire         ?
001100160519       //?   l'indirizzo e-mail da TFNTC - APL = "C" e TNT = "87"):    ?
001200160519       //?   · 311     - GEL Express Logistik GmbH                     ?
001300160519       //?   · 333     - France Express Lyon Corbas                    ?
001400160519       //?   · 334     - Calexpress Gennevilliers                      ?
001500160519       //?   · 340/345 - Azkar                                         ?
001600160518       //?SE?codice partner NON reperito    in TIPEX00F                ?
001700160518       //? O?partner SENZA indirizzo e-mail in TFNTC00F                ?
001800160519       //?=>?Uscita CON errore (O00ERR = 'E').                         ?
001900141007       //==============================================================
002000141007
002100141007       //--------------------------------------------------------------
002200141007       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
002300141007       //--------------------------------------------------------------
002400141007
002500141007     /*PRM  dbgview(*source)
002600141007     /*END
002700141007
002800141007       //--------------------------------------------------------------
002900141007       //?Specifiche di controllo.                                     ?
003000141007       //--------------------------------------------------------------
003100141007
003200141007     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003300141007     h dftactgrp(*no)
003400141007
003500141007       //--------------------------------------------------------------
003600141007       //?Dichiarazione file.                                          ?
003700141007       //--------------------------------------------------------------
003800141007
003900141007       // -?DANNI: Testata C.A.?
004000141007     fFNDCT01L  if   e           k disk
004100141007       // -?DANNI: Testata Eventi/Denunce?
004200141007     fFNDET01L  if   e           k disk
004300141007       // -?DANNI: Descrizioni?
004400141007     fFNDCS01L  if   e           k disk
004500141007
004600141007       // -?BOLLE ARRIVI  : Testata?
004700141007     fFNARB01L  if   e           k disk
004800141007       // -?BOLLE PARTENZA: Testata?
004900141007     fFNBLP01L  if   e           k disk    prefix(ARB:3)
005000160516
005100160516       // -?Note Clienti?
005200160516     fTFNTC01L  if   e           k disk
005300141007
005400141007       // -?Organigramma Filiale/Aziendale?
005500141007     fAZORG01L  if   e           k disk
005600141007
005700141007       // -?Stampa lettera (e-mail)?
005800160518     fFIDN68P   o    e             printer usropn
005900141007     f                                     OFlind(*in01)
006000141007
006100141007       //--------------------------------------------------------------
006200141007       //?Definizione costanti.                                        ?
006300141007       //--------------------------------------------------------------
006400141007
006500160523       // -?Fase "Conferma C.A."?
006600160523     d c_FCA_Conf      c                   const(100)
006700141007
006800141007       // -?Comandi di override al PrtF?
006900141007     d c_CmdOvrPrtF    c                   const('OVRPRTF +
007000160518     d                                           file(FIDN68P) +
007100141007     d                                           ovrscope(*actgrpdfn) +
007200141007     d                                           ')
007300141007     d c_CmdDltOvr     c                   const('DLTOVR +
007400160518     d                                            file(FIDN68P) +
007500141007     d                                            lvl(*actgrpdfn)')
007600141007
007700141007       //--------------------------------------------------------------
007800141007       //?Definizione schiere.                                         ?
007900141007       //--------------------------------------------------------------
008000141007
008100141007       // -?Descrizione Tipo Anomalia?
008200141007     d sk_TAD          s                   dim( 3) like(DCSnot)  inz            descr. tipo anomalia
008300141007
008400141007       //--------------------------------------------------------------
008500141007       //?Definizione aree dati.                                       ?
008600141007       //--------------------------------------------------------------
008700141007
008800141007       // -?Dati utente?
008900141007     d §AzUte        e ds                  extname(AZUTE00F)
009000141007     d                                     dtaara
009100141007     d §DatiUte      e ds                  extname(dDatiUte)
009200141007     d                                     dtaara
009300141007
009400141007       //--------------------------------------------------------------
009500141007       //?Definizione strutture dati.                                  ?
009600141007       //--------------------------------------------------------------
009700141007
009800141007       // -?Parametri ricevuti?
009900141007     d KPJBA         e ds
010000141007     d FIDN00ds      e ds                  inz
010100141007     d   wNumCA                6     19
010200141007
010300141007       // -?Flag operativi in FNDCT?
010400141007     d dDCT01        e ds                  inz
010500141007
010600141007       // -?Tab. MRA/DAN = Bart-Maling - Danni?
010700141007     d dMRAdan       e ds
010800141007       // -?Tab. TAD = Tipi anomalia danni?
010900141007     d dTAD          e ds                  inz
011000141007
011100141007       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
011200141007     d TRTCM1ds      e ds                  inz
011300141007       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
011400141007     d   §CM1mitt    e                     inz('servizi.assicurativi')
011500141007       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
011600141007     d   §CM1dst     e                     inz('servizi.assicurativi@brt.it')
011700141007       //  ?·§CM1tips = Tipo lettera via e-mail:?
011800141007       //   ?"LET" = testo allegato in corpo con logo?
011900141007       //           ?(richiede righe libere iniziali per il logo)?
012000141007       //   ?"COR" = testo integrato senza logo?
012100141007       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
012200141007     d   §CM1tips    e                     inz('E05')
012300141007       //  ?·§CM1po   = Filiale?
012400141007     d   §CM1po      e                     inz('046')
012500141007       //  ?·§CM1var  = Oggetto e-mail?
012600141007     d   §CM1var     e                     inz('*OBJM*+
012700141007     d                                     Mail CONTESTAZIONE UK')
012800141007       //  ?·§CM1sts  = Stato?
012900141007     d   §CM1sts     e                     inz(*off)
013000141007       //  ?·§CM1sts  = Id processo?
013100141007     d   §CM1idp     e                     inz('1')
013200141007
013300141007       // -?Status?
013400141007     d Psds           sds
013500141007     d   SDSpgm          *proc
013600141007
013700141007       //--------------------------------------------------------------
013800141007       //?Definizione variabili globali.                               ?
013900141007       //--------------------------------------------------------------
014000141007
014100141007       // -?Flags booleani?
014200141007     d*// $Invio          s               n   inz
014300141007
014400141007       // -?Indici di schiera?
014500141007     d YY              s              3  0 inz
014600141007
014700141007       // -?Campi di comodo?
014800141007     d Wtad            s                   like(DCTtad)   inz
014900160517     d Wrag            s                   like(ACOrag)   inz
015000160517     d Weml            s                   like(NTCrnt)   inz
015100141007
015200141007       //--------------------------------------------------------------
015300141007       //?Definizione procedure usate e relativi parametri.            ?
015400141007       //--------------------------------------------------------------
015500141007
015600141007       // -?Reperimento dati utente?
015700141007     d TIBS34ds      e ds
015800141007      /copy gaitrasrc/srcProtoPR,TIBS34R
015900141007
016000141007       // -?Ricerca/Controllo tabelle?
016100141007     d TIBS02ds      e ds                  inz
016200141007      /copy gaitrasrc/srcProtoPR,TIBS02R
016300160516
016400160516       // -?Reperimento dati anagrafici?
016500160516      /copy gaitrasrc/srcProtoPI,TIBS69R
016600160516      /copy gaitrasrc/srcProtoPR,TIBS69R
016700160516
016800160516       // -?Partner EuroExpress?
016900160516     d FIDN40ds      e ds                  inz
017000160516     d fidn40r         pr                  extpgm('FIDN40R')
017100160516     d   fidn40ds                          likeds(FIDN40ds)
017200141007
017300141007       // -?Controllo & Inversione date?
017400141007     d WLBdat          ds                  inz
017500141007     d   G02dat                1      8  0 inz
017600141007     d   G02inv                9     16  0 inz
017700141007     d   G02err               17     17    inz('3')
017800141007     d   G02tgi               18     22  0 inz
017900141007      /copy gaitrasrc/srcProtoPR,XSRDA8
018000141007
018100141007       // -?API QCAPCMD (Process Commands)?
018200141007     d Qcmd            s           2048    inz  varying
018300141007      /copy qSysInc/qRpgleSrc,QCAPCMD
018400141007      /copy gaitrasrc/srcProtoPR,QCAPCMD
018500141007
018600141007       // -?Parametri gestione errori API.?
018700141007      /copy qsysinc/qrpglesrc,QUSEC
018800141007
018900141007       //--------------------------------------------------------------
019000141007       //?Definizione key-list.                                        ?
019100141007       //--------------------------------------------------------------
019200141007
019300141007       // -?File FNDCT01L?
019400160518     d keyFNDCT01    e ds                  extname( FNDCT01L : *key )
019500160518     d                                     prefix( k_ )        inz
019600141007
019700141007       // -?File FNDCS01L?
019800160518     d keyFNDCS01    e ds                  extname( FNDCS01L : *key )
019900160518     d                                     prefix( k_ )        inz
020000141007       // -?File FNDET01L?
020100160518     d keyFNDET01    e ds                  extname( FNDET01L : *key )
020200160518     d                                     prefix( k_ )        inz
020300141007
020400141007       // -?File FNBLP01L / FNARB01L?
020500160518     d keyFNBLPARB   e ds                  extname( FNBLP01L : *key )
020600160518     d                                     prefix( k_ARB : 3 ) inz
020700160516
020800160516       // -?File TFNTC01L?
020900160518     d keyTFNTC01    e ds                  extname( TFNTC01L : *key )
021000160518     d                                     prefix( k_ )        inz
021100141007
021200141007       //--------------------------------------------------------------
021300141007       //?Riepilogo indicatori.                                        ?
021400141007       //--------------------------------------------------------------
021500141007       //--------------------------------------------------------------
021600141007
021700141007       //--------------------------------------------------------------
021800141007       //?M A I N - L I N E                                            ?
021900141007       //--------------------------------------------------------------
022000141007
022100141007     c     *Entry        plist
022200141007     c                   parm                    KPJBA
022300141007
022400141007      /free
022500141007
022600141007       // -?Operazioni iniziali?
022700141007       exsr  sr_RoutInz;
022800141007
022900141007       // -?Preparazione della e-mail da inviare?
023000141007       if  o00err = *blank;
023100141007         exsr sr_PrintMail;
023200141007       endif;
023300141007
023400141007       // -?Operazioni finali?
023500141007       exsr  sr_RoutEnd;
023600141007
023700141007       //--------------------------------------------------------------
023800141007       //?Operazioni iniziali.                                         ?
023900141007       //--------------------------------------------------------------
024000141007       BEGSR  sr_RoutInz;
024100141007
024200141007         *inLR  = *on;
024300141007
024400141007         // -?Ricezione parametri?
024500141007         FIDN00ds = kpjbu;
024600141007         clear  o00f12;
024700141007         clear  o00f03;
024800141007         clear  o00err;
024900141007         clear  o00msg;
025000141007
025100141007         // -?Reperimento dati job?
025200141007         exsr  sr_DatiJob;
025300141007
025400141007         // -?Reperimento tab. "MRA"?
025500141007         exsr  sr_Rep_TabMRA;
025600141007         if  o00err = 'E';
025700141007           leavesr;
025800141007         endif;
025900141007
026000141007         // -?Verifica dei parametri ricecvuti?
026100141007         if  i00aac = *zero   or
026200141007             i00fil = *zero   or
026300141007             i00nca = *zero;
026400141007           o00err = 'E';
026500141007           o00msg = 'Manca il numero C.A.!!!';
026600141007           leavesr;
026700141007         endif;
026800141007
026900141007         // -?Reperimento dati testata C.A.?
027000141007         exsr  sr_Rep_FNDCT;
027100141007         if  o00err = 'E';
027200141007           leavesr;
027300141007         endif;
027400141007
027500141007         // -?Decodifica filiale gestione?
027600160518         chain  ( i00fgs )  AZORG;
027700160518         if  NOT  %found( AZORG01L );
027800141007           o00err = 'E';
027900141007           o00msg = 'Filiale gestione errata';
028000141007           leavesr;
028100141007         endif;
028200141007
028300141007         // -?Reperimento dati spedizione?
028400141007         exsr  sr_Rep_FNBLPARB;
028500160517         if  o00err = 'E';
028600160517           leavesr;
028700160517         endif;
028800160516
028900160516         // -?Reperimento partner?
029000160517         clear  Wrag;
029100160517         clear  Weml;
029200160518
029300160516         clear  FIDN40ds;
029400160516         I40dsp = ( ( ARBaas * 10000 ) + ARBmgs );
029500160516         if  §DCTtisp = 'E';
029600160516           I40fil = DCTlna;
029700160516         endif;
029800160516
029900160516         FIDN40R ( FIDN40ds );
030000160516
030100160517         if  O40cod = *zeros;
030200160519           o00err = 'E';
030300160519           o00msg = 'Codice partner NON reperito';
030400160517           leavesr;
030500160517         endif;
030600160517
030700160517         // -?Decodifica partner?
030800160518         clear  TIBS69ds;
030900160518         I69tla = 'L';
031000160518         I69kac = O40cod;
031100160518         tibs69r( TIBS69ds :
031200160518                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
031300160518
031400160518         if  O69err = *blanks;
031500160518           Wrag = ACOrag;
031600160518         else;
031700160518           o00err = 'E';
031800160518           o00msg = 'Codice partner errato (' + %editc( O40cod : 'X' )
031900160518                  +                       ')';
032000160518           leavesr;
032100160518         endif;
032200160516
032300160517         // -?Reperimento indirizzo e-mail del partner?
032400160517         k_NTCapl = 'C' ;
032500160517         k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( O40cod : 'X' );
032600160517         clear  k_NTCnk2;
032700160517         k_NTCtnt = '87';
032800160517
032900160518         chain  %kds( keyTFNTC01 : 4 )  TFNTC;
033000160517
033100160518         if  %found( TFNTC01L )  and  NTCrnt <> *blanks;
033200160517           Weml = NTCrnt;
033300160517         else;
033400160519           o00err = 'E';
033500160519           o00msg = 'Indirizzo e-mail del partner NON reperito';
033600160517           leavesr;
033700160517         endif;
033800141007
033900141007       ENDSR;
034000141007
034100141007       //--------------------------------------------------------------
034200141007       //?Reperimento Dati del job (Utente/Operativi).                 ?
034300141007       //--------------------------------------------------------------
034400141007       BEGSR  sr_DatiJob;
034500141007
034600141007         in(E) §AzUte;
034700141007         if NOT %error;
034800141007           in(E) §DatiUte;
034900141007         endif;
035000141007         if %error or RSut = *blanks;
035100141007           clear TIBS34ds;
035200141007           tibs34r(tibs34ds);
035300141007           in §AzUte;
035400141007           in §DatiUte;
035500141007         endif;
035600141007
035700141007       ENDSR;
035800141007
035900141007       //--------------------------------------------------------------
036000141007       //?Reperimento dati testata C.A.                                ?
036100141007       //--------------------------------------------------------------
036200141007       BEGSR  sr_Rep_FNDCT;
036300141007
036400141007         clear  keyFNDCT01;
036500141007         k_DCTaac = i00aac;
036600141007         k_DCTfil = i00fil;
036700141007         k_DCTnca = i00nca;
036800141007
036900141007         chain  %kds( keyFNDCT01 )  FNDCT000;
037000141007
037100141007         if  NOT  %found(FNDCT01L)   or
037200141007             DCTatb <> *blank;
037300141007           o00err = 'E';
037400141007           o00msg = 'C.A. non reperita';
037500141007           leavesr;
037600141007         endif;
037700141007
037800141007         dDCT01 = DCTflo;
037900141007
038000160523         if  DCTfca   <> c_FCA_Conf  or
038100141007             §DCTtisp <> 'E'         or
038200141007             §DCTeuro <> 'X'         or
038300141007             DCTres   <> 'P';
038400141007           o00err = 'E';
038500160519           o00msg = 'C.A. non contestabile a EuroExpress';
038600141007           leavesr;
038700141007         endif;
038800160519
038900160519         if  DCTlna <> 311  and
039000160519             DCTlna <> 333  and
039100160519             DCTlna <> 334  and
039200160519             DCTlna <> 340  and  DCTlna <> 345;
039300160519           o00err = 'E';
039400160519           o00msg = 'C.A. non contestabile a EuroExpress';
039500160519           leavesr;
039600160519         endif;
039700141007
039800141007         // -?Impostazione numero spedizione tra i parms SE non ricevuto?
039900141007         if  i00aas = *zero   or
040000141007             i00lnp = *zero   or
040100141007             i00nsp = *zero;
040200141007           i00aas = DCTaas;
040300141007           i00lnp = DCTlnp;
040400141007           i00nrs = DCTnrs;
040500141007           i00nsp = DCTnsp;
040600141007         endif;
040700141007
040800141007         // -?Impostazione altri dati tra i parms SE non ricevuti?
040900141007         if  i00fca = *zero;
041000141007           i00fca = DCTfca;
041100141007         endif;
041200141007         if  i00fgs = *zero;
041300141007           i00fgs = DCTgfc;
041400141007         endif;
041500141007         if  i00tad = *blank;
041600141007           i00tad = DCTtad;
041700141007         endif;
041800141007
041900141007       ENDSR;
042000141007
042100141007       //--------------------------------------------------------------
042200141007       //?Reperimento dati spedizione.                                 ?
042300141007       //--------------------------------------------------------------
042400141007       BEGSR  sr_Rep_FNBLPARB;
042500141007
042600141021         clear  keyFNBLPARB;
042700141021         k_ARBaas = i00aas;
042800141021         k_ARBlnp = i00lnp;
042900141021         k_ARBnrs = i00nrs;
043000141021         k_ARBnsp = i00nsp;
043100141021
043200141007         If  i00tpb = 'P';
043300141007
043400141007           chain  %kds( keyFNBLPARB )  FNBLP000;
043500141007
043600141007           if  NOT  %found(FNBLP01L)   or
043700141007               ARBatb <> *blank;
043800141007             o00err = 'E';
043900141007             o00msg = 'Spedizione non reperita';
044000141007             leavesr;
044100141007           endif;
044200141007
044300141007         Else;
044400141007
044500141007           chain  %kds( keyFNBLPARB )  FNARB000;
044600141007
044700141007           if  NOT  %found(FNARB01L)   or
044800141007               ARBatb <> *blank;
044900141007             o00err = 'E';
045000141007             o00msg = 'Spedizione non reperita';
045100141007             leavesr;
045200141007           endif;
045300141007
045400141007         EndIf;
045500141007
045600141007       ENDSR;
045700141007
045800141007       //--------------------------------------------------------------
045900141007       //?Stampa della lettera da inviare via e-mail.                  ?
046000141007       //--------------------------------------------------------------
046100141007       BEGSR  sr_PrintMail;
046200141007
046300141007         // -?Override al file di stampa ed apertura dello stesso?
046400141007         exsr  sr_Override;
046500141007
046600141007         // -?Impostazione dei campi in stampa?
046700141007         exsr  sr_RieField;
046800141007
046900141007         // -?Impostazione indicatori utilizzati nel PrtF?
047000141007         *in42  = (PT1da2 <> *blanks);
047100141007         *in43  = (PT1qtd <> *blanks);
047200141007         *in44  = (PT1dp2 <> *blanks);
047300141007         *in47  = (ARBrma <> *blanks);
047400141007
047500141007         // -?Stampa?
047600160518         write  DN68T01;
047700141007
047800141007         // -?Chiusura file stampa & Cancellazione override?
047900141007         exsr  sr_DeleteOvr;
048000141007
048100141007       ENDSR;
048200141007
048300141007       //--------------------------------------------------------------
048400141007       //?Esecuzione override ed apertura del file di stampa.          ?
048500141007       //--------------------------------------------------------------
048600141007       BEGSR  sr_Override;
048700141007
048800141007         //clear  $Invio;
048900141007         reset  TRTCM1ds;
049000141007
049100141007         // -?Esecuzione override?
049200141007         If  §MRADreg <> *blank;
049300141007
049400141007           §CM1mitt = %trim(§MRADmitt);
049500160517           §CM1dst  = Weml;
049600141007           §CM1tips = §MRADreg;
049700141007           §CM1po   = %editc( DUTpou : 'X' );
049800141007           §CM1var  = '*OBJM*'
049900141007                    + 'Damage comunication n. '
050000160518                    + %editw( I00fil : '0   ' )
050100141007                    + '/'
050200141007                    + %trim( %editc( I00nca : 'Z' ))
050300141007                    + ' '
050400141007                    + %subst( %char( I00aac ) : 3 : 2 );
050500141007           §CM1idp  = §MRADidpro;
050600141007
050700141007           Qcmd = c_CmdOvrPrtF
050800160518                + ' outq(' + %trim( §MRADoutqi ) + ')'
050900141007                + ' usrdfndta(''' + TRTCM1ds + ''')';
051000141007           exsr  sr_ExecCmd;
051100141007           //$Invio = (Qusei = *blank);
051200141007
051300141007         EndIf;
051400141007
051500141007         // -?Apertura file di stampa?
051600160518         open  FIDN68P;
051700141007
051800141007       ENDSR;
051900141007
052000141007       //--------------------------------------------------------------
052100141007       //?Impostazione dei campi in stampa.                            ?
052200141007       //--------------------------------------------------------------
052300141007       BEGSR  sr_RieField;
052400141007
052500160518         clear  DN68T01;
052600141007
052700141007         // -?Intestazione Destinatario?
052800160517         PT1rgs = wRag;
052900141007
053000141007         // -?Località + Data?
053100141007         if  ORGfl1 = 'E'  and  ORGfva = *blank;
053200160518           PT1dal = 'MILANO, ' + %editc( *date : 'Y' );
053300141007         else;
053400160518           PT1dal = %trim( ORGdes ) + ', ' + %editc( *date : 'Y' );
053500141007         endif;
053600141007
053700141007         // -?Oggetto?
053800141007         PT1nca = %trim( %editw( I00fil:'0   ' )) + '/'
053900141007                + %trim( %editc( I00nca:'Z' ));
054000141007         PT1aac = I00aac;
054100141007
054200141007         // -?Dati relativi alla Spedizione?
054300141007         PT1lnp = I00lnp;
054400141007         PT1nrs = I00nrs;
054500141007         PT1nsp = I00nsp;
054600141007         reset WLBdat;
054700160518         G02inv = ( I00aas * 10000 ) + ARBmgs;
054800141007         xsrda8 ( WLBdat );
054900141007         PT1dat = G02dat;
055000141007         PT1ncl = ARBncl;
055100141007         PT1pkf = ARBpkf;
055200141007
055300141007         // -?Dati relativi al Mittente?
055400141007         PT1rsm = ARBrsm;
055500141007         PT1rmn = ARBrmn;
055600141007         PT1rma = ARBrma;
055700141007
055800141007         // -?Dati relativi al Destinatario?
055900141007         PT1rsd = ARBrsd;
056000141007         PT1lod = ARBlod;
056100141007         if  ARBnzd <> *blank;
056200141007           PT1prn = ARBnzd;
056300141007         else;
056400141007           PT1prn = ARBprd;
056500141007         endif;
056600141007
056700141007         // -?Anomalia?
056800141007         Wtad = I00tad;
056900141007         exsr  sr_Rep_TabTAD;
057000141007         select;
057100141007           when  §TADdesi <> *blank;
057200141007             PT1tad = §TADdesi;
057300141007           when  §TADdest <> *blank;
057400141007             PT1tad = §TADdest;
057500141007           other;
057600141007             PT1tad = §TADdesc;
057700141007         endsl;
057800141007         // -?Descrizione anomalia?
057900141007         exsr  sr_Rep_DesTAD;
058000141007
058100141007         // -?Evento?
058200141007         IF  DCTnev > *zero;
058300141007           PT1kev = %trim( %editw( DCTaae : '0    ' )) + '/'
058400141007                  + %trim( %editc( DCTnev : 'Z' ));
058500141007           chain (DCTaae : DCTnev) FNDET000;
058600141007           If  %found(FNDET01L);
058700141007             Wtad = DETtad;
058800141007             exsr  sr_Rep_TabTAD;
058900141007             if  §TADdest <> *blank;
059000141007               PT1tae = §TADdest;
059100141007             else;
059200141007               PT1tae = §TADdesc;
059300141007             endif;
059400141007           EndIf;
059500141007         ENDIF;
059600141007
059700141007         // -?Quantità danneggiate?
059800141007         //  ?·Peso?
059900141007         if  DCTpkd > *zero;
060000141007           PT1qtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
060100141007         endif;
060200141007         //  ?·Colli?
060300141007         if  DCTncn > *zero;
060400141007           PT1qtd = %trim(PT1qtd)
060500141007                  + ' Parcels ' + %trim( %editc( DCTncn : 'Z' ));
060600141007         endif;
060700141007
060800141007         // -?Pezzi?
060900141007         //  ?+ Descrizione pezzi danneggiati?
061000141007         if  DCTnpz > *zero;
061100141007           PT1qtd  = %trim(PT1qtd)
061200141007                   + ' Pieces ' + %trim( %editc( DCTnpz : 'Z' ));
061300141007           exsr  sr_Rep_DesPZD;
061400141007         endif;
061500141007
061600141007       ENDSR;
061700141007
061800141007       //--------------------------------------------------------------
061900141007       //?Chiusura file stampa & Cancellazione override.               ?
062000141007       //--------------------------------------------------------------
062100141007       BEGSR  sr_DeleteOvr;
062200141007
062300160518         if  %open(FIDN68P);
062400141007
062500160518           close  FIDN68P;
062600141007
062700141007           Qcmd = c_CmdDltOvr;
062800141007           exsr  sr_ExecCmd;
062900141007
063000141007         endif;
063100141007
063200141007       ENDSR;
063300141007
063400141007       //--------------------------------------------------------------
063500141007       //?Reperimento tab. MRA = Bart-Maling - Danni.                  ?
063600141007       //--------------------------------------------------------------
063700141007       BEGSR  sr_Rep_TabMRA;
063800141007
063900141007         clear  dMRAdan;
064000160519
064100160519         clear  TIBS02ds;
064200160519         T02mod = 'C';
064300160519         T02cod = 'MRA';
064400160519         T02ke1 = %trimR( SDSpgm );
064500160519         T02ke2 = 'DAN';
064600160519
064700160519         TNTBE_RicercaControllo ( kpjba : tibs02ds );
064800141007
064900141007         if T02err = *blanks;
065000141007           dMRAdan = T02uni;
065100141007         else;
065200141007           o00err = 'E';
065300141007           o00msg = 'Manca tab. MRA'
065400141007                  + ' "' + %trimr(T02ke1) + '" /'
065500141007                  + ' "' + %trimr(T02ke2) + '"';
065600141007           leavesr;
065700141007         endif;
065800141007
065900141007       ENDSR;
066000141007
066100141007       //--------------------------------------------------------------
066200141007       //?Reperimento tab. TAD = Tipo Anomalia.                        ?
066300141007       //--------------------------------------------------------------
066400141007       BEGSR  sr_Rep_TabTAD;
066500141007
066600141007         clear  dTAD;
066700141007
066800141007         clear  TIBS02ds;
066900141007         T02mod = 'C';
067000141007         T02sif = knsif;
067100141007         T02cod = 'TAD';
067200141007         T02ke1 = Wtad;
067300141007
067400141007         TNTBE_RicercaControllo ( kpjba : tibs02ds );
067500141007
067600141007         if T02err = *blanks;
067700141007           dTAD = T02uni;
067800141007         endif;
067900141007
068000141007       ENDSR;
068100141007
068200141007       //--------------------------------------------------------------
068300141007       //?Reperimento descrizione anomalia.                            ?
068400141007       //--------------------------------------------------------------
068500141007       BEGSR  sr_Rep_DesTAD;
068600141007
068700141007         clear  YY;
068800141007         clear  sk_TAD;
068900141007
069000141007         clear  keyFNDCS01;
069100141007         k_DCStpd = 'C';
069200141007         k_DCSnkt = wNumCA;
069300141007         k_DCSstd = 'A';
069400141007
069500141007         setll  %kds( keyFNDCS01 : 3 )  FNDCS000;
069600141007         reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
069700141007
069800141007         DoW  NOT  %eof(FNDCS01L)   and
069900141007              YY < %elem(sk_TAD);
070000141007           yy += 1;
070100141007           sk_TAD(yy) = DCSnot;
070200141007           reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
070300141007         EndDo;
070400141007
070500141007         PT1da1 = sk_TAD(1);
070600141007         PT1da2 = sk_TAD(2) + sk_TAD(3);
070700141007
070800141007       ENDSR;
070900141007
071000141007       //--------------------------------------------------------------
071100141007       //?Reperimento descrizione pezzi danneggiati/mancanti.          ?
071200141007       //--------------------------------------------------------------
071300141007       BEGSR  sr_Rep_DesPZD;
071400141007
071500141007         clear  YY;
071600141007         clear  sk_TAD;
071700141007
071800141007         clear  keyFNDCS01;
071900141007         k_DCStpd = 'C';
072000141007         k_DCSnkt = wNumCA;
072100141007         k_DCSstd = 'P';
072200141007
072300141007         setll  %kds( keyFNDCS01 : 3 )  FNDCS000;
072400141007         reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
072500141007
072600141007         DoW  NOT  %eof(FNDCS01L)   and
072700141007              YY < %elem(sk_TAD);
072800141007           yy += 1;
072900141007           sk_TAD(yy) = DCSnot;
073000141007           reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
073100141007         EndDo;
073200141007
073300141007         PT1dpz = sk_TAD(1);
073400141007         PT1dp2 = sk_TAD(2) + sk_TAD(3);
073500141007
073600141007       ENDSR;
073700141007
073800141007       //--------------------------------------------------------------
073900141007       //?Esecuzione del comando (già impostato).                      ?
074000141007       //--------------------------------------------------------------
074100141007       BEGSR  sr_ExecCmd;
074200141007
074300141007         clear Qcap0100;
074400141007         Qcabcsdh = *off;
074500141007         Qcapa    = *off;
074600141007         Qcacmdss = *off;
074700141007         Qcaerved = *allX'00';
074800141007
074900141007         clear Qusec;
075000141007         Qusbprv  = %size(Qusec);
075100141007
075200141007         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
075300141007                           %size(Qcap0100) : 'CPOP0100' : *omit :
075400141007                           0 : 0 : Qusec);
075500141007
075600141007         //if  Qusei <> *blank;
075700141007         //  ...;
075800141007         //endif;
075900141007
076000141007       ENDSR;
076100141007
076200141007       //--------------------------------------------------------------
076300141007       //?Operazioni finali.                                           ?
076400141007       //--------------------------------------------------------------
076500141007       BEGSR  sr_RoutEnd;
076600160517
076700160517         // -?Chiusura funzioni precedentemente aperte?
076800160517         reset  TIBS69ds;
076900160517         //I69tla = 'C';         ?(già così)?
077000160517         tibs69r( tibs69ds :
077100160517                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
077200141007
077300141007         // -?Restituzione parametri?
077400141007         kpjbu = FIDN00ds;
077500141007
077600141007         // -?Uscita dal *pgm?
077700141007         return;
077800141007
077900141007       ENDSR;
078000141007
078100141007      /end-free
