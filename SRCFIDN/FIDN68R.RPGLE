000100141007       //==============================================================
000200160517       //?INVIO E-MAIL DI CONTESTAZIONE - PREAVVISO DANNO PARTNER EEX  ?
000300141007       //==============================================================
000400160817       //?Richiamato da FIDN03R & FIDN78R x le C.A. con le seguenti    ?
000500160817       //?caratteristiche:                                             ?
000600160523       //?1) Fase 100                                                  ?
000700141007       //?2) Bolla Export (§DCTTISP di DCTFLO = "E")                   ?
000800141007       //?3) EuroExpress  (§DCTEURO di DCTFLO = "X")                   ?
000900141007       //?4) Responsabilità "PARTNER"                                  ?
001000160518       //?5) LNA nel *file TIPEX (v. *pgm FIDN40R)                     ?
001100160518       //?   (e con il relativo codice partner PEXCOD reperire         ?
001200160519       //?   l'indirizzo e-mail da TFNTC - APL = "C" e TNT = "87"):    ?
001300160519       //?   · 311     - GEL Express Logistik GmbH                     ?
001400160519       //?   · 333     - France Express Lyon Corbas                    ?
001500160519       //?   · 334     - Calexpress Gennevilliers                      ?
001600160519       //?   · 340/345 - Azkar                                         ?
001700160518       //?SE?codice partner NON reperito    in TIPEX00F                ?
001800160518       //? O?partner SENZA indirizzo e-mail in TFNTC00F                ?
001900160519       //?=>?Uscita CON errore (O00ERR = 'E').                         ?
002000141007       //==============================================================
002100141007
002200141007       //--------------------------------------------------------------
002300141007       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
002400141007       //--------------------------------------------------------------
002500141007
002600141007     /*PRM  dbgview(*source)
002700160831     /*CMD  ovrdbf file(FNARB01L) tofile(FILTRAPRD/FNARB01L) +
002800160831     /*CMD         ovrscope(*calllvl)
002900160831     /*CMD  ovrdbf file(FNBLP01L) tofile(FILTRAPRD/FNBLP01L) +
003000160831     /*CMD         ovrscope(*calllvl)
003100160831     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
003200160831     /*CMD         ovrscope(*calllvl)
003300160831     /*CMD  ovrdbf file(TITAA30C) tofile(GAITRAGRPS/TITAA30C) +
003400160831     /*CMD         ovrscope(*calllvl)
003500160831     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
003600160831     /*CMD         ovrscope(*calllvl)
003700160831     /*END  dltovr file(*all) lvl(*)
003800141007     /*END
003900141007
004000141007       //--------------------------------------------------------------
004100141007       //?Specifiche di controllo.                                     ?
004200141007       //--------------------------------------------------------------
004300141007
004400141007     h decedit('0,') datedit(*dmy/) option(*nodebugio)
004500141007     h dftactgrp(*no)
004600141007
004700141007       //--------------------------------------------------------------
004800141007       //?Dichiarazione file.                                          ?
004900141007       //--------------------------------------------------------------
005000141007
005100141007       // -?DANNI: Testata C.A.?
005200141007     fFNDCT01L  if   e           k disk
005300141007       // -?DANNI: Testata Eventi/Denunce?
005400141007     fFNDET01L  if   e           k disk
005500141007       // -?DANNI: Descrizioni?
005600141007     fFNDCS01L  if   e           k disk
005700141007
005800141007       // -?BOLLE ARRIVI  : Testata?
005900160831     fFNARB01L  if   e           k disk    usropn
006000141007       // -?BOLLE PARTENZA: Testata?
006100160831     fFNBLP01L  if   e           k disk    usropn
006200160831     f                                     prefix(ARB:3)
006300160831
006400160831       // -?BOLLE SEDE: da Fatturare + fatturate "ultimi mesi"?
006500160831       //  ?                         + fatturate "storico"?
006600160831     fTITAS30C  if   e           k disk    usropn
006700160831       // -?BOLLE SEDE: Anagrafiche (con "Storico")?
006800160831     fTITAA30C  if   e           k disk    usropn
006900160831       // -?BOLLE SEDE: Riferimenti bolla (con "Storico")?
007000160831     fTITA430C  if   e           k disk    usropn
007100160516
007200160516       // -?Note Clienti?
007300160516     fTFNTC01L  if   e           k disk
007400141007
007500141007       // -?Organigramma Filiale/Aziendale?
007600141007     fAZORG01L  if   e           k disk
007700141007
007800141007       // -?Stampa lettera (e-mail)?
007900160518     fFIDN68P   o    e             printer usropn
008000141007     f                                     OFlind(*in01)
008100141007
008200141007       //--------------------------------------------------------------
008300141007       //?Definizione costanti.                                        ?
008400141007       //--------------------------------------------------------------
008500141007
008600160523       // -?Fase "Conferma C.A."?
008700160523     d c_FCA_Conf      c                   const(100)
008800141007
008900141007       // -?Comandi di override al PrtF?
009000141007     d c_CmdOvrPrtF    c                   const('OVRPRTF +
009100160518     d                                           file(FIDN68P) +
009200141007     d                                           ovrscope(*actgrpdfn) +
009300141007     d                                           ')
009400141007     d c_CmdDltOvr     c                   const('DLTOVR +
009500160518     d                                            file(FIDN68P) +
009600141007     d                                            lvl(*actgrpdfn)')
009700141007
009800141007       //--------------------------------------------------------------
009900141007       //?Definizione schiere.                                         ?
010000141007       //--------------------------------------------------------------
010100141007
010200141007       // -?Descrizione Tipo Anomalia?
010300141007     d sk_TAD          s                   dim( 3) like(DCSnot)  inz            descr. tipo anomalia
010400141007
010500141007       //--------------------------------------------------------------
010600141007       //?Definizione aree dati.                                       ?
010700141007       //--------------------------------------------------------------
010800141007
010900141007       // -?Dati utente?
011000141007     d §AzUte        e ds                  extname(AZUTE00F)
011100141007     d                                     dtaara
011200141007     d §DatiUte      e ds                  extname(dDatiUte)
011300141007     d                                     dtaara
011400141007
011500141007       //--------------------------------------------------------------
011600141007       //?Definizione strutture dati.                                  ?
011700141007       //--------------------------------------------------------------
011800141007
011900141007       // -?Parametri ricevuti?
012000141007     d KPJBA         e ds
012100141007     d FIDN00ds      e ds                  inz
012200141007     d   wNumCA                6     19
012300141007
012400141007       // -?Flag operativi in FNDCT?
012500141007     d dDCT01        e ds                  inz
012600160831
012700160831       // -?TITA4 tipo record "A"?
012800160831     d dTA4a         e ds                  inz   qualified
012900141007
013000141007       // -?Tab. MRA/DAN = Bart-Maling - Danni?
013100141007     d dMRAdan       e ds
013200141007       // -?Tab. TAD = Tipi anomalia danni?
013300141007     d dTAD          e ds                  inz
013400141007
013500141007       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
013600141007     d TRTCM1ds      e ds                  inz
013700141007       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
013800141007     d   §CM1mitt    e                     inz('servizi.assicurativi')
013900141007       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
014000141007     d   §CM1dst     e                     inz('servizi.assicurativi@brt.it')
014100141007       //  ?·§CM1tips = Tipo lettera via e-mail:?
014200141007       //   ?"LET" = testo allegato in corpo con logo?
014300141007       //           ?(richiede righe libere iniziali per il logo)?
014400141007       //   ?"COR" = testo integrato senza logo?
014500141007       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
014600141007     d   §CM1tips    e                     inz('E05')
014700141007       //  ?·§CM1po   = Filiale?
014800141007     d   §CM1po      e                     inz('046')
014900141007       //  ?·§CM1var  = Oggetto e-mail?
015000141007     d   §CM1var     e                     inz('*OBJM*+
015100141007     d                                     Mail CONTESTAZIONE UK')
015200141007       //  ?·§CM1sts  = Stato?
015300141007     d   §CM1sts     e                     inz(*off)
015400141007       //  ?·§CM1sts  = Id processo?
015500141007     d   §CM1idp     e                     inz('1')
015600141007
015700141007       // -?Status?
015800141007     d Psds           sds
015900141007     d   SDSpgm          *proc
016000141007
016100141007       //--------------------------------------------------------------
016200141007       //?Definizione variabili globali.                               ?
016300141007       //--------------------------------------------------------------
016400141007
016500141007       // -?Flags booleani?
016600141007     d*// $Invio          s               n   inz
016700141007
016800141007       // -?Indici di schiera?
016900141007     d YY              s              3  0 inz
017000141007
017100141007       // -?Campi di comodo?
017200141007     d Wtad            s                   like(DCTtad)   inz
017300160517     d Wrag            s                   like(ACOrag)   inz
017400160517     d Weml            s                   like(NTCrnt)   inz
017500141007
017600141007       //--------------------------------------------------------------
017700141007       //?Definizione procedure usate e relativi parametri.            ?
017800141007       //--------------------------------------------------------------
017900141007
018000141007       // -?Reperimento dati utente?
018100141007     d TIBS34ds      e ds
018200141007      /copy gaitrasrc/srcProtoPR,TIBS34R
018300141007
018400141007       // -?Ricerca/Controllo tabelle?
018500141007     d TIBS02ds      e ds                  inz
018600141007      /copy gaitrasrc/srcProtoPR,TIBS02R
018700160516
018800160516       // -?Reperimento dati anagrafici?
018900160516      /copy gaitrasrc/srcProtoPI,TIBS69R
019000160516      /copy gaitrasrc/srcProtoPR,TIBS69R
019100160516
019200160516       // -?Partner EuroExpress?
019300160516     d FIDN40ds      e ds                  inz
019400160516     d fidn40r         pr                  extpgm('FIDN40R')
019500160516     d   fidn40ds                          likeds(FIDN40ds)
019600141007
019700141007       // -?Controllo & Inversione date?
019800141007     d WLBdat          ds                  inz
019900141007     d   G02dat                1      8  0 inz
020000141007     d   G02inv                9     16  0 inz
020100141007     d   G02err               17     17    inz('3')
020200141007     d   G02tgi               18     22  0 inz
020300141007      /copy gaitrasrc/srcProtoPR,XSRDA8
020400141007
020500141007       // -?API QCAPCMD (Process Commands)?
020600141007     d Qcmd            s           2048    inz  varying
020700141007      /copy qSysInc/qRpgleSrc,QCAPCMD
020800141007      /copy gaitrasrc/srcProtoPR,QCAPCMD
020900141007
021000141007       // -?Parametri gestione errori API.?
021100141007      /copy qsysinc/qrpglesrc,QUSEC
021200141007
021300141007       //--------------------------------------------------------------
021400141007       //?Definizione key-list.                                        ?
021500141007       //--------------------------------------------------------------
021600141007
021700141007       // -?File FNDCT01L?
021800160518     d keyFNDCT01    e ds                  extname( FNDCT01L : *key )
021900160518     d                                     prefix( k_ )        inz
022000141007
022100141007       // -?File FNDCS01L?
022200160518     d keyFNDCS01    e ds                  extname( FNDCS01L : *key )
022300160518     d                                     prefix( k_ )        inz
022400141007       // -?File FNDET01L?
022500160518     d keyFNDET01    e ds                  extname( FNDET01L : *key )
022600160518     d                                     prefix( k_ )        inz
022700141007
022800141007       // -?File FNBLP01L / FNARB01L?
022900160518     d keyFNBLPARB   e ds                  extname( FNBLP01L : *key )
023000160518     d                                     prefix( k_ARB : 3 ) inz
023100160831
023200160831       // -?File TITAS30C?
023300160831     d keyTITAS30    e ds                  extname( TITAS30C : *key )
023400160831     d                                     prefix( k_ )  inz
023500160831       // -?File TITAA30C?
023600160831     d keyTITAA30    e ds                  extname( TITAA30C : *key )
023700160831     d                                     prefix( k_ )  inz
023800160831       // -?File TITA430C?
023900160831     d keyTITA430    e ds                  extname( TITA430C : *key )
024000160831     d                                     prefix( k_ )  inz
024100160516
024200160516       // -?File TFNTC01L?
024300160518     d keyTFNTC01    e ds                  extname( TFNTC01L : *key )
024400160518     d                                     prefix( k_ )        inz
024500141007
024600141007       //--------------------------------------------------------------
024700141007       //?Riepilogo indicatori.                                        ?
024800141007       //--------------------------------------------------------------
024900141007       //--------------------------------------------------------------
025000141007
025100141007       //--------------------------------------------------------------
025200141007       //?M A I N - L I N E                                            ?
025300141007       //--------------------------------------------------------------
025400141007
025500141007     c     *Entry        plist
025600141007     c                   parm                    KPJBA
025700141007
025800141007      /free
025900141007
026000141007       // -?Operazioni iniziali?
026100141007       exsr  sr_RoutInz;
026200141007
026300141007       // -?Preparazione della e-mail da inviare?
026400141007       if  o00err = *blank;
026500141007         exsr sr_PrintMail;
026600141007       endif;
026700141007
026800141007       // -?Operazioni finali?
026900141007       exsr  sr_RoutEnd;
027000141007
027100141007       //--------------------------------------------------------------
027200141007       //?Operazioni iniziali.                                         ?
027300141007       //--------------------------------------------------------------
027400141007       BEGSR  sr_RoutInz;
027500141007
027600141007         *inLR  = *on;
027700141007
027800141007         // -?Ricezione parametri?
027900141007         FIDN00ds = kpjbu;
028000141007         clear  o00f12;
028100141007         clear  o00f03;
028200141007         clear  o00err;
028300141007         clear  o00msg;
028400141007
028500141007         // -?Reperimento dati job?
028600141007         exsr  sr_DatiJob;
028700141007
028800141007         // -?Reperimento tab. "MRA"?
028900141007         exsr  sr_Rep_TabMRA;
029000141007         if  o00err = 'E';
029100141007           leavesr;
029200141007         endif;
029300141007
029400141007         // -?Verifica dei parametri ricecvuti?
029500141007         if  i00aac = *zero   or
029600141007             i00fil = *zero   or
029700141007             i00nca = *zero;
029800141007           o00err = 'E';
029900141007           o00msg = 'Manca il numero C.A.!!!';
030000141007           leavesr;
030100141007         endif;
030200141007
030300141007         // -?Reperimento dati testata C.A.?
030400141007         exsr  sr_Rep_FNDCT;
030500141007         if  o00err = 'E';
030600141007           leavesr;
030700141007         endif;
030800141007
030900141007         // -?Decodifica filiale gestione?
031000160518         chain  ( i00fgs )  AZORG;
031100160518         if  NOT  %found( AZORG01L );
031200141007           o00err = 'E';
031300141007           o00msg = 'Filiale gestione errata';
031400141007           leavesr;
031500141007         endif;
031600141007
031700141007         // -?Reperimento dati spedizione?
031800160831         if  I00tpb = 'S';
031900160831           open  TITAS30C;
032000160831           open  TITAA30C;
032100160831           open  TITA430C;
032200160831           exsr  sr_rep_TITAS;
032300160831         else;
032400160831           open  FNARB01L;
032500160831           open  FNBLP01L;
032600160831           exsr  sr_Rep_FNBLPARB;
032700160831         endif;
032800160517         if  o00err = 'E';
032900160517           leavesr;
033000160517         endif;
033100160516
033200160516         // -?Reperimento partner?
033300160517         clear  Wrag;
033400160517         clear  Weml;
033500160518
033600160516         clear  FIDN40ds;
033700160831         if  I00tpb = 'S';
033800160831           I40dsp = ( ( TASaas * 10000 ) + TASmgs );
033900160831         else;
034000160831           I40dsp = ( ( ARBaas * 10000 ) + ARBmgs );
034100160831         endif;
034200160516         if  §DCTtisp = 'E';
034300160516           I40fil = DCTlna;
034400160516         endif;
034500160516
034600160516         FIDN40R ( FIDN40ds );
034700160516
034800160517         if  O40cod = *zeros;
034900160519           o00err = 'E';
035000160519           o00msg = 'Codice partner NON reperito';
035100160517           leavesr;
035200160517         endif;
035300160517
035400160517         // -?Decodifica partner?
035500160518         clear  TIBS69ds;
035600160518         I69tla = 'L';
035700160518         I69kac = O40cod;
035800160518         tibs69r( TIBS69ds :
035900160518                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
036000160518
036100160518         if  O69err = *blanks;
036200160518           Wrag = ACOrag;
036300160518         else;
036400160518           o00err = 'E';
036500160518           o00msg = 'Codice partner errato (' + %editc( O40cod : 'X' )
036600160518                  +                       ')';
036700160518           leavesr;
036800160518         endif;
036900160516
037000160517         // -?Reperimento indirizzo e-mail del partner?
037100160517         k_NTCapl = 'C' ;
037200160517         k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( O40cod : 'X' );
037300160517         clear  k_NTCnk2;
037400160517         k_NTCtnt = '87';
037500160517
037600160518         chain  %kds( keyTFNTC01 : 4 )  TFNTC;
037700160517
037800160518         if  %found( TFNTC01L )  and  NTCrnt <> *blanks;
037900160517           Weml = NTCrnt;
038000160517         else;
038100160519           o00err = 'E';
038200160519           o00msg = 'Indirizzo e-mail del partner NON reperito';
038300160517           leavesr;
038400160517         endif;
038500141007
038600141007       ENDSR;
038700141007
038800141007       //--------------------------------------------------------------
038900141007       //?Reperimento Dati del job (Utente/Operativi).                 ?
039000141007       //--------------------------------------------------------------
039100141007       BEGSR  sr_DatiJob;
039200141007
039300141007         in(E) §AzUte;
039400141007         if NOT %error;
039500141007           in(E) §DatiUte;
039600141007         endif;
039700141007         if %error or RSut = *blanks;
039800141007           clear TIBS34ds;
039900141007           tibs34r(tibs34ds);
040000141007           in §AzUte;
040100141007           in §DatiUte;
040200141007         endif;
040300141007
040400141007       ENDSR;
040500141007
040600141007       //--------------------------------------------------------------
040700141007       //?Reperimento dati testata C.A.                                ?
040800141007       //--------------------------------------------------------------
040900141007       BEGSR  sr_Rep_FNDCT;
041000141007
041100141007         clear  keyFNDCT01;
041200141007         k_DCTaac = i00aac;
041300141007         k_DCTfil = i00fil;
041400141007         k_DCTnca = i00nca;
041500141007
041600141007         chain  %kds( keyFNDCT01 )  FNDCT000;
041700141007
041800141007         if  NOT  %found(FNDCT01L)   or
041900141007             DCTatb <> *blank;
042000141007           o00err = 'E';
042100141007           o00msg = 'C.A. non reperita';
042200141007           leavesr;
042300141007         endif;
042400141007
042500141007         dDCT01 = DCTflo;
042600141007
042700160830         if  ( DCTfca <> c_FCA_Conf  and
042800160830               I00dsb <= *zero )     or
042900141007             §DCTtisp <> 'E'         or
043000141007             §DCTeuro <> 'X'         or
043100141007             DCTres   <> 'P';
043200141007           o00err = 'E';
043300160519           o00msg = 'C.A. non contestabile a EuroExpress';
043400141007           leavesr;
043500141007         endif;
043600160519
043700160519         if  DCTlna <> 311  and
043800160519             DCTlna <> 333  and
043900160519             DCTlna <> 334  and
044000160519             DCTlna <> 340  and  DCTlna <> 345;
044100160519           o00err = 'E';
044200160519           o00msg = 'C.A. non contestabile a EuroExpress';
044300160519           leavesr;
044400160519         endif;
044500141007
044600141007         // -?Impostazione numero spedizione tra i parms SE non ricevuto?
044700141007         if  i00aas = *zero   or
044800141007             i00lnp = *zero   or
044900141007             i00nsp = *zero;
045000141007           i00aas = DCTaas;
045100141007           i00lnp = DCTlnp;
045200141007           i00nrs = DCTnrs;
045300141007           i00nsp = DCTnsp;
045400141007         endif;
045500141007
045600141007         // -?Impostazione altri dati tra i parms SE non ricevuti?
045700141007         if  i00fca = *zero;
045800141007           i00fca = DCTfca;
045900141007         endif;
046000141007         if  i00fgs = *zero;
046100141007           i00fgs = DCTgfc;
046200141007         endif;
046300141007         if  i00tad = *blank;
046400141007           i00tad = DCTtad;
046500141007         endif;
046600141007
046700141007       ENDSR;
046800160831
046900160831       //--------------------------------------------------------------
047000160831       //?Reperimento dati spedizione di Sede.                         ?
047100160831       //--------------------------------------------------------------
047200160831       BEGSR  sr_Rep_TITAS;
047300160831
047400160831         clear  keyTITAS30;
047500160831         k_TASaas = I00aas;
047600160831         k_TASlnp = I00lnp;
047700160831         k_TASnrs = I00nrs;
047800160831         k_TASnsp = I00nsp;
047900160831         chain  %kds( keyTITAS30 : 4 )  TITAS30C;
048000160831         if  NOT  %found( TITAS30C );
048100160831           o00err = 'E';
048200160831           o00msg = 'Spedizione non reperita';
048300160831           leavesr;
048400160831         endif;
048500160831
048600160831         // -?Decodifica Mittente?
048700160831         // ·?SE non codificato: aggancia TITAA?
048800160831         IF  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
048900160831             %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
049000160831
049100160831           clear  keyTITAA30;
049200160831           k_TAAaas = I00aas;
049300160831           k_TAAlnp = I00lnp;
049400160831           k_TAAnrs = I00nrs;
049500160831           k_TAAnsp = I00nsp;
049600160831           k_TAAtrc = 'M';
049700160831           chain  %kds( keyTITAA30 )  TITAA30C;
049800160831           if  NOT %found( TITAA30C );
049900160831             o00err = 'E';
050000160831             o00msg = 'Mittente non reperito';
050100160831             leavesr;
050200160831           endif;
050300160831
050400160831         // ·?SE codificato: aggancia Anagrafica?
050500160831         ELSE;
050600160831
050700160831           clear  TIBS69ds;
050800160831           I69sif = knsif;
050900160831           I69kcc = DUTkci;
051000160831           I69kac = TASccm;
051100160831           tibs69r( TIBS69ds :
051200160831                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
051300160831           If  O69err <> *blank;
051400160831             o00err = 'E';
051500160831             o00msg = 'Mittente non reperito';
051600160831             leavesr;
051700160831           EndIf;
051800160831
051900160831         ENDIF;
052000160831
052100160831         // -?Reperimento dati dal *file TITA4 / trk "A"?
052200160831         //  ?(Rif.Mitt.Alfanum.)?
052300160831         clear  dTA4a;
052400160831         clear  keyTITA430;
052500160831         k_TA4aas = I00aas;
052600160831         k_TA4lnp = I00lnp;
052700160831         k_TA4nrs = I00nrs;
052800160831         k_TA4nsp = I00nsp;
052900160831         k_TA4trc = 'A';
053000160831         chain  %kds( keyTITA430 )  TITA430C;
053100160831         if  %found( TITA430C );
053200160831           dTA4a   = TA4not;
053300160831         endif;
053400160831
053500160831       ENDSR;
053600141007
053700141007       //--------------------------------------------------------------
053800160831       //?Reperimento dati spedizione di Filiale.                      ?
053900141007       //--------------------------------------------------------------
054000141007       BEGSR  sr_Rep_FNBLPARB;
054100141007
054200141021         clear  keyFNBLPARB;
054300141021         k_ARBaas = i00aas;
054400141021         k_ARBlnp = i00lnp;
054500141021         k_ARBnrs = i00nrs;
054600141021         k_ARBnsp = i00nsp;
054700141021
054800141007         If  i00tpb = 'P';
054900141007
055000141007           chain  %kds( keyFNBLPARB )  FNBLP000;
055100141007
055200141007           if  NOT  %found(FNBLP01L)   or
055300141007               ARBatb <> *blank;
055400141007             o00err = 'E';
055500141007             o00msg = 'Spedizione non reperita';
055600141007             leavesr;
055700141007           endif;
055800141007
055900141007         Else;
056000141007
056100141007           chain  %kds( keyFNBLPARB )  FNARB000;
056200141007
056300141007           if  NOT  %found(FNARB01L)   or
056400141007               ARBatb <> *blank;
056500141007             o00err = 'E';
056600141007             o00msg = 'Spedizione non reperita';
056700141007             leavesr;
056800141007           endif;
056900141007
057000141007         EndIf;
057100141007
057200141007       ENDSR;
057300141007
057400141007       //--------------------------------------------------------------
057500141007       //?Stampa della lettera da inviare via e-mail.                  ?
057600141007       //--------------------------------------------------------------
057700141007       BEGSR  sr_PrintMail;
057800141007
057900141007         // -?Override al file di stampa ed apertura dello stesso?
058000141007         exsr  sr_Override;
058100141007
058200141007         // -?Impostazione dei campi in stampa?
058300141007         exsr  sr_RieField;
058400141007
058500141007         // -?Impostazione indicatori utilizzati nel PrtF?
058600141007         *in42  = (PT1da2 <> *blanks);
058700141007         *in43  = (PT1qtd <> *blanks);
058800141007         *in44  = (PT1dp2 <> *blanks);
058900141007         *in47  = (ARBrma <> *blanks);
059000141007
059100141007         // -?Stampa?
059200160518         write  DN68T01;
059300141007
059400141007         // -?Chiusura file stampa & Cancellazione override?
059500141007         exsr  sr_DeleteOvr;
059600141007
059700141007       ENDSR;
059800141007
059900141007       //--------------------------------------------------------------
060000141007       //?Esecuzione override ed apertura del file di stampa.          ?
060100141007       //--------------------------------------------------------------
060200141007       BEGSR  sr_Override;
060300141007
060400141007         //clear  $Invio;
060500141007         reset  TRTCM1ds;
060600141007
060700141007         // -?Esecuzione override?
060800160817         If  I00mod   <> 'R'     and
060900160817             §MRADreg <> *blank;
061000141007
061100141007           §CM1mitt = %trim(§MRADmitt);
061200160517           §CM1dst  = Weml;
061300141007           §CM1tips = §MRADreg;
061400141007           §CM1po   = %editc( DUTpou : 'X' );
061500141007           §CM1var  = '*OBJM*'
061600141007                    + 'Damage comunication n. '
061700160518                    + %editw( I00fil : '0   ' )
061800141007                    + '/'
061900141007                    + %trim( %editc( I00nca : 'Z' ))
062000141007                    + ' '
062100141007                    + %subst( %char( I00aac ) : 3 : 2 );
062200141007           §CM1idp  = §MRADidpro;
062300141007
062400141007           Qcmd = c_CmdOvrPrtF
062500160518                + ' outq(' + %trim( §MRADoutqi ) + ')'
062600141007                + ' usrdfndta(''' + TRTCM1ds + ''')';
062700141007           exsr  sr_ExecCmd;
062800141007           //$Invio = (Qusei = *blank);
062900141007
063000141007         EndIf;
063100141007
063200141007         // -?Apertura file di stampa?
063300160518         open  FIDN68P;
063400141007
063500141007       ENDSR;
063600141007
063700141007       //--------------------------------------------------------------
063800141007       //?Impostazione dei campi in stampa.                            ?
063900141007       //--------------------------------------------------------------
064000141007       BEGSR  sr_RieField;
064100141007
064200160518         clear  DN68T01;
064300141007
064400141007         // -?Intestazione Destinatario?
064500160517         PT1rgs = wRag;
064600160817
064700160817         // -?Data (in formato gg/mm/aaaa)?
064800160830         //*·if  I00dsb <= *zero;
064900160830         //*·  I00dsb = %int( %subst( %char( %dec( %timestamp() ) )
065000160830         //*·                         : 1 : 8 ) );
065100160830         //*·endif;
065200160817         reset WLBdat;
065300160830         if  I00dsb > *zero;
065400160830           G02inv = I00dsb;
065500160830           xsrda8(wlbdat);
065600160830         endif;
065700160817         if  G02dat = *zero;
065800160817           G02dat = *date;
065900160817         endif;
066000141007
066100141007         // -?Località + Data?
066200141007         if  ORGfl1 = 'E'  and  ORGfva = *blank;
066300160817           PT1dal = 'MILANO, ' + %editc( G02dat : 'Y' );
066400141007         else;
066500160817           PT1dal = %trim( ORGdes ) + ', ' + %editc( G02dat : 'Y' );
066600141007         endif;
066700141007
066800141007         // -?Oggetto?
066900141007         PT1nca = %trim( %editw( I00fil:'0   ' )) + '/'
067000141007                + %trim( %editc( I00nca:'Z' ));
067100141007         PT1aac = I00aac;
067200141007
067300141007         // -?Dati relativi alla Spedizione?
067400141007         PT1lnp = I00lnp;
067500141007         PT1nrs = I00nrs;
067600141007         PT1nsp = I00nsp;
067700141007         reset WLBdat;
067800160831         if  I00tpb = 'S';
067900160831           G02inv = ( I00aas * 10000 ) + TASmgs;
068000160831         else;
068100160831           G02inv = ( I00aas * 10000 ) + ARBmgs;
068200160831         endif;
068300141007         xsrda8 ( WLBdat );
068400141007         PT1dat = G02dat;
068500160831         if  I00tpb = 'S';
068600160831           PT1ncl = TASncl;
068700160831           PT1pkf = TASpkf;
068800160831         else;
068900160831           PT1ncl = ARBncl;
069000160831           PT1pkf = ARBpkf;
069100160831         endif;
069200141007
069300141007         // -?Dati relativi al Mittente?
069400160831         if  I00tpb = 'S';
069500160831           if  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
069600160831               %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
069700160831             PT1rsm = TAArsc;
069800160831           else;
069900160831             PT1rsm = ACOrag;
070000160831           endif;
070100160831           PT1rmn = TASrmn;
070200160831           PT1rma = dTA4a.§TA4Arma;
070300160831         else;
070400160831           PT1rsm = ARBrsm;
070500160831           PT1rmn = ARBrmn;
070600160831           PT1rma = ARBrma;
070700160831         endif;
070800141007
070900141007         // -?Dati relativi al Destinatario?
071000160831         if  I00tpb = 'S';
071100160831           PT1rsd = TASrsd;
071200160831           PT1lod = TASlod;
071300160831           if  TASnzd <> *blank;
071400160831             PT1prn = TASnzd;
071500160831           else;
071600160831             PT1prn = TASprd;
071700160831           endif;
071800160831         else;
071900160831           PT1rsd = ARBrsd;
072000160831           PT1lod = ARBlod;
072100160831           if  ARBnzd <> *blank;
072200160831             PT1prn = ARBnzd;
072300160831           else;
072400160831             PT1prn = ARBprd;
072500160831           endif;
072600160831         endif;
072700141007
072800141007         // -?Anomalia?
072900141007         Wtad = I00tad;
073000141007         exsr  sr_Rep_TabTAD;
073100141007         select;
073200141007           when  §TADdesi <> *blank;
073300141007             PT1tad = §TADdesi;
073400141007           when  §TADdest <> *blank;
073500141007             PT1tad = §TADdest;
073600141007           other;
073700141007             PT1tad = §TADdesc;
073800141007         endsl;
073900141007         // -?Descrizione anomalia?
074000141007         exsr  sr_Rep_DesTAD;
074100141007
074200141007         // -?Evento?
074300141007         IF  DCTnev > *zero;
074400141007           PT1kev = %trim( %editw( DCTaae : '0    ' )) + '/'
074500141007                  + %trim( %editc( DCTnev : 'Z' ));
074600141007           chain (DCTaae : DCTnev) FNDET000;
074700141007           If  %found(FNDET01L);
074800141007             Wtad = DETtad;
074900141007             exsr  sr_Rep_TabTAD;
075000141007             if  §TADdest <> *blank;
075100141007               PT1tae = §TADdest;
075200141007             else;
075300141007               PT1tae = §TADdesc;
075400141007             endif;
075500141007           EndIf;
075600141007         ENDIF;
075700141007
075800141007         // -?Quantità danneggiate?
075900141007         //  ?·Peso?
076000141007         if  DCTpkd > *zero;
076100141007           PT1qtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
076200141007         endif;
076300141007         //  ?·Colli?
076400141007         if  DCTncn > *zero;
076500141007           PT1qtd = %trim(PT1qtd)
076600141007                  + ' Parcels ' + %trim( %editc( DCTncn : 'Z' ));
076700141007         endif;
076800141007
076900141007         // -?Pezzi?
077000141007         //  ?+ Descrizione pezzi danneggiati?
077100141007         if  DCTnpz > *zero;
077200141007           PT1qtd  = %trim(PT1qtd)
077300141007                   + ' Pieces ' + %trim( %editc( DCTnpz : 'Z' ));
077400141007           exsr  sr_Rep_DesPZD;
077500141007         endif;
077600141007
077700141007       ENDSR;
077800141007
077900141007       //--------------------------------------------------------------
078000141007       //?Chiusura file stampa & Cancellazione override.               ?
078100141007       //--------------------------------------------------------------
078200141007       BEGSR  sr_DeleteOvr;
078300141007
078400160518         if  %open(FIDN68P);
078500141007
078600160518           close  FIDN68P;
078700141007
078800160817           if  I00mod   <> 'R'     and
078900160817               §MRADreg <> *blank;
079000160817             Qcmd = c_CmdDltOvr;
079100160817             exsr  sr_ExecCmd;
079200160817           endif;
079300141007
079400141007         endif;
079500141007
079600141007       ENDSR;
079700141007
079800141007       //--------------------------------------------------------------
079900141007       //?Reperimento tab. MRA = Bart-Maling - Danni.                  ?
080000141007       //--------------------------------------------------------------
080100141007       BEGSR  sr_Rep_TabMRA;
080200141007
080300141007         clear  dMRAdan;
080400160519
080500160519         clear  TIBS02ds;
080600160519         T02mod = 'C';
080700160519         T02cod = 'MRA';
080800160519         T02ke1 = %trimR( SDSpgm );
080900160519         T02ke2 = 'DAN';
081000160519
081100160519         TNTBE_RicercaControllo ( kpjba : tibs02ds );
081200141007
081300141007         if T02err = *blanks;
081400141007           dMRAdan = T02uni;
081500141007         else;
081600141007           o00err = 'E';
081700141007           o00msg = 'Manca tab. MRA'
081800141007                  + ' "' + %trimr(T02ke1) + '" /'
081900141007                  + ' "' + %trimr(T02ke2) + '"';
082000141007           leavesr;
082100141007         endif;
082200141007
082300141007       ENDSR;
082400141007
082500141007       //--------------------------------------------------------------
082600141007       //?Reperimento tab. TAD = Tipo Anomalia.                        ?
082700141007       //--------------------------------------------------------------
082800141007       BEGSR  sr_Rep_TabTAD;
082900141007
083000141007         clear  dTAD;
083100141007
083200141007         clear  TIBS02ds;
083300141007         T02mod = 'C';
083400141007         T02sif = knsif;
083500141007         T02cod = 'TAD';
083600141007         T02ke1 = Wtad;
083700141007
083800141007         TNTBE_RicercaControllo ( kpjba : tibs02ds );
083900141007
084000141007         if T02err = *blanks;
084100141007           dTAD = T02uni;
084200141007         endif;
084300141007
084400141007       ENDSR;
084500141007
084600141007       //--------------------------------------------------------------
084700141007       //?Reperimento descrizione anomalia.                            ?
084800141007       //--------------------------------------------------------------
084900141007       BEGSR  sr_Rep_DesTAD;
085000141007
085100141007         clear  YY;
085200141007         clear  sk_TAD;
085300141007
085400141007         clear  keyFNDCS01;
085500141007         k_DCStpd = 'C';
085600141007         k_DCSnkt = wNumCA;
085700141007         k_DCSstd = 'A';
085800141007
085900141007         setll  %kds( keyFNDCS01 : 3 )  FNDCS000;
086000141007         reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
086100141007
086200141007         DoW  NOT  %eof(FNDCS01L)   and
086300141007              YY < %elem(sk_TAD);
086400141007           yy += 1;
086500141007           sk_TAD(yy) = DCSnot;
086600141007           reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
086700141007         EndDo;
086800141007
086900141007         PT1da1 = sk_TAD(1);
087000141007         PT1da2 = sk_TAD(2) + sk_TAD(3);
087100141007
087200141007       ENDSR;
087300141007
087400141007       //--------------------------------------------------------------
087500141007       //?Reperimento descrizione pezzi danneggiati/mancanti.          ?
087600141007       //--------------------------------------------------------------
087700141007       BEGSR  sr_Rep_DesPZD;
087800141007
087900141007         clear  YY;
088000141007         clear  sk_TAD;
088100141007
088200141007         clear  keyFNDCS01;
088300141007         k_DCStpd = 'C';
088400141007         k_DCSnkt = wNumCA;
088500141007         k_DCSstd = 'P';
088600141007
088700141007         setll  %kds( keyFNDCS01 : 3 )  FNDCS000;
088800141007         reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
088900141007
089000141007         DoW  NOT  %eof(FNDCS01L)   and
089100141007              YY < %elem(sk_TAD);
089200141007           yy += 1;
089300141007           sk_TAD(yy) = DCSnot;
089400141007           reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
089500141007         EndDo;
089600141007
089700141007         PT1dpz = sk_TAD(1);
089800141007         PT1dp2 = sk_TAD(2) + sk_TAD(3);
089900141007
090000141007       ENDSR;
090100141007
090200141007       //--------------------------------------------------------------
090300141007       //?Esecuzione del comando (già impostato).                      ?
090400141007       //--------------------------------------------------------------
090500141007       BEGSR  sr_ExecCmd;
090600141007
090700141007         clear Qcap0100;
090800141007         Qcabcsdh = *off;
090900141007         Qcapa    = *off;
091000141007         Qcacmdss = *off;
091100141007         Qcaerved = *allX'00';
091200141007
091300141007         clear Qusec;
091400141007         Qusbprv  = %size(Qusec);
091500141007
091600141007         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
091700141007                           %size(Qcap0100) : 'CPOP0100' : *omit :
091800141007                           0 : 0 : Qusec);
091900141007
092000141007         //if  Qusei <> *blank;
092100141007         //  ...;
092200141007         //endif;
092300141007
092400141007       ENDSR;
092500141007
092600141007       //--------------------------------------------------------------
092700141007       //?Operazioni finali.                                           ?
092800141007       //--------------------------------------------------------------
092900141007       BEGSR  sr_RoutEnd;
093000160517
093100160517         // -?Chiusura funzioni precedentemente aperte?
093200160517         reset  TIBS69ds;
093300160517         //I69tla = 'C';         ?(già così)?
093400160517         tibs69r( tibs69ds :
093500160517                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
093600141007
093700141007         // -?Restituzione parametri?
093800141007         kpjbu = FIDN00ds;
093900141007
094000141007         // -?Uscita dal *pgm?
094100141007         return;
094200141007
094300141007       ENDSR;
094400141007
094500141007      /end-free
