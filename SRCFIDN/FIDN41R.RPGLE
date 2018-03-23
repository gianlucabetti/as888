000100141007       //==============================================================
000200141007       //?INVIO E-MAIL DI CONTESTAZIONE UK                             ?
000300141007       //==============================================================
000400160817       //?Richiamato da FIDN03R & FIDN78R x le C.A. con le seguenti    ?
000500160817       //?caratteristiche:                                             ?
000600141007       //?1) Fase 100                                                  ?
000700141007       //?2) Bolla Export (§DCTTISP di DCTFLO = "E")                   ?
000800141007       //?3) EuroExpress  (§DCTEURO di DCTFLO = "X")                   ?
000900141007       //?   FedEx        (§DCTEURO di DCTFLO = "F") - NO !            ?
001000141007       //?4) Responsabilità "PARTNER"                                  ?
001100141007       //?5) LNA 350                                                   ?
001200141007       //==============================================================
001300141007
001400141007       //--------------------------------------------------------------
001500141007       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001600141007       //--------------------------------------------------------------
001700141007
001800141007     /*PRM  dbgview(*source)
001900160831     /*CMD  ovrdbf file(FNARB01L) tofile(FILTRAPRD/FNARB01L) +
002000160831     /*CMD         ovrscope(*calllvl)
002100160831     /*CMD  ovrdbf file(FNBLP01L) tofile(FILTRAPRD/FNBLP01L) +
002200160831     /*CMD         ovrscope(*calllvl)
002300160831     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
002400160831     /*CMD         ovrscope(*calllvl)
002500160831     /*CMD  ovrdbf file(TITAA30C) tofile(GAITRAGRPS/TITAA30C) +
002600160831     /*CMD         ovrscope(*calllvl)
002700160831     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
002800160831     /*CMD         ovrscope(*calllvl)
002900160831     /*END  dltovr file(*all) lvl(*)
003000141007     /*END
003100141007
003200141007       //--------------------------------------------------------------
003300141007       //?Specifiche di controllo.                                     ?
003400141007       //--------------------------------------------------------------
003500141007
003600141007     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003700141007     h dftactgrp(*no)
003800141007
003900141007       //--------------------------------------------------------------
004000141007       //?Dichiarazione file.                                          ?
004100141007       //--------------------------------------------------------------
004200141007
004300141007       // -?DANNI: Testata C.A.?
004400141007     fFNDCT01L  if   e           k disk
004500141007       // -?DANNI: Testata Eventi/Denunce?
004600141007     fFNDET01L  if   e           k disk
004700141007       // -?DANNI: Descrizioni?
004800141007     fFNDCS01L  if   e           k disk
004900141007
005000141007       // -?BOLLE ARRIVI  : Testata?
005100160831     fFNARB01L  if   e           k disk    usropn
005200141007       // -?BOLLE PARTENZA: Testata?
005300160831     fFNBLP01L  if   e           k disk    usropn
005400160831     f                                     prefix(ARB:3)
005500160831
005600160831       // -?BOLLE SEDE: da Fatturare + fatturate "ultimi mesi"?
005700160831       //  ?                         + fatturate "storico"?
005800160831     fTITAS30C  if   e           k disk    usropn
005900160831       // -?BOLLE SEDE: Anagrafiche (con "Storico")?
006000160831     fTITAA30C  if   e           k disk    usropn
006100160831       // -?BOLLE SEDE: Riferimenti bolla (con "Storico")?
006200160831     fTITA430C  if   e           k disk    usropn
006300141007
006400141007       // -?Organigramma Filiale/Aziendale?
006500141007     fAZORG01L  if   e           k disk
006600141007
006700141007       // -?Stampa lettera (e-mail)?
006800141007     fFIDN41P   o    e             printer usropn
006900141007     f                                     OFlind(*in01)
007000141007
007100141007       //--------------------------------------------------------------
007200141007       //?Definizione costanti.                                        ?
007300141007       //--------------------------------------------------------------
007400141007
007500141007       // -?Fase "Conferma C.A."?
007600141007     d c_FCA_Conf      c                   const(100)
007700141007
007800141007       // -?Comandi di override al PrtF?
007900141007     d c_CmdOvrPrtF    c                   const('OVRPRTF +
008000141007     d                                           file(FIDN41P) +
008100141007     d                                           ovrscope(*actgrpdfn) +
008200141007     d                                           ')
008300141007     d c_CmdDltOvr     c                   const('DLTOVR +
008400141007     d                                            file(FIDN41P) +
008500141007     d                                            lvl(*actgrpdfn)')
008600141007
008700141007       //--------------------------------------------------------------
008800141007       //?Definizione schiere.                                         ?
008900141007       //--------------------------------------------------------------
009000141007
009100141007       // -?Descrizione Tipo Anomalia?
009200141007     d sk_TAD          s                   dim( 3) like(DCSnot)  inz            descr. tipo anomalia
009300141007
009400141007       //--------------------------------------------------------------
009500141007       //?Definizione aree dati.                                       ?
009600141007       //--------------------------------------------------------------
009700141007
009800141007       // -?Dati utente?
009900141007     d §AzUte        e ds                  extname(AZUTE00F)
010000141007     d                                     dtaara
010100141007     d §DatiUte      e ds                  extname(dDatiUte)
010200141007     d                                     dtaara
010300141007
010400141007       //--------------------------------------------------------------
010500141007       //?Definizione strutture dati.                                  ?
010600141007       //--------------------------------------------------------------
010700141007
010800141007       // -?Parametri ricevuti?
010900141007     d KPJBA         e ds
011000141007     d FIDN00ds      e ds                  inz
011100141007     d   wNumCA                6     19
011200141007
011300141007       // -?Flag operativi in FNDCT?
011400141007     d dDCT01        e ds                  inz
011500160831
011600160831       // -?TITA4 tipo record "A"?
011700160831     d dTA4a         e ds                  inz   qualified
011800141007
011900141007       // -?Tab. MRA/DAN = Bart-Maling - Danni?
012000141007     d dMRAdan       e ds
012100141007       // -?Tab. TAD = Tipi anomalia danni?
012200141007     d dTAD          e ds                  inz
012300141007
012400141007       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
012500141007     d TRTCM1ds      e ds                  inz
012600141007       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
012700141007     d   §CM1mitt    e                     inz('servizi.assicurativi')
012800141007       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
012900141007     d   §CM1dst     e                     inz('servizi.assicurativi@brt.it')
013000141007       //  ?·§CM1tips = Tipo lettera via e-mail:?
013100141007       //   ?"LET" = testo allegato in corpo con logo?
013200141007       //           ?(richiede righe libere iniziali per il logo)?
013300141007       //   ?"COR" = testo integrato senza logo?
013400141007       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
013500141007     d   §CM1tips    e                     inz('E05')
013600141007       //  ?·§CM1po   = Filiale?
013700141007     d   §CM1po      e                     inz('046')
013800141007       //  ?·§CM1var  = Oggetto e-mail?
013900141007     d   §CM1var     e                     inz('*OBJM*+
014000141007     d                                     Mail CONTESTAZIONE UK')
014100141007       //  ?·§CM1sts  = Stato?
014200141007     d   §CM1sts     e                     inz(*off)
014300141007       //  ?·§CM1sts  = Id processo?
014400141007     d   §CM1idp     e                     inz('1')
014500141007
014600141007       // -?Status?
014700141007     d Psds           sds
014800141007     d   SDSpgm          *proc
014900141007
015000141007       //--------------------------------------------------------------
015100141007       //?Definizione variabili globali.                               ?
015200141007       //--------------------------------------------------------------
015300141007
015400141007       // -?Flags booleani?
015500141007     d*// $Invio          s               n   inz
015600141007
015700141007       // -?Indici di schiera?
015800141007     d YY              s              3  0 inz
015900141007
016000141007       // -?Campi di comodo?
016100141007     d Wtad            s                   like(DCTtad)   inz
016200141007
016300141007       //--------------------------------------------------------------
016400141007       //?Definizione procedure usate e relativi parametri.            ?
016500141007       //--------------------------------------------------------------
016600141007
016700141007       // -?Reperimento dati utente?
016800141007     d TIBS34ds      e ds
016900141007      /copy gaitrasrc/srcProtoPR,TIBS34R
017000141007
017100141007       // -?Ricerca/Controllo tabelle?
017200141007     d TIBS02ds      e ds                  inz
017300141007      /copy gaitrasrc/srcProtoPR,TIBS02R
017400160831
017500160831       // -?Controllo/Decodifica cliente?
017600160831      /copy gaitrasrc/srcProtoPI,TIBS69R
017700160831      /copy gaitrasrc/srcProtoPR,TIBS69R
017800141007
017900141007       // -?Controllo & Inversione date?
018000141007     d WLBdat          ds                  inz
018100141007     d   G02dat                1      8  0 inz
018200141007     d   G02inv                9     16  0 inz
018300141007     d   G02err               17     17    inz('3')
018400141007     d   G02tgi               18     22  0 inz
018500141007      /copy gaitrasrc/srcProtoPR,XSRDA8
018600141007
018700141007       // -?API QCAPCMD (Process Commands)?
018800141007     d Qcmd            s           2048    inz  varying
018900141007      /copy qSysInc/qRpgleSrc,QCAPCMD
019000141007      /copy gaitrasrc/srcProtoPR,QCAPCMD
019100141007
019200141007       // -?Parametri gestione errori API.?
019300141007      /copy qsysinc/qrpglesrc,QUSEC
019400141007
019500141007       //--------------------------------------------------------------
019600141007       //?Definizione key-list.                                        ?
019700141007       //--------------------------------------------------------------
019800141007
019900141007       // -?File FNDCT01L?
020000141007     d keyFNDCT01    e ds                  extname(FNDCT01L:*key)
020100141007     d                                     prefix(k_)        inz
020200141007
020300141007       // -?File FNDCS01L?
020400141007     d keyFNDCS01    e ds                  extname(FNDCS01L:*key)
020500141007     d                                     prefix(k_)        inz
020600141007       // -?File FNDET01L?
020700141007     d keyFNDET01    e ds                  extname(FNDET01L:*key)
020800141007     d                                     prefix(k_)        inz
020900141007
021000141007       // -?File FNBLP01L / FNARB01L?
021100141007     d keyFNBLPARB   e ds                  extname(FNBLP01L:*key)
021200141007     d                                     prefix(k_ARB:3)   inz
021300160831
021400160831       // -?File TITAS30C?
021500160831     d keyTITAS30    e ds                  extname( TITAS30C : *key )
021600160831     d                                     prefix( k_ )  inz
021700160831       // -?File TITAA30C?
021800160831     d keyTITAA30    e ds                  extname( TITAA30C : *key )
021900160831     d                                     prefix( k_ )  inz
022000160831       // -?File TITA430C?
022100160831     d keyTITA430    e ds                  extname( TITA430C : *key )
022200160831     d                                     prefix( k_ )  inz
022300141007
022400141007       //--------------------------------------------------------------
022500141007       //?Riepilogo indicatori.                                        ?
022600141007       //--------------------------------------------------------------
022700141007       //--------------------------------------------------------------
022800141007
022900141007       //--------------------------------------------------------------
023000141007       //?M A I N - L I N E                                            ?
023100141007       //--------------------------------------------------------------
023200141007
023300141007     c     *Entry        plist
023400141007     c                   parm                    KPJBA
023500141007
023600141007      /free
023700141007
023800141007       // -?Operazioni iniziali?
023900141007       exsr  sr_RoutInz;
024000141007
024100141007       // -?Preparazione della e-mail da inviare?
024200141007       if  o00err = *blank;
024300141007         exsr sr_PrintMail;
024400141007       endif;
024500141007
024600141007       // -?Operazioni finali?
024700141007       exsr  sr_RoutEnd;
024800141007
024900141007       //--------------------------------------------------------------
025000141007       //?Operazioni iniziali.                                         ?
025100141007       //--------------------------------------------------------------
025200141007       BEGSR  sr_RoutInz;
025300141007
025400141007         *inLR  = *on;
025500141007
025600141007         // -?Ricezione parametri?
025700141007         FIDN00ds = kpjbu;
025800141007         clear  o00f12;
025900141007         clear  o00f03;
026000141007         clear  o00err;
026100141007         clear  o00msg;
026200141007
026300141007         // -?Reperimento dati job?
026400141007         exsr  sr_DatiJob;
026500141007
026600141007         // -?Reperimento tab. "MRA"?
026700141007         exsr  sr_Rep_TabMRA;
026800141007         if  o00err = 'E';
026900141007           leavesr;
027000141007         endif;
027100141007
027200141007         // -?Verifica dei parametri ricecvuti?
027300141007         if  i00aac = *zero   or
027400141007             i00fil = *zero   or
027500141007             i00nca = *zero;
027600141007           o00err = 'E';
027700141007           o00msg = 'Manca il numero C.A.!!!';
027800141007           leavesr;
027900141007         endif;
028000141007
028100141007         // -?Reperimento dati testata C.A.?
028200141007         exsr  sr_Rep_FNDCT;
028300141007         if  o00err = 'E';
028400141007           leavesr;
028500141007         endif;
028600141007
028700141007         // -?Decodifica filiale gestione?
028800141007         chain  (i00fgs)  AZORG;
028900141007         if  NOT  %found(AZORG01L);
029000141007           o00err = 'E';
029100141007           o00msg = 'Filiale gestione errata';
029200141007           leavesr;
029300141007         endif;
029400141007
029500141007         // -?Reperimento dati spedizione?
029600160831         if  I00tpb = 'S';
029700160831           open  TITAS30C;
029800160831           open  TITAA30C;
029900160831           open  TITA430C;
030000160831           exsr  sr_rep_TITAS;
030100160831         else;
030200160831           open  FNARB01L;
030300160831           open  FNBLP01L;
030400160831           exsr  sr_Rep_FNBLPARB;
030500160831         endif;
030600141007
030700141007       ENDSR;
030800141007
030900141007       //--------------------------------------------------------------
031000141007       //?Reperimento Dati del job (Utente/Operativi).                 ?
031100141007       //--------------------------------------------------------------
031200141007       BEGSR  sr_DatiJob;
031300141007
031400141007         in(E) §AzUte;
031500141007         if NOT %error;
031600141007           in(E) §DatiUte;
031700141007         endif;
031800141007         if %error or RSut = *blanks;
031900141007           clear TIBS34ds;
032000141007           tibs34r(tibs34ds);
032100141007           in §AzUte;
032200141007           in §DatiUte;
032300141007         endif;
032400141007
032500141007       ENDSR;
032600141007
032700141007       //--------------------------------------------------------------
032800141007       //?Reperimento dati testata C.A.                                ?
032900141007       //--------------------------------------------------------------
033000141007       BEGSR  sr_Rep_FNDCT;
033100141007
033200141007         clear  keyFNDCT01;
033300141007         k_DCTaac = i00aac;
033400141007         k_DCTfil = i00fil;
033500141007         k_DCTnca = i00nca;
033600141007
033700141007         chain  %kds( keyFNDCT01 )  FNDCT000;
033800141007
033900141007         if  NOT  %found(FNDCT01L)   or
034000141007             DCTatb <> *blank;
034100141007           o00err = 'E';
034200141007           o00msg = 'C.A. non reperita';
034300141007           leavesr;
034400141007         endif;
034500141007
034600141007         dDCT01 = DCTflo;
034700141007
034800160830         if  ( DCTfca <> c_FCA_Conf  and
034900160830               I00dsb <= *zero )     or
035000141007             §DCTtisp <> 'E'         or
035100141007             §DCTeuro <> 'X'         or
035200141007             DCTlna   <> 350         or
035300141007             DCTres   <> 'P';
035400141007           o00err = 'E';
035500141007           o00msg = 'C.A. non contestabile a UK';
035600141007           leavesr;
035700141007         endif;
035800141007
035900141007         // -?Impostazione numero spedizione tra i parms SE non ricevuto?
036000141007         if  i00aas = *zero   or
036100141007             i00lnp = *zero   or
036200141007             i00nsp = *zero;
036300141007           i00aas = DCTaas;
036400141007           i00lnp = DCTlnp;
036500141007           i00nrs = DCTnrs;
036600141007           i00nsp = DCTnsp;
036700141007         endif;
036800141007
036900141007         // -?Impostazione altri dati tra i parms SE non ricevuti?
037000141007         if  i00fca = *zero;
037100141007           i00fca = DCTfca;
037200141007         endif;
037300141007         if  i00fgs = *zero;
037400141007           i00fgs = DCTgfc;
037500141007         endif;
037600141007         if  i00tad = *blank;
037700141007           i00tad = DCTtad;
037800141007         endif;
037900141007
038000141007       ENDSR;
038100160831
038200160831       //--------------------------------------------------------------
038300160831       //?Reperimento dati spedizione di Sede.                         ?
038400160831       //--------------------------------------------------------------
038500160831       BEGSR  sr_Rep_TITAS;
038600160831
038700160831         clear  keyTITAS30;
038800160831         k_TASaas = I00aas;
038900160831         k_TASlnp = I00lnp;
039000160831         k_TASnrs = I00nrs;
039100160831         k_TASnsp = I00nsp;
039200160831         chain  %kds( keyTITAS30 : 4 )  TITAS30C;
039300160831         if  NOT  %found( TITAS30C );
039400160831           o00err = 'E';
039500160831           o00msg = 'Spedizione non reperita';
039600160831           leavesr;
039700160831         endif;
039800160831
039900160831         // -?Decodifica Mittente?
040000160831         // ·?SE non codificato: aggancia TITAA?
040100160831         IF  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
040200160831             %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
040300160831
040400160831           clear  keyTITAA30;
040500160831           k_TAAaas = I00aas;
040600160831           k_TAAlnp = I00lnp;
040700160831           k_TAAnrs = I00nrs;
040800160831           k_TAAnsp = I00nsp;
040900160831           k_TAAtrc = 'M';
041000160831           chain  %kds( keyTITAA30 )  TITAA30C;
041100160831           if  NOT %found( TITAA30C );
041200160831             o00err = 'E';
041300160831             o00msg = 'Mittente non reperito';
041400160831             leavesr;
041500160831           endif;
041600160831
041700160831         // ·?SE codificato: aggancia Anagrafica?
041800160831         ELSE;
041900160831
042000160831           clear  TIBS69ds;
042100160831           I69sif = knsif;
042200160831           I69kcc = DUTkci;
042300160831           I69kac = TASccm;
042400160831           tibs69r( TIBS69ds :
042500160831                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
042600160831           If  O69err <> *blank;
042700160831             o00err = 'E';
042800160831             o00msg = 'Mittente non reperito';
042900160831             leavesr;
043000160831           EndIf;
043100160831
043200160831         ENDIF;
043300160831
043400160831         // -?Reperimento dati dal *file TITA4 / trk "A"?
043500160831         //  ?(Rif.Mitt.Alfanum.)?
043600160831         clear  dTA4a;
043700160831         clear  keyTITA430;
043800160831         k_TA4aas = I00aas;
043900160831         k_TA4lnp = I00lnp;
044000160831         k_TA4nrs = I00nrs;
044100160831         k_TA4nsp = I00nsp;
044200160831         k_TA4trc = 'A';
044300160831         chain  %kds( keyTITA430 )  TITA430C;
044400160831         if  %found( TITA430C );
044500160831           dTA4a   = TA4not;
044600160831         endif;
044700160831
044800160831       ENDSR;
044900141007
045000141007       //--------------------------------------------------------------
045100160831       //?Reperimento dati spedizione di Filiale.                      ?
045200141007       //--------------------------------------------------------------
045300141007       BEGSR  sr_Rep_FNBLPARB;
045400141007
045500141021         clear  keyFNBLPARB;
045600141021         k_ARBaas = i00aas;
045700141021         k_ARBlnp = i00lnp;
045800141021         k_ARBnrs = i00nrs;
045900141021         k_ARBnsp = i00nsp;
046000141021
046100141007         If  i00tpb = 'P';
046200141007
046300141007           chain  %kds( keyFNBLPARB )  FNBLP000;
046400141007
046500141007           if  NOT  %found(FNBLP01L)   or
046600141007               ARBatb <> *blank;
046700141007             o00err = 'E';
046800141007             o00msg = 'Spedizione non reperita';
046900141007             leavesr;
047000141007           endif;
047100141007
047200141007         Else;
047300141007
047400141007           chain  %kds( keyFNBLPARB )  FNARB000;
047500141007
047600141007           if  NOT  %found(FNARB01L)   or
047700141007               ARBatb <> *blank;
047800141007             o00err = 'E';
047900141007             o00msg = 'Spedizione non reperita';
048000141007             leavesr;
048100141007           endif;
048200141007
048300141007         EndIf;
048400141007
048500141007       ENDSR;
048600141007
048700141007       //--------------------------------------------------------------
048800141007       //?Stampa della lettera da inviare via e-mail.                  ?
048900141007       //--------------------------------------------------------------
049000141007       BEGSR  sr_PrintMail;
049100141007
049200141007         // -?Override al file di stampa ed apertura dello stesso?
049300141007         exsr  sr_Override;
049400141007
049500141007         // -?Impostazione dei campi in stampa?
049600141007         exsr  sr_RieField;
049700141007
049800141007         // -?Impostazione indicatori utilizzati nel PrtF?
049900141007         *in42  = (PT1da2 <> *blanks);
050000141007         *in43  = (PT1qtd <> *blanks);
050100141007         *in44  = (PT1dp2 <> *blanks);
050200141007         *in47  = (ARBrma <> *blanks);
050300141007
050400141007         // -?Stampa?
050500141007         write  DN41T01;
050600141007
050700141007         // -?Chiusura file stampa & Cancellazione override?
050800141007         exsr  sr_DeleteOvr;
050900141007
051000141007       ENDSR;
051100141007
051200141007       //--------------------------------------------------------------
051300141007       //?Esecuzione override ed apertura del file di stampa.          ?
051400141007       //--------------------------------------------------------------
051500141007       BEGSR  sr_Override;
051600141007
051700141007         //clear  $Invio;
051800141007         reset  TRTCM1ds;
051900141007
052000141007         // -?Esecuzione override?
052100160817         If  I00mod   <> 'R'     and
052200160817             §MRADreg <> *blank;
052300141007
052400141007           §CM1mitt = %trim(§MRADmitt);
052500141007           §CM1dst  = %trim(§MRADdest);
052600141007           §CM1tips = §MRADreg;
052700141007           §CM1po   = %editc( DUTpou : 'X' );
052800141007           §CM1var  = '*OBJM*'
052900141007                    + 'Damage comunication n. '
053000141007                    + %editw(I00fil : '0   ')
053100141007                    + '/'
053200141007                    + %trim( %editc( I00nca : 'Z' ))
053300141007                    + ' '
053400141007                    + %subst( %char( I00aac ) : 3 : 2 );
053500141007           §CM1idp  = §MRADidpro;
053600141007
053700141007           Qcmd = c_CmdOvrPrtF
053800141007                + ' outq(' + %trim(§MRADoutqi) + ')'
053900141007                + ' usrdfndta(''' + TRTCM1ds + ''')';
054000141007           exsr  sr_ExecCmd;
054100141007           //$Invio = (Qusei = *blank);
054200141007
054300141007         EndIf;
054400141007
054500141007         // -?Apertura file di stampa?
054600141007         open  FIDN41P;
054700141007
054800141007       ENDSR;
054900141007
055000141007       //--------------------------------------------------------------
055100141007       //?Impostazione dei campi in stampa.                            ?
055200141007       //--------------------------------------------------------------
055300141007       BEGSR  sr_RieField;
055400141007
055500141007         clear  DN41T01;
055600141007
055700141007         // -?Intestazione Destinatario?
055800141021         PT1rgs = 'Fed Ex UK';
055900160817
056000160817         // -?Data (in formato gg/mm/aaaa)?
056100160830         //*·if  I00dsb <= *zero;
056200160830         //*·  I00dsb = %int( %subst( %char( %dec( %timestamp() ) )
056300160830         //*·                         : 1 : 8 ) );
056400160830         //*·endif;
056500160817         reset WLBdat;
056600160830         if  I00dsb > *zero;
056700160830           G02inv = I00dsb;
056800160830           xsrda8(wlbdat);
056900160830         endif;
057000160817         if  G02dat = *zero;
057100160817           G02dat = *date;
057200160817         endif;
057300141007
057400141007         // -?Località + Data?
057500141007         if  ORGfl1 = 'E'  and  ORGfva = *blank;
057600160817           PT1dal = 'MILANO, ' + %editc(G02dat : 'Y');
057700141007         else;
057800160817           PT1dal = %trim(ORGdes) + ', ' + %editc(G02dat : 'Y');
057900141007         endif;
058000141007
058100141007         // -?Oggetto?
058200141007         PT1nca = %trim( %editw( I00fil:'0   ' )) + '/'
058300141007                + %trim( %editc( I00nca:'Z' ));
058400141007         PT1aac = I00aac;
058500141007
058600141007         // -?Dati relativi alla Spedizione?
058700141007         PT1lnp = I00lnp;
058800141007         PT1nrs = I00nrs;
058900141007         PT1nsp = I00nsp;
059000141007         reset WLBdat;
059100160831         if  I00tpb = 'S';
059200160831           G02inv = ( I00aas * 10000 ) + TASmgs;
059300160831         else;
059400160831           G02inv = (I00aas * 10000) + ARBmgs;
059500160831         endif;
059600141007         xsrda8 ( WLBdat );
059700141007         PT1dat = G02dat;
059800160831         if  I00tpb = 'S';
059900160831           PT1ncl = TASncl;
060000160831           PT1pkf = TASpkf;
060100160831         else;
060200160831           PT1ncl = ARBncl;
060300160831           PT1pkf = ARBpkf;
060400160831         endif;
060500141007
060600141007         // -?Dati relativi al Mittente?
060700160831         if  I00tpb = 'S';
060800160831           if  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
060900160831               %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
061000160831             PT1rsm = TAArsc;
061100160831           else;
061200160831             PT1rsm = ACOrag;
061300160831           endif;
061400160831           PT1rmn = TASrmn;
061500160831           PT1rma = dTA4a.§TA4Arma;
061600160831         else;
061700160831           PT1rsm = ARBrsm;
061800160831           PT1rmn = ARBrmn;
061900160831           PT1rma = ARBrma;
062000160831         endif;
062100141007
062200141007         // -?Dati relativi al Destinatario?
062300160831         if  I00tpb = 'S';
062400160831           PT1rsd = TASrsd;
062500160831           PT1lod = TASlod;
062600160831           if  TASnzd <> *blank;
062700160831             PT1prn = TASnzd;
062800160831           else;
062900160831             PT1prn = TASprd;
063000160831           endif;
063100160831         else;
063200160831           PT1rsd = ARBrsd;
063300160831           PT1lod = ARBlod;
063400160831           if  ARBnzd <> *blank;
063500160831             PT1prn = ARBnzd;
063600160831           else;
063700160831             PT1prn = ARBprd;
063800160831           endif;
063900160831         endif;
064000141007
064100141007         // -?Anomalia?
064200141007         Wtad = I00tad;
064300141007         exsr  sr_Rep_TabTAD;
064400141007         select;
064500141007           when  §TADdesi <> *blank;
064600141007             PT1tad = §TADdesi;
064700141007           when  §TADdest <> *blank;
064800141007             PT1tad = §TADdest;
064900141007           other;
065000141007             PT1tad = §TADdesc;
065100141007         endsl;
065200141007         // -?Descrizione anomalia?
065300141007         exsr  sr_Rep_DesTAD;
065400141007
065500141007         // -?Evento?
065600141007         IF  DCTnev > *zero;
065700141007           PT1kev = %trim( %editw( DCTaae : '0    ' )) + '/'
065800141007                  + %trim( %editc( DCTnev : 'Z' ));
065900141007           chain (DCTaae : DCTnev) FNDET000;
066000141007           If  %found(FNDET01L);
066100141007             Wtad = DETtad;
066200141007             exsr  sr_Rep_TabTAD;
066300141007             if  §TADdest <> *blank;
066400141007               PT1tae = §TADdest;
066500141007             else;
066600141007               PT1tae = §TADdesc;
066700141007             endif;
066800141007           EndIf;
066900141007         ENDIF;
067000141007
067100141007         // -?Quantità danneggiate?
067200141007         //  ?·Peso?
067300141007         if  DCTpkd > *zero;
067400141007           PT1qtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
067500141007         endif;
067600141007         //  ?·Colli?
067700141007         if  DCTncn > *zero;
067800141007           PT1qtd = %trim(PT1qtd)
067900141007                  + ' Parcels ' + %trim( %editc( DCTncn : 'Z' ));
068000141007         endif;
068100141007
068200141007         // -?Pezzi?
068300141007         //  ?+ Descrizione pezzi danneggiati?
068400141007         if  DCTnpz > *zero;
068500141007           PT1qtd  = %trim(PT1qtd)
068600141007                   + ' Pieces ' + %trim( %editc( DCTnpz : 'Z' ));
068700141007           exsr  sr_Rep_DesPZD;
068800141007         endif;
068900141007
069000141007       ENDSR;
069100141007
069200141007       //--------------------------------------------------------------
069300141007       //?Chiusura file stampa & Cancellazione override.               ?
069400141007       //--------------------------------------------------------------
069500141007       BEGSR  sr_DeleteOvr;
069600141007
069700141007         if  %open(FIDN41P);
069800141007
069900141007           close  FIDN41P;
070000141007
070100160817           if  I00mod   <> 'R'     and
070200160817               §MRADreg <> *blank;
070300160817             Qcmd = c_CmdDltOvr;
070400160817             exsr  sr_ExecCmd;
070500160817           endif;
070600141007
070700141007         endif;
070800141007
070900141007       ENDSR;
071000141007
071100141007       //--------------------------------------------------------------
071200141007       //?Reperimento tab. MRA = Bart-Maling - Danni.                  ?
071300141007       //--------------------------------------------------------------
071400141007       BEGSR  sr_Rep_TabMRA;
071500141007
071600141007         clear  dMRAdan;
071700141007
071800141007         clear  TIBS02ds;
071900141007         T02mod = 'C';
072000141007         //T02sif = knsif;
072100141007         T02cod = 'MRA';
072200141007         T02ke1 = %trimr(SDSpgm);
072300141007         T02ke2 = 'DAN';
072400141007
072500141007         TNTBE_RicercaControllo ( kpjba : tibs02ds );
072600141007
072700141007         if T02err = *blanks;
072800141007           dMRAdan = T02uni;
072900141007         else;
073000141007           o00err = 'E';
073100141007           o00msg = 'Manca tab. MRA'
073200141007                  + ' "' + %trimr(T02ke1) + '" /'
073300141007                  + ' "' + %trimr(T02ke2) + '"';
073400141007           leavesr;
073500141007         endif;
073600141007
073700141007       ENDSR;
073800141007
073900141007       //--------------------------------------------------------------
074000141007       //?Reperimento tab. TAD = Tipo Anomalia.                        ?
074100141007       //--------------------------------------------------------------
074200141007       BEGSR  sr_Rep_TabTAD;
074300141007
074400141007         clear  dTAD;
074500141007
074600141007         clear  TIBS02ds;
074700141007         T02mod = 'C';
074800141007         T02sif = knsif;
074900141007         T02cod = 'TAD';
075000141007         T02ke1 = Wtad;
075100141007
075200141007         TNTBE_RicercaControllo ( kpjba : tibs02ds );
075300141007
075400141007         if T02err = *blanks;
075500141007           dTAD = T02uni;
075600141007         endif;
075700141007
075800141007       ENDSR;
075900141007
076000141007       //--------------------------------------------------------------
076100141007       //?Reperimento descrizione anomalia.                            ?
076200141007       //--------------------------------------------------------------
076300141007       BEGSR  sr_Rep_DesTAD;
076400141007
076500141007         clear  YY;
076600141007         clear  sk_TAD;
076700141007
076800141007         clear  keyFNDCS01;
076900141007         k_DCStpd = 'C';
077000141007         k_DCSnkt = wNumCA;
077100141007         k_DCSstd = 'A';
077200141007
077300141007         setll  %kds( keyFNDCS01 : 3 )  FNDCS000;
077400141007         reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
077500141007
077600141007         DoW  NOT  %eof(FNDCS01L)   and
077700141007              YY < %elem(sk_TAD);
077800141007           yy += 1;
077900141007           sk_TAD(yy) = DCSnot;
078000141007           reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
078100141007         EndDo;
078200141007
078300141007         PT1da1 = sk_TAD(1);
078400141007         PT1da2 = sk_TAD(2) + sk_TAD(3);
078500141007
078600141007       ENDSR;
078700141007
078800141007       //--------------------------------------------------------------
078900141007       //?Reperimento descrizione pezzi danneggiati/mancanti.          ?
079000141007       //--------------------------------------------------------------
079100141007       BEGSR  sr_Rep_DesPZD;
079200141007
079300141007         clear  YY;
079400141007         clear  sk_TAD;
079500141007
079600141007         clear  keyFNDCS01;
079700141007         k_DCStpd = 'C';
079800141007         k_DCSnkt = wNumCA;
079900141007         k_DCSstd = 'P';
080000141007
080100141007         setll  %kds( keyFNDCS01 : 3 )  FNDCS000;
080200141007         reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
080300141007
080400141007         DoW  NOT  %eof(FNDCS01L)   and
080500141007              YY < %elem(sk_TAD);
080600141007           yy += 1;
080700141007           sk_TAD(yy) = DCSnot;
080800141007           reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
080900141007         EndDo;
081000141007
081100141007         PT1dpz = sk_TAD(1);
081200141007         PT1dp2 = sk_TAD(2) + sk_TAD(3);
081300141007
081400141007       ENDSR;
081500141007
081600141007       //--------------------------------------------------------------
081700141007       //?Esecuzione del comando (già impostato).                      ?
081800141007       //--------------------------------------------------------------
081900141007       BEGSR  sr_ExecCmd;
082000141007
082100141007         clear Qcap0100;
082200141007         Qcabcsdh = *off;
082300141007         Qcapa    = *off;
082400141007         Qcacmdss = *off;
082500141007         Qcaerved = *allX'00';
082600141007
082700141007         clear Qusec;
082800141007         Qusbprv  = %size(Qusec);
082900141007
083000141007         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
083100141007                           %size(Qcap0100) : 'CPOP0100' : *omit :
083200141007                           0 : 0 : Qusec);
083300141007
083400141007         //if  Qusei <> *blank;
083500141007         //  ...;
083600141007         //endif;
083700141007
083800141007       ENDSR;
083900141007
084000141007       //--------------------------------------------------------------
084100141007       //?Operazioni finali.                                           ?
084200141007       //--------------------------------------------------------------
084300141007       BEGSR  sr_RoutEnd;
084400141007
084500141007         // -?Restituzione parametri?
084600141007         kpjbu = FIDN00ds;
084700141007
084800141007         // -?Uscita dal *pgm?
084900141007         return;
085000141007
085100141007       ENDSR;
085200141007
085300141007      /end-free
