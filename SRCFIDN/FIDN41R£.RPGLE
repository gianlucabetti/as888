000100141007       //==============================================================
000200141007       //?INVIO E-MAIL DI CONTESTAZIONE UK                             ?
000300141007       //==============================================================
000400141007       //?Richiamato da FIDN03R x le C.A. con le segg. caratteristiche:?
000500141007       //?1) Fase 100                                                  ?
000600141007       //?2) Bolla Export (§DCTTISP di DCTFLO = "E")                   ?
000700141007       //?3) EuroExpress  (§DCTEURO di DCTFLO = "X")                   ?
000800141007       //?   FedEx        (§DCTEURO di DCTFLO = "F") - NO !            ?
000900141007       //?4) Responsabilità "PARTNER"                                  ?
001000141007       //?5) LNA 350                                                   ?
001100141007       //==============================================================
001200141007
001300141007       //--------------------------------------------------------------
001400141007       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001500141007       //--------------------------------------------------------------
001600141007
001700141007     /*PRM  dbgview(*source)
001800141007     /*END
001900141007
002000141007       //--------------------------------------------------------------
002100141007       //?Specifiche di controllo.                                     ?
002200141007       //--------------------------------------------------------------
002300141007
002400141007     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002500141007     h dftactgrp(*no)
002600141007
002700141007       //--------------------------------------------------------------
002800141007       //?Dichiarazione file.                                          ?
002900141007       //--------------------------------------------------------------
003000141007
003100141007       // -?DANNI: Testata C.A.?
003200141007     fFNDCT01L  if   e           k disk
003300141007       // -?DANNI: Testata Eventi/Denunce?
003400141007     fFNDET01L  if   e           k disk
003500141007       // -?DANNI: Descrizioni?
003600141007     fFNDCS01L  if   e           k disk
003700141007
003800141007       // -?BOLLE ARRIVI  : Testata?
003900141007     fFNARB01L  if   e           k disk
004000141007       // -?BOLLE PARTENZA: Testata?
004100141007     fFNBLP01L  if   e           k disk    prefix(ARB:3)
004200141007
004300141007       // -?Organigramma Filiale/Aziendale?
004400141007     fAZORG01L  if   e           k disk
004500141007
004600141007       // -?Stampa lettera (e-mail)?
004700141007     fFIDN41P   o    e             printer usropn
004800141007     f                                     OFlind(*in01)
004900141007
005000141007       //--------------------------------------------------------------
005100141007       //?Definizione costanti.                                        ?
005200141007       //--------------------------------------------------------------
005300141007
005400141007       // -?Fase "Conferma C.A."?
005500141007     d c_FCA_Conf      c                   const(100)
005600141007
005700141007       // -?Comandi di override al PrtF?
005800141007     d c_CmdOvrPrtF    c                   const('OVRPRTF +
005900141007     d                                           file(FIDN41P) +
006000141007     d                                           ovrscope(*actgrpdfn) +
006100141007     d                                           ')
006200141007     d c_CmdDltOvr     c                   const('DLTOVR +
006300141007     d                                            file(FIDN41P) +
006400141007     d                                            lvl(*actgrpdfn)')
006500141007
006600141007       //--------------------------------------------------------------
006700141007       //?Definizione schiere.                                         ?
006800141007       //--------------------------------------------------------------
006900141007
007000141007       // -?Descrizione Tipo Anomalia?
007100141007     d sk_TAD          s                   dim( 3) like(DCSnot)  inz            descr. tipo anomalia
007200141007
007300141007       //--------------------------------------------------------------
007400141007       //?Definizione aree dati.                                       ?
007500141007       //--------------------------------------------------------------
007600141007
007700141007       // -?Dati utente?
007800141007     d §AzUte        e ds                  extname(AZUTE00F)
007900141007     d                                     dtaara
008000141007     d §DatiUte      e ds                  extname(dDatiUte)
008100141007     d                                     dtaara
008200141007
008300141007       //--------------------------------------------------------------
008400141007       //?Definizione strutture dati.                                  ?
008500141007       //--------------------------------------------------------------
008600141007
008700141007       // -?Parametri ricevuti?
008800141007     d KPJBA         e ds
008900141007     d FIDN00ds      e ds                  inz
009000141007     d   wNumCA                6     19
009100141007
009200141007       // -?Flag operativi in FNDCT?
009300141007     d dDCT01        e ds                  inz
009400141007
009500141007       // -?Tab. MRA/DAN = Bart-Maling - Danni?
009600141007     d dMRAdan       e ds
009700141007       // -?Tab. TAD = Tipi anomalia danni?
009800141007     d dTAD          e ds                  inz
009900141007
010000141007       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
010100141007     d TRTCM1ds      e ds                  inz
010200141007       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010300141007     d   §CM1mitt    e                     inz('servizi.assicurativi')
010400141007       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
010500141007     d   §CM1dst     e                     inz('servizi.assicurativi@brt.it')
010600141007       //  ?·§CM1tips = Tipo lettera via e-mail:?
010700141007       //   ?"LET" = testo allegato in corpo con logo?
010800141007       //           ?(richiede righe libere iniziali per il logo)?
010900141007       //   ?"COR" = testo integrato senza logo?
011000141007       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
011100141007     d   §CM1tips    e                     inz('E05')
011200141007       //  ?·§CM1po   = Filiale?
011300141007     d   §CM1po      e                     inz('046')
011400141007       //  ?·§CM1var  = Oggetto e-mail?
011500141007     d   §CM1var     e                     inz('*OBJM*+
011600141007     d                                     Mail CONTESTAZIONE UK')
011700141007       //  ?·§CM1sts  = Stato?
011800141007     d   §CM1sts     e                     inz(*off)
011900141007       //  ?·§CM1sts  = Id processo?
012000141007     d   §CM1idp     e                     inz('1')
012100141007
012200141007       // -?Status?
012300141007     d Psds           sds
012400141007     d   SDSpgm          *proc
012500141007
012600141007       //--------------------------------------------------------------
012700141007       //?Definizione variabili globali.                               ?
012800141007       //--------------------------------------------------------------
012900141007
013000141007       // -?Flags booleani?
013100141007     d*// $Invio          s               n   inz
013200141007
013300141007       // -?Indici di schiera?
013400141007     d YY              s              3  0 inz
013500141007
013600141007       // -?Campi di comodo?
013700141007     d Wtad            s                   like(DCTtad)   inz
013800141007
013900141007       //--------------------------------------------------------------
014000141007       //?Definizione procedure usate e relativi parametri.            ?
014100141007       //--------------------------------------------------------------
014200141007
014300141007       // -?Reperimento dati utente?
014400141007     d TIBS34ds      e ds
014500141007      /copy gaitrasrc/srcProtoPR,TIBS34R
014600141007
014700141007       // -?Ricerca/Controllo tabelle?
014800141007     d TIBS02ds      e ds                  inz
014900141007      /copy gaitrasrc/srcProtoPR,TIBS02R
015000141007
015100141007       // -?Controllo & Inversione date?
015200141007     d WLBdat          ds                  inz
015300141007     d   G02dat                1      8  0 inz
015400141007     d   G02inv                9     16  0 inz
015500141007     d   G02err               17     17    inz('3')
015600141007     d   G02tgi               18     22  0 inz
015700141007      /copy gaitrasrc/srcProtoPR,XSRDA8
015800141007
015900141007       // -?API QCAPCMD (Process Commands)?
016000141007     d Qcmd            s           2048    inz  varying
016100141007      /copy qSysInc/qRpgleSrc,QCAPCMD
016200141007      /copy gaitrasrc/srcProtoPR,QCAPCMD
016300141007
016400141007       // -?Parametri gestione errori API.?
016500141007      /copy qsysinc/qrpglesrc,QUSEC
016600141007
016700141007       //--------------------------------------------------------------
016800141007       //?Definizione key-list.                                        ?
016900141007       //--------------------------------------------------------------
017000141007
017100141007       // -?File FNDCT01L?
017200141007     d keyFNDCT01    e ds                  extname(FNDCT01L:*key)
017300141007     d                                     prefix(k_)        inz
017400141007
017500141007       // -?File FNDCS01L?
017600141007     d keyFNDCS01    e ds                  extname(FNDCS01L:*key)
017700141007     d                                     prefix(k_)        inz
017800141007       // -?File FNDET01L?
017900141007     d keyFNDET01    e ds                  extname(FNDET01L:*key)
018000141007     d                                     prefix(k_)        inz
018100141007
018200141007       // -?File FNBLP01L / FNARB01L?
018300141007     d keyFNBLPARB   e ds                  extname(FNBLP01L:*key)
018400141007     d                                     prefix(k_ARB:3)   inz
018500141007
018600141007       //--------------------------------------------------------------
018700141007       //?Riepilogo indicatori.                                        ?
018800141007       //--------------------------------------------------------------
018900141007       //--------------------------------------------------------------
019000141007
019100141007       //--------------------------------------------------------------
019200141007       //?M A I N - L I N E                                            ?
019300141007       //--------------------------------------------------------------
019400141007
019500141007     c     *Entry        plist
019600141007     c                   parm                    KPJBA
019700141007
019800141007      /free
019900141007
020000141007       // -?Operazioni iniziali?
020100141007       exsr  sr_RoutInz;
020200141007
020300141007       // -?Preparazione della e-mail da inviare?
020400141007       if  o00err = *blank;
020500141007         exsr sr_PrintMail;
020600141007       endif;
020700141007
020800141007       // -?Operazioni finali?
020900141007       exsr  sr_RoutEnd;
021000141007
021100141007       //--------------------------------------------------------------
021200141007       //?Operazioni iniziali.                                         ?
021300141007       //--------------------------------------------------------------
021400141007       BEGSR  sr_RoutInz;
021500141007
021600141007         *inLR  = *on;
021700141007
021800141007         // -?Ricezione parametri?
021900141007         FIDN00ds = kpjbu;
022000141007         clear  o00f12;
022100141007         clear  o00f03;
022200141007         clear  o00err;
022300141007         clear  o00msg;
022400141007
022500141007         // -?Reperimento dati job?
022600141007         exsr  sr_DatiJob;
022700141007
022800141007         // -?Reperimento tab. "MRA"?
022900141007         exsr  sr_Rep_TabMRA;
023000141007         if  o00err = 'E';
023100141007           leavesr;
023200141007         endif;
023300141007
023400141007         // -?Verifica dei parametri ricecvuti?
023500141007         if  i00aac = *zero   or
023600141007             i00fil = *zero   or
023700141007             i00nca = *zero;
023800141007           o00err = 'E';
023900141007           o00msg = 'Manca il numero C.A.!!!';
024000141007           leavesr;
024100141007         endif;
024200141007
024300141007         // -?Reperimento dati testata C.A.?
024400141007         exsr  sr_Rep_FNDCT;
024500141007         if  o00err = 'E';
024600141007           leavesr;
024700141007         endif;
024800141007
024900141007         // -?Decodifica filiale gestione?
025000141007         chain  (i00fgs)  AZORG;
025100141007         if  NOT  %found(AZORG01L);
025200141007           o00err = 'E';
025300141007           o00msg = 'Filiale gestione errata';
025400141007           leavesr;
025500141007         endif;
025600141007
025700141007         // -?Reperimento dati spedizione?
025800141007         exsr  sr_Rep_FNBLPARB;
025900141007
026000141007       ENDSR;
026100141007
026200141007       //--------------------------------------------------------------
026300141007       //?Reperimento Dati del job (Utente/Operativi).                 ?
026400141007       //--------------------------------------------------------------
026500141007       BEGSR  sr_DatiJob;
026600141007
026700141007         in(E) §AzUte;
026800141007         if NOT %error;
026900141007           in(E) §DatiUte;
027000141007         endif;
027100141007         if %error or RSut = *blanks;
027200141007           clear TIBS34ds;
027300141007           tibs34r(tibs34ds);
027400141007           in §AzUte;
027500141007           in §DatiUte;
027600141007         endif;
027700141007
027800141007       ENDSR;
027900141007
028000141007       //--------------------------------------------------------------
028100141007       //?Reperimento dati testata C.A.                                ?
028200141007       //--------------------------------------------------------------
028300141007       BEGSR  sr_Rep_FNDCT;
028400141007
028500141007         clear  keyFNDCT01;
028600141007         k_DCTaac = i00aac;
028700141007         k_DCTfil = i00fil;
028800141007         k_DCTnca = i00nca;
028900141007
029000141007         chain  %kds( keyFNDCT01 )  FNDCT000;
029100141007
029200141007         if  NOT  %found(FNDCT01L)   or
029300141007             DCTatb <> *blank;
029400141007           o00err = 'E';
029500141007           o00msg = 'C.A. non reperita';
029600141007           leavesr;
029700141007         endif;
029800141007
029900141007         dDCT01 = DCTflo;
030000141007
030100141007         if  DCTfca   <> c_FCA_Conf  or
030200141007             §DCTtisp <> 'E'         or
030300141007             §DCTeuro <> 'X'         or
030400141007             DCTlna   <> 350         or
030500141007             DCTres   <> 'P';
030600141007           o00err = 'E';
030700141007           o00msg = 'C.A. non contestabile a UK';
030800141007           leavesr;
030900141007         endif;
031000141007
031100141007         // -?Impostazione numero spedizione tra i parms SE non ricevuto?
031200141007         if  i00aas = *zero   or
031300141007             i00lnp = *zero   or
031400141007             i00nsp = *zero;
031500141007           i00aas = DCTaas;
031600141007           i00lnp = DCTlnp;
031700141007           i00nrs = DCTnrs;
031800141007           i00nsp = DCTnsp;
031900141007         endif;
032000141007
032100141007         // -?Impostazione altri dati tra i parms SE non ricevuti?
032200141007         if  i00fca = *zero;
032300141007           i00fca = DCTfca;
032400141007         endif;
032500141007         if  i00fgs = *zero;
032600141007           i00fgs = DCTgfc;
032700141007         endif;
032800141007         if  i00tad = *blank;
032900141007           i00tad = DCTtad;
033000141007         endif;
033100141007
033200141007       ENDSR;
033300141007
033400141007       //--------------------------------------------------------------
033500141007       //?Reperimento dati spedizione.                                 ?
033600141007       //--------------------------------------------------------------
033700141007       BEGSR  sr_Rep_FNBLPARB;
033800141007
033900141021         clear  keyFNBLPARB;
034000141021         k_ARBaas = i00aas;
034100141021         k_ARBlnp = i00lnp;
034200141021         k_ARBnrs = i00nrs;
034300141021         k_ARBnsp = i00nsp;
034400141021
034500141007         If  i00tpb = 'P';
034600141007
034700141007           chain  %kds( keyFNBLPARB )  FNBLP000;
034800141007
034900141007           if  NOT  %found(FNBLP01L)   or
035000141007               ARBatb <> *blank;
035100141007             o00err = 'E';
035200141007             o00msg = 'Spedizione non reperita';
035300141007             leavesr;
035400141007           endif;
035500141007
035600141007         Else;
035700141007
035800141007           chain  %kds( keyFNBLPARB )  FNARB000;
035900141007
036000141007           if  NOT  %found(FNARB01L)   or
036100141007               ARBatb <> *blank;
036200141007             o00err = 'E';
036300141007             o00msg = 'Spedizione non reperita';
036400141007             leavesr;
036500141007           endif;
036600141007
036700141007         EndIf;
036800141007
036900141007       ENDSR;
037000141007
037100141007       //--------------------------------------------------------------
037200141007       //?Stampa della lettera da inviare via e-mail.                  ?
037300141007       //--------------------------------------------------------------
037400141007       BEGSR  sr_PrintMail;
037500141007
037600141007         // -?Override al file di stampa ed apertura dello stesso?
037700141007         exsr  sr_Override;
037800141007
037900141007         // -?Impostazione dei campi in stampa?
038000141007         exsr  sr_RieField;
038100141007
038200141007         // -?Impostazione indicatori utilizzati nel PrtF?
038300141007         *in42  = (PT1da2 <> *blanks);
038400141007         *in43  = (PT1qtd <> *blanks);
038500141007         *in44  = (PT1dp2 <> *blanks);
038600141007         *in47  = (ARBrma <> *blanks);
038700141007
038800141007         // -?Stampa?
038900141007         write  DN41T01;
039000141007
039100141007         // -?Chiusura file stampa & Cancellazione override?
039200141007         exsr  sr_DeleteOvr;
039300141007
039400141007       ENDSR;
039500141007
039600141007       //--------------------------------------------------------------
039700141007       //?Esecuzione override ed apertura del file di stampa.          ?
039800141007       //--------------------------------------------------------------
039900141007       BEGSR  sr_Override;
040000141007
040100141007         //clear  $Invio;
040200141007         reset  TRTCM1ds;
040300141007
040400141007         // -?Esecuzione override?
040500141007         If  §MRADreg <> *blank;
040600141007
040700141007           §CM1mitt = %trim(§MRADmitt);
040800141007           §CM1dst  = %trim(§MRADdest);
040900141007           §CM1tips = §MRADreg;
041000141007           §CM1po   = %editc( DUTpou : 'X' );
041100141007           §CM1var  = '*OBJM*'
041200141007                    + 'Damage comunication n. '
041300141007                    + %editw(I00fil : '0   ')
041400141007                    + '/'
041500141007                    + %trim( %editc( I00nca : 'Z' ))
041600141007                    + ' '
041700141007                    + %subst( %char( I00aac ) : 3 : 2 );
041800141007           §CM1idp  = §MRADidpro;
041900141007
042000141007           Qcmd = c_CmdOvrPrtF
042100141007                + ' outq(' + %trim(§MRADoutqi) + ')'
042200141007                + ' usrdfndta(''' + TRTCM1ds + ''')';
042300141007           exsr  sr_ExecCmd;
042400141007           //$Invio = (Qusei = *blank);
042500141007
042600141007         EndIf;
042700141007
042800141007         // -?Apertura file di stampa?
042900141007         open  FIDN41P;
043000141007
043100141007       ENDSR;
043200141007
043300141007       //--------------------------------------------------------------
043400141007       //?Impostazione dei campi in stampa.                            ?
043500141007       //--------------------------------------------------------------
043600141007       BEGSR  sr_RieField;
043700141007
043800141007         clear  DN41T01;
043900141007
044000141007         // -?Intestazione Destinatario?
044100141021         PT1rgs = 'Fed Ex UK';
044200141007
044300141007         // -?Località + Data?
044400141007         if  ORGfl1 = 'E'  and  ORGfva = *blank;
044500141007           PT1dal = 'MILANO, ' + %editc(*date : 'Y');
044600141007         else;
044700141007           PT1dal = %trim(ORGdes) + ', ' + %editc(*date : 'Y');
044800141007         endif;
044900141007
045000141007         // -?Oggetto?
045100141007         PT1nca = %trim( %editw( I00fil:'0   ' )) + '/'
045200141007                + %trim( %editc( I00nca:'Z' ));
045300141007         PT1aac = I00aac;
045400141007
045500141007         // -?Dati relativi alla Spedizione?
045600141007         PT1lnp = I00lnp;
045700141007         PT1nrs = I00nrs;
045800141007         PT1nsp = I00nsp;
045900141007         reset WLBdat;
046000141007         G02inv = (I00aas * 10000) + ARBmgs;
046100141007         xsrda8 ( WLBdat );
046200141007         PT1dat = G02dat;
046300141007         PT1ncl = ARBncl;
046400141007         PT1pkf = ARBpkf;
046500141007
046600141007         // -?Dati relativi al Mittente?
046700141007         PT1rsm = ARBrsm;
046800141007         PT1rmn = ARBrmn;
046900141007         PT1rma = ARBrma;
047000141007
047100141007         // -?Dati relativi al Destinatario?
047200141007         PT1rsd = ARBrsd;
047300141007         PT1lod = ARBlod;
047400141007         if  ARBnzd <> *blank;
047500141007           PT1prn = ARBnzd;
047600141007         else;
047700141007           PT1prn = ARBprd;
047800141007         endif;
047900141007
048000141007         // -?Anomalia?
048100141007         Wtad = I00tad;
048200141007         exsr  sr_Rep_TabTAD;
048300141007         select;
048400141007           when  §TADdesi <> *blank;
048500141007             PT1tad = §TADdesi;
048600141007           when  §TADdest <> *blank;
048700141007             PT1tad = §TADdest;
048800141007           other;
048900141007             PT1tad = §TADdesc;
049000141007         endsl;
049100141007         // -?Descrizione anomalia?
049200141007         exsr  sr_Rep_DesTAD;
049300141007
049400141007         // -?Evento?
049500141007         IF  DCTnev > *zero;
049600141007           PT1kev = %trim( %editw( DCTaae : '0    ' )) + '/'
049700141007                  + %trim( %editc( DCTnev : 'Z' ));
049800141007           chain (DCTaae : DCTnev) FNDET000;
049900141007           If  %found(FNDET01L);
050000141007             Wtad = DETtad;
050100141007             exsr  sr_Rep_TabTAD;
050200141007             if  §TADdest <> *blank;
050300141007               PT1tae = §TADdest;
050400141007             else;
050500141007               PT1tae = §TADdesc;
050600141007             endif;
050700141007           EndIf;
050800141007         ENDIF;
050900141007
051000141007         // -?Quantità danneggiate?
051100141007         //  ?·Peso?
051200141007         if  DCTpkd > *zero;
051300141007           PT1qtd = 'Kg ' + %trim( %editc( DCTpkd : 'L' ));
051400141007         endif;
051500141007         //  ?·Colli?
051600141007         if  DCTncn > *zero;
051700141007           PT1qtd = %trim(PT1qtd)
051800141007                  + ' Parcels ' + %trim( %editc( DCTncn : 'Z' ));
051900141007         endif;
052000141007
052100141007         // -?Pezzi?
052200141007         //  ?+ Descrizione pezzi danneggiati?
052300141007         if  DCTnpz > *zero;
052400141007           PT1qtd  = %trim(PT1qtd)
052500141007                   + ' Pieces ' + %trim( %editc( DCTnpz : 'Z' ));
052600141007           exsr  sr_Rep_DesPZD;
052700141007         endif;
052800141007
052900141007       ENDSR;
053000141007
053100141007       //--------------------------------------------------------------
053200141007       //?Chiusura file stampa & Cancellazione override.               ?
053300141007       //--------------------------------------------------------------
053400141007       BEGSR  sr_DeleteOvr;
053500141007
053600141007         if  %open(FIDN41P);
053700141007
053800141007           close  FIDN41P;
053900141007
054000141007           Qcmd = c_CmdDltOvr;
054100141007           exsr  sr_ExecCmd;
054200141007
054300141007         endif;
054400141007
054500141007       ENDSR;
054600141007
054700141007       //--------------------------------------------------------------
054800141007       //?Reperimento tab. MRA = Bart-Maling - Danni.                  ?
054900141007       //--------------------------------------------------------------
055000141007       BEGSR  sr_Rep_TabMRA;
055100141007
055200141007         clear  dMRAdan;
055300141007
055400141007         clear  TIBS02ds;
055500141007         T02mod = 'C';
055600141007         //T02sif = knsif;
055700141007         T02cod = 'MRA';
055800141007         T02ke1 = %trimr(SDSpgm);
055900141007         T02ke2 = 'DAN';
056000141007
056100141007         TNTBE_RicercaControllo ( kpjba : tibs02ds );
056200141007
056300141007         if T02err = *blanks;
056400141007           dMRAdan = T02uni;
056500141007         else;
056600141007           o00err = 'E';
056700141007           o00msg = 'Manca tab. MRA'
056800141007                  + ' "' + %trimr(T02ke1) + '" /'
056900141007                  + ' "' + %trimr(T02ke2) + '"';
057000141007           leavesr;
057100141007         endif;
057200141007
057300141007       ENDSR;
057400141007
057500141007       //--------------------------------------------------------------
057600141007       //?Reperimento tab. TAD = Tipo Anomalia.                        ?
057700141007       //--------------------------------------------------------------
057800141007       BEGSR  sr_Rep_TabTAD;
057900141007
058000141007         clear  dTAD;
058100141007
058200141007         clear  TIBS02ds;
058300141007         T02mod = 'C';
058400141007         T02sif = knsif;
058500141007         T02cod = 'TAD';
058600141007         T02ke1 = Wtad;
058700141007
058800141007         TNTBE_RicercaControllo ( kpjba : tibs02ds );
058900141007
059000141007         if T02err = *blanks;
059100141007           dTAD = T02uni;
059200141007         endif;
059300141007
059400141007       ENDSR;
059500141007
059600141007       //--------------------------------------------------------------
059700141007       //?Reperimento descrizione anomalia.                            ?
059800141007       //--------------------------------------------------------------
059900141007       BEGSR  sr_Rep_DesTAD;
060000141007
060100141007         clear  YY;
060200141007         clear  sk_TAD;
060300141007
060400141007         clear  keyFNDCS01;
060500141007         k_DCStpd = 'C';
060600141007         k_DCSnkt = wNumCA;
060700141007         k_DCSstd = 'A';
060800141007
060900141007         setll  %kds( keyFNDCS01 : 3 )  FNDCS000;
061000141007         reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
061100141007
061200141007         DoW  NOT  %eof(FNDCS01L)   and
061300141007              YY < %elem(sk_TAD);
061400141007           yy += 1;
061500141007           sk_TAD(yy) = DCSnot;
061600141007           reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
061700141007         EndDo;
061800141007
061900141007         PT1da1 = sk_TAD(1);
062000141007         PT1da2 = sk_TAD(2) + sk_TAD(3);
062100141007
062200141007       ENDSR;
062300141007
062400141007       //--------------------------------------------------------------
062500141007       //?Reperimento descrizione pezzi danneggiati/mancanti.          ?
062600141007       //--------------------------------------------------------------
062700141007       BEGSR  sr_Rep_DesPZD;
062800141007
062900141007         clear  YY;
063000141007         clear  sk_TAD;
063100141007
063200141007         clear  keyFNDCS01;
063300141007         k_DCStpd = 'C';
063400141007         k_DCSnkt = wNumCA;
063500141007         k_DCSstd = 'P';
063600141007
063700141007         setll  %kds( keyFNDCS01 : 3 )  FNDCS000;
063800141007         reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
063900141007
064000141007         DoW  NOT  %eof(FNDCS01L)   and
064100141007              YY < %elem(sk_TAD);
064200141007           yy += 1;
064300141007           sk_TAD(yy) = DCSnot;
064400141007           reade  %kds( keyFNDCS01 : 3 )  FNDCS000;
064500141007         EndDo;
064600141007
064700141007         PT1dpz = sk_TAD(1);
064800141007         PT1dp2 = sk_TAD(2) + sk_TAD(3);
064900141007
065000141007       ENDSR;
065100141007
065200141007       //--------------------------------------------------------------
065300141007       //?Esecuzione del comando (già impostato).                      ?
065400141007       //--------------------------------------------------------------
065500141007       BEGSR  sr_ExecCmd;
065600141007
065700141007         clear Qcap0100;
065800141007         Qcabcsdh = *off;
065900141007         Qcapa    = *off;
066000141007         Qcacmdss = *off;
066100141007         Qcaerved = *allX'00';
066200141007
066300141007         clear Qusec;
066400141007         Qusbprv  = %size(Qusec);
066500141007
066600141007         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
066700141007                           %size(Qcap0100) : 'CPOP0100' : *omit :
066800141007                           0 : 0 : Qusec);
066900141007
067000141007         //if  Qusei <> *blank;
067100141007         //  ...;
067200141007         //endif;
067300141007
067400141007       ENDSR;
067500141007
067600141007       //--------------------------------------------------------------
067700141007       //?Operazioni finali.                                           ?
067800141007       //--------------------------------------------------------------
067900141007       BEGSR  sr_RoutEnd;
068000141007
068100141007         // -?Restituzione parametri?
068200141007         kpjbu = FIDN00ds;
068300141007
068400141007         // -?Uscita dal *pgm?
068500141007         return;
068600141007
068700141007       ENDSR;
068800141007
068900141007      /end-free
