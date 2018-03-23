000100121024       //==============================================================
000200121024       //?TNTB79R3 * Annullamento fisico record tab. "3EW" con flag    ?
000300121024       //?           "Annullamento Applicativo" valorizzato.           ?
000400121024       //==============================================================
000500121024
000600121024       //--------------------------------------------------------------
000700121024       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800121024       //--------------------------------------------------------------
000900121024
001000121024     /*PRM dbgview(*source)
001100121024     /*PRM commit(*NONE)
001200121024     /*END
001300121024
001400121024       //--------------------------------------------------------------
001500121024       //?Specifiche di controllo.                                     ?
001600121024       //--------------------------------------------------------------
001700121024
001800121025     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900130305     h dftactgrp(*no)
002000130305     h bnddir('UBBNDDIR')
002100121024     h alwnull(*inputonly)
002200121024
002300121024       //--------------------------------------------------------------
002400121024       //?Dichiarazione file.                                          ?
002500121024       //--------------------------------------------------------------
002600121024
002700121025       // -?Tabelle?
002800121113     fTNTBE01L  Uf   e           k disk
002900130220     fTABEL00F  Uf   e           k disk
003000121025
003100121024       // -?Spool da inviare via e-mail:?
003200121024       //   ·?Corpo del messaggio di posta elettronica?
003300121024     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
003400140107       //   ·?Avviso di figli NON EasyWEB rimasti in tab. "3C"?
003500140107     fPrtEMAIL2 o    f  132        printer  oflind(*inOE)  usropn
003600121024
003700121024       //--------------------------------------------------------------
003800121024       //?Definizione costanti.                                        ?
003900121024       //--------------------------------------------------------------
004000121024
004100121024       // -?Codice tabella in gestione?
004200121024     d c_Tab           c                   const('3EW')
004300131126
004400131126       // -?Abilitazione ad EasySped WEB?
004500131126     d c_EasyWEB       c                   const('ESWEB')
004600121024
004700121024       // -?Dati di default?
004800121024     d c_Mittente      c                   const('ced')
004900121024     d c_at            c                   const('@brt.it')
005000121024     d c_Sede          c                   const('046')
005100121024
005200121024       // -?Comandi di override al PrtF "corpo e-mail"?
005300121024     d c_CmdOvrPrtF    c                   const('OVRPRTF +
005400121024     d                                           file(PRTEMAIL) +
005500121024     d                                           pagesize(66 132) +
005600121024     d                                           lpi(6) cpi(10) +
005700121024     d                                           ovrscope(*actgrpdfn) +
005800121024     d                                           ')
005900140107     d c_CmdOvrPrtF2   c                   const('OVRPRTF +
006000140108     d                                           file(PRTEMAIL2) +
006100140108     d                                           tofile(PRTEMAIL) +
006200140107     d                                           pagesize(66 132) +
006300140107     d                                           lpi(6) cpi(10) +
006400140107     d                                           ovrscope(*actgrpdfn) +
006500140107     d                                           ')
006600121024     d c_CmdDltOvr     c                   const('DLTOVR +
006700140107     d                                            file(*all) +
006800121024     d                                            lvl(*actgrpdfn)')
006900121113
007000121113       // -?Costante per controllo "caratteri solo numerici"?
007100121113     d c_Digits        c                   const('0123456789')
007200121024
007300121024       //--------------------------------------------------------------
007400121024       //?Definizione schiere.                                         ?
007500121024       //--------------------------------------------------------------
007600121024
007700140124       // -?Esiti errati nel reperimento dei legami in tab. "3C"?
007800131120     d sk_ErrEsitoLeg3C...
007900131209     d                 s             40    dim( 5)  ctdata  perrcd( 1)
008000121024
008100121024       //--------------------------------------------------------------
008200121024       //?Definizione aree dati.                                       ?
008300121024       //--------------------------------------------------------------
008400121024
008500121024       // -?Dati utente?
008600121024     d §AzUte        e ds                  extname(AZUTE00F)
008700121024     d                                     dtaara
008800121024     d §DatiUte      e ds                  extname(dDatiUte)
008900121024     d                                     dtaara
009000121024
009100121024       //--------------------------------------------------------------
009200121024       //?Definizione strutture dati.                                  ?
009300121024       //--------------------------------------------------------------
009400121024
009500121024       // -?Status ds?
009600121024     d Status         sds
009700121024     d   SDSpgm          *proc
009800121024
009900121024       // -?Dati estratti via SQL per la stampa?
010000121025     d wSQL_dati       ds                  inz
010100130220     d   SQL_ke1                           like(TBEke1)      inz                 15=>  1- 15
010200130220     d   SQL_ke2                           like(TBEke2)      inz                 15=> 16- 30
010300130220     d   SQL_uni                           like(TBEuni)      inz                256=> 31-286
010400121024
010500121024       // -?Parametri ricevuti?
010600121113     d KPJBA         e ds
010700121024
010800121024       // -?Tab. 3EW = Dati assegnati alla "postazione" in abilitazione?
010900121024       //             ?ad EasyWeb?
011000121024     d d3EW          e ds                  inz
011100130220       // -?Tab. 3C  = Clienti che inviano spedizioni partenza?
011200130220     d ds3C          e ds                  inz
011300121024
011400121024       // -?Tab. "MRA" = Bart-Mailing?
011500121025     d dMRAdan       e ds                  inz
011600121025     d   §MRAddes    e                     inz('Rec. annullati in tab.+
011700121025     d                                          3EW')
011800121025     d   §MRAdreg    e                     inz('COR')
011900121025     d   §MRAdmitt   e                     inz(c_Mittente)
012000121024     d   §MRAddest   e                     inz('stefano.merighi+
012100121025     d                                          @brt.it;')
012200121024     d   §MRAdidpro  e                     inz('1')
012300121025     d   §MRAdoutqi  e                     inz('QPRINTS')
012400121024
012500121024       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
012600121024     d TRTCM1ds      e ds                  inz
012700121024       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
012800121025     d   §CM1mitt    e                     inz(c_Mittente)
012900121024       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
013000121024     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
013100121024       //  ?·§CM1tips = Tipo lettera via e-mail:?
013200121024       //   ?"LET" = testo allegato in corpo con logo?
013300121024       //           ?(richiede righe libere iniziali per il logo)?
013400121024       //   ?"COR" = testo integrato senza logo?
013500121024       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
013600121024       //   ?"SP1" = spool allegato (in formato PDF)?
013700121024     d   §CM1tips    e                     inz('COR')
013800121024       //  ?·§CM1po   = Sede?
013900121024     d   §CM1po      e                     inz(c_Sede)
014000121024       //  ?·§CM1var  = Oggetto e-mail?
014100121024     d   §CM1var     e                     inz('*OBJM*+
014200121025     d                                     Rec. annullati in tab. +
014300121025     d                                     "3EW"')
014400121024       //  ?·§CM1sts  = Stato?
014500121024     d   §CM1sts     e                     inz(*off)
014600121024       //  ?·§CM1idp  = Id processo?
014700121024     d   §CM1idp     e                     inz('2')
014800121024
014900121024       //--------------------------------------------------------------
015000121024       //?Definizione variabili globali.                               ?
015100121024       //--------------------------------------------------------------
015200121113
015300121113       // -?Parametri?
015400121113     d prm_NrDays      s              3  0 inz(060)
015500121024
015600121024       // -?Flags booleani?
015700121025     d $Err            s               n   inz
015800121025     d $EoF            s               n   inz
015900131120
016000131120       // -?Indici di schiera?
016100131120     d xx              s              3  0 inz
016200121024
016300121025       // -?Stringa SQL da eseguire:?
016400121025       //  ?· per lettura e stampa - prima,?
016500121025       //  ?· per annullamento logico - dopo.?
016600121025     d wSql            s           2048    inz  varying
016700121024
016800121024       // -?Variabili di comodo?
016900121025     d wDate_Eur       s               d   inz  datfmt(*eur)
017000121024     d wDate_Iso       s               d   inz  datfmt(*iso)
017100121024     d wDateA          s              8a   inz(*zero)
017200131120     d wEsitoLeg3C     s             10i 0 inz
017300121025
017400121025       // -?Variabili di stampa?
017500121025     d ds_Time14       ds            14    inz
017600121025     d   wDate                        8  0 inz
017700131120     d   s_Time                       6  0 inz
017800131120     d s_Date          s              8  0 inz
017900131120     d s_DataLim       s              8  0 inz
018000131120     d s_TotRecDel     s              9  0 inz
018100140108     d s_TotErr3C      s              9  0 inz
018200131120     d s_NrFigliNoEW   s              3  0 inz
018300131209     d s_EsitoLeg3C    s             40    inz
018400121024
018500121024       //--------------------------------------------------------------
018600121024       //?Definizione prototipi procedure.                             ?
018700121024       //--------------------------------------------------------------
018800121024
018900121024       // -?Reperimento dati utente?
019000121024     d TIBS34ds      e ds                  inz
019100121024      /copy gaitrasrc/srcProtoPR,TIBS34R
019200140107
019300140107       // -?Verifica abilitazione STRATEGI?
019400140107     d TIVSSds       e ds                  extname(TIVSS00F) qualified
019500140107      /copy gaitrasrc/srcProtoPI,TIS7701R
019600140107      /copy gaitrasrc/srcProtoPR,TIS7701R
019700130305
019800130305       // -?Reperimento legami in tab. "3C"?
019900130305      /copy gaitrasrc/srcProtoPI,UBLEG3C
020000130305      /copy gaitrasrc/srcProtoPR,UBLEG3C
020100121024
020200121024       // -?Parametri API QCAPCMD (Process Commands)?
020300121024     d Qcmd            s           2048    inz  varying
020400121024      /copy qSysInc/qRpgleSrc,QCAPCMD
020500121024       // -?API QCAPCMD (Process Commands)?
020600121024      /copy gaitrasrc/srcProtoPR,QCAPCMD
020700121024
020800121024       // -?Parametri gestione errori API.?
020900121024      /copy qsysinc/qrpglesrc,QUSEC
021000121024
021100121024       //--------------------------------------------------------------
021200121024       //?Definizione key-list.                                        ?
021300121024       //--------------------------------------------------------------
021400121024
021500121025       // -?File TNTBE01L?
021600121025     d k05tntbe01    e ds                  extname(TNTBE01L:*key) inz
021700121025     d                                     prefix(K_)
021800130220
021900130220       // -?File TABEL00F?
022000130220     d k03tabel00    e ds                  extname(TABEL00F:*key) inz
022100130220     d                                     prefix(K_)
022200121024
022300121024       //--------------------------------------------------------------
022400121024       //?Riepilogo indicatori utilizzati.                             ?
022500121024       //--------------------------------------------------------------
022600121024
022700121024
022800121024       //--------------------------------------------------------------
022900121024       //?M A I N - L I N E                                            ?
023000121024       //--------------------------------------------------------------
023100121024
023200121113     c     *Entry        plist
023300121113     c                   parm                    KPJBA
023400121024
023500121024      /free
023600121024
023700121024       // -?Operazioni iniziali?
023800121024       exsr  sr_RoutInz;
023900121024
024000121119       // -?Stampa (ed invio) e-mail  &?
024100121119       //  ?Annullamento logico tab."3EW"?
024200121024       exsr  sr_PrtEMAIL;
024300121024
024400121024       // -?Operazioni finali?
024500121024       exsr  sr_RoutEnd;
024600121024
024700121024       //--------------------------------------------------------------
024800121024       //?Operazioni iniziali.                                         ?
024900121024       //--------------------------------------------------------------
025000121024       BEGSR  sr_RoutInz;
025100121024
025200121024         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
025300121024
025400121024         *inLR = *on;
025500121113
025600121113         // -?Ricezione parametri?
025700121113         if  %parms() > *zero  and
025800121113             %check( c_Digits : %subst( kpjbu : 1 : 3 ) ) = *zero;
025900121113           prm_NrDays = %int( %subst( kpjbu : 1 : 3 ) );
026000121113         endif;
026100121024
026200121024         // -?Reperimento dati job?
026300121024         exsr  sr_DatiJob;
026400121024
026500121025         // -?Reperimento data & orario corrente?
026600121025         ds_Time14 = %subst( %char( %dec( %timestamp() ) )
026700121025                             : 1 : 14 );
026800121113         wDate_Iso = %date( wDate : *iso ) - %days(prm_NrDays);
026900121025         wDateA    = %editc( %dec(wDate_Iso) : 'X' );
027000121113         wDate_Eur = wDate_Iso;
027100131120         s_DataLim = %dec( wDate_Eur );
027200121025         wDate_Eur = %date();
027300131120         s_Date    = %dec( wDate_Eur );
027400121024
027500121024         // -?Reperimento tab. "MRA"?
027600121025         reset  dMRAdan;
027700130423         clear  k05tntbe01;
027800121024         k_TBEcod = 'MRA';
027900121024         k_TBEke1 = SDSpgm;
028000121024         k_TBEke2 = *blank;
028100121113         chain(N)  %kds( k05tntbe01 : 3 )  TNTBE000;
028200121024         if  %found(TNTBE01L)   and   TBEatb = *blank;
028300121024           dMRAdan = TBEuni;
028400121024         endif;
028500121024
028600121024         // -?Override ai file di stampa?
028700121024         //  ?(per impostarvi i dati per l'invio via e-mail)?
028800131209         exsr sr_OvrPrtF;
028900121024
029000121024         // -?Apertura file di stampa?
029100121119         //  ?(NO: verrà aperto SOLO SE ci sono rec. da annullare)?
029200121119         //open  PrtEmail;
029300121119         //except  PrtTxt;
029400121119         //*inOF = *off;
029500140108         //open  PrtEmail2;
029600140108         //except  ErrTxt;
029700140108         //*inOE = *off;
029800121024
029900121024       ENDSR;
030000121024
030100121024       //--------------------------------------------------------------
030200121024       //?Reperimento Dati del job (Utente/Operativi).                 ?
030300121024       //--------------------------------------------------------------
030400121024       BEGSR  sr_DatiJob;
030500121024
030600121024         in(e) §AzUte;
030700121024         if NOT %error;
030800121024           in(e) §DatiUte;
030900121024         endif;
031000121024         if %error or RSut = *blank;
031100121024           tibs34r ( tibs34ds );
031200121024           in §AzUte;
031300121024           in §DatiUte;
031400121024         endif;
031500121024
031600121024       ENDSR;
031700121025
031800121025       //--------------------------------------------------------------
031900121025       //?Override al file di stampa per impostarvi i dati per         ?
032000121025       //?  l'invio via e-mail.                                        ?
032100121025       //--------------------------------------------------------------
032200121025       BEGSR  sr_OvrPrtF;
032300121025
032400121025         reset  TRTCM1ds;
032500121025         if  §MRAdmitt <> *blank;
032600121025           §CM1mitt = %trim(§MRAdmitt);
032700121025         endif;
032800121025         §CM1dst  = %trim(§MRAddest);
032900121025         §CM1tips = §MRAdreg;
033000121025         §CM1idp  = §MRAdidpro;
033100121025
033200140108         // -?Override al "corpo e-mail"?
033300140108         //§CM1var  = '*OBJM*' + §MRAddes;
033400140124         if  §MRAdreg <> *blank;
033500121025           Qcmd = c_CmdOvrPrtF
033600121025                + ' outq(' + %trim(§MRAdoutqi) + ')'
033700121025                + ' usrdfndta(''' + TRTCM1ds + ''')';
033800121025         else;
033900121025           Qcmd = c_CmdOvrPrtF;
034000121025         endif;
034100121025
034200121025         exsr  sr_ExecCmd;
034300121025
034400121025         //if  Qusei <> *blank;
034500121025         //  ...;
034600121025         //endif;
034700140107
034800140107         // -?Override al "corpo e-mail 2"?
034900140107         §CM1var  = '*OBJM*' + 'ERRORI in annull. tab. "3EW"';
035000140124         if  §MRAdreg <> *blank;
035100140107           Qcmd = c_CmdOvrPrtF2
035200140107                + ' outq(' + %trim(§MRAdoutqi) + ')'
035300140107                + ' usrdfndta(''' + TRTCM1ds + ''')';
035400140107         else;
035500140107           Qcmd = c_CmdOvrPrtF2;
035600140107         endif;
035700140107
035800140107         exsr  sr_ExecCmd;
035900140107
036000140107         //if  Qusei <> *blank;
036100140107         //  ...;
036200140107         //endif;
036300121025
036400121025       ENDSR;
036500121024
036600121024       //--------------------------------------------------------------
036700121024       //?Stampa elenco dei clienti in tab. "3EW" in corso di annullam.?
036800121024       //--------------------------------------------------------------
036900121024       BEGSR  sr_PrtEMAIL;
037000121025
037100121025         clear  $Err;
037200121025         clear  $EoF;
037300121024
037400121025         wSql = 'select TBEke1, TBEke2, TBEuni +
037500121024                   from TNTBE00F +
037600121025                  where TBEcod = ''' + c_Tab + ''' +
037700121024                    and TBEatb = '' '' +
037800121024                    and substr( TBEuni, 33, 1 ) = ''A'' +
037900121025                    and substr( TBEuni, 34, 8 ) < ''' + wDateA + ''' +
038000130117                  order by TBEke1, TBEke2 +
038100121025                    for fetch only';
038200121024
038300121024         // -?Dichiarazione cursore?
038400121024         exec sql   prepare S1   from :wSql;
038500121024         exec sql   declare C1   cursor for S1;
038600121024
038700121024         if  SQLcode < *zero;
038800121025           $Err = *on;
038900121024           exsr  sr_PrintErr;
039000121025           leavesr;
039100121024         endif;
039200121024
039300121024         // -?Apertura del cursore?
039400121024         exec sql   open C1;
039500121024
039600121024         if  SQLcode < *zero;
039700121025           $Err = *on;
039800121024           exsr  sr_PrintErr;
039900121025           leavesr;
040000121024         endif;
040100121024
040200121024         // -?Ciclo di lettura rec. da annullare dalla tab. "3EW"?
040300121025         DoW  Not $EoF;
040400121024
040500121025           exec sql   fetch next   from C1   into :wSQL_dati;
040600121024
040700121024           select;
040800121024
040900121024             when  SQLcode = 100;
041000121025               $EoF  = *on;
041100121119               if  %open(PrtEMAIL);
041200121119                 except  PrtEnd;
041300121119               endif;
041400140108               if  %open(PrtEMAIL2);
041500140108                 except  ErrEnd;
041600140108               endif;
041700121024
041800121024             when  SQLcode < *zero;
041900121025               $Err  = *on;
042000121025               $EoF  = *on;
042100121024               exsr  sr_PrintErr;
042200121024
042300121024             other;
042400121113               exsr  sr_Elaboraz3EW;
042500121024
042600121024           endsl;
042700121024
042800121024         EndDo;
042900121024
043000121024         // -?Chiusura del cursore?
043100121024         exec SQL   close C1;
043200121024
043300121024       ENDSR;
043400121113
043500121113       //--------------------------------------------------------------
043600121113       //?Stampa & Annullamento logico del singolo record in tab."3EW".?
043700121113       //--------------------------------------------------------------
043800121113       BEGSR  sr_Elaboraz3EW;
043900121113
044000121113         d3EW = SQL_uni;
044100140107
044200140107         // -?Reperimento dati di TIVSS tramite TIS7701R (con S.U.N.)?
044300140107         rqsKSU = SQL_ke1;
044400140107         rqsSUN = SQL_ke2;
044500140107         clear  rpyEsito;
044600140107         Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
044700140108                                  rqsSUN : %dec(wDate_Iso) : TIVSSds :
044800140107                                  rpyEsito );
044900140107
045000140107         // -?SE cliente NON bloccato su Strategi => è da lasciare?
045100140108         if  rpyEsito = *zero  and  TIVSSds.VSSksu > *zero;
045200140124           if  Not %open(PrtEMAIL);
045300140124             open  PrtEmail;
045400140124             *inOF = *on;
045500140124           endif;
045600140124           if  *inOF;
045700140124             except  PrtTxt;
045800140124             *inOF = *off;
045900140124           endif;
046000140505           except  PrtDet;
046100140124           except  PrtErr0;
046200140107           leavesr;
046300140107         endif;
046400121113
046500121113         // -?Stampa?
046600131120         s_TotRecDel += 1;
046700121119         if  Not %open(PrtEMAIL);
046800121119           open  PrtEmail;
046900121119           *inOF = *on;
047000121119         endif;
047100121119         if  *inOF;
047200121119           except  PrtTxt;
047300121119           *inOF = *off;
047400121119         endif;
047500121113         except  PrtDet;
047600121113
047700130220         // -?"Annullamento" record TNTBE00F?
047800130423         clear  k05tntbe01;
047900121113         k_TBEcod = c_Tab;
048000121113         k_TBEke1 = SQL_ke1;
048100121113         k_TBEke2 = SQL_ke2;
048200121113         chain(e)  %kds( k05tntbe01 : 3 )  TNTBE000;
048300121113         if  NOT %error         and
048400121113             %found(TNTBE01L)   and   TBEatb = *blank  and
048500121113             §3EWfaa = 'A';
048600121113           TBEatb = 'A';
048700121113           TBEftt = 'S';
048800121113           clear  TBEftr;
048900140124           //_______________
049000140124           update  TNTBE000;
049100140124           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
049200130220           exsr  sr_Elaboraz3C;
049300121113         endif;
049400121113
049500121113       ENDSR;
049600130220
049700130220       //--------------------------------------------------------------
049800130220       //?Annullamento logico del singolo record in tab."3C".          ?
049900130220       //--------------------------------------------------------------
050000130220       BEGSR  sr_Elaboraz3C;
050100130220
050200130220         // -?Verifica esistenza altri "Strategi User Number" attivi?
050300130220         //  ?(non annullati) per lo stesso unificante in tab. "3EW"?
050400130423         clear  k05tntbe01;
050500130220         k_TBEcod = c_Tab;
050600130220         k_TBEke1 = SQL_ke1;
050700130220         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
050800130220         reade(n)  %kds( k05tntbe01 : 2 )  TNTBE000;
050900130220         DoW  Not  %eof(TNTBE01L);
051000130220           // ·?SE trovato: NON si annulla la tab. "3C"?
051100140124           if  TBEatb = *blank;
051200130220             leavesr;
051300130220           endif;
051400130220           reade(n)  %kds( k05tntbe01 : 2 )  TNTBE000;
051500130220         EndDo;
051600140107
051700140107         // -?Reperimento dati di TIVSS tramite TIS7701R (senza S.U.N.)?
051800140108         rqsKSU = SQL_ke1;
051900140108         clear  rqsSUN;
052000140108         clear  rpyEsito;
052100140108         Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
052200140108                                  *blank : %dec(wDate_Iso) : TIVSSds :
052300140108                                  rpyEsito );
052400140107
052500140107         // -?SE cliente NON bloccato su Strategi => è da lasciare?
052600140108         if  rpyEsito = *zero  and  TIVSSds.VSSksu > *zero;
052700140107           leavesr;
052800140107         endif;
052900130220
053000130305         // -?"Annullamento" record TABEL00F?
053100140124         //  ?se rimasto unificante "ESWEB";?
053200140124         //  ?se con figli NON "ESWEB" viene segnalato...?
053300130423         clear  k03tabel00;
053400130220         k_TBLkut = 1;
053500130220         k_TBLcod = '3C';
053600130220         k_TBLkey = %subst( SQL_ke1 : 2 );
053700130220         chain(e)  %kds( k03tabel00 )  TABEL;
053800131120
053900131120         IF  NOT %error         and
054000130220             %found(TABEL00F)   and   TBLflg = *blank;
054100131120
054200130220           ds3C = TBLuni;
054300131120
054400131120           // -?SE cliente unificante ancora EasySpedWEB...?
054500131126           If  §3Ccba = c_EasyWEB  and  §3Ccks = %int( %trim( k_TBLkey ) );
054600131120
054700131120             clear  wEsitoLeg3C;
054800131120             clear  s_EsitoLeg3C;
054900131120             clear  s_NrFigliNoEW;
055000131120
055100131120             // -?Reperimento legami in tab. "3C"?
055200131120             wEsitoLeg3C = ubLeg3C_Rtv ( %int( %trim( k_TBLkey ) ) :
055300131120                                         ubLeg3C_FlgPF :
055400131120                                         ubLeg3C_Padre :
055500131120                                         ubLeg3C_SKC :
055600131120                                         ubLeg3C_SKEW );
055700131120
055800131120             // -?Rilevato ERRORE nel reperimento legami in tab. "3C":?
055900131120             //  ?segnalazione ed uscita?
056000131120             if  wEsitoLeg3C < *zero;
056100131120               if  wEsitoLeg3C >= -4;
056200131120                 s_EsitoLeg3C = sk_ErrEsitoLeg3C( %abs( wEsitoLeg3C ) );
056300131120               else;
056400131120                 s_EsitoLeg3C = sk_ErrEsitoLeg3C( 5 );
056500131120               endif;
056600131119               except  PrtErr1;
056700131119               leavesr;
056800131119             endif;
056900131120
057000131120             // -?Verifica se l'unificante ha figli NON EasySpedWEB;?
057100140107             //  ?SE ne ha: conteggio e segnalazione via e-mail?
057200140108             //  ?(ci dovrà pensare qualcuno del CED a cambiare lo?
057300140108             //  ? unificante in tali figli)?
057400131120             For  xx = 2  To  %elem(sch_SKC);
057500131120
057600131120               if  sch_SKC(xx) = *zero;
057700131120                 leave;
057800131120               endif;
057900131120
058000131120               if  sch_SKEW(xx) <> *on;
058100131120                 s_NrFigliNoEW += 1;
058200131120               endif;
058300131120
058400131120             EndFor;
058500131120
058600131120             if  s_NrFigliNoEW > *zero;
058700131120               except  PrtErr2;
058800140107               //leavesr;
058900140108               s_TotErr3C += s_NrFigliNoEW;
059000140107               exsr  sr_PrtDettErr2;
059100131120             endif;
059200131120
059300131120             // -?Annullamento dell'unificante (EasySpedWEB)?
059400130220             TBLflg = '*';
059500130220             TBLftt = '1';
059600130220             clear  TBLftr;
059700130220             clear  TBLdtr;
059800140124             //____________
059900140124             update  TABEL;
060000140124             //¯¯¯¯¯¯¯¯¯¯¯¯
060100131120
060200131120             // -?"Annullamento" degli eventuali figli (tutti "ESWEB")?
060300131120             if  sch_SKC(02) > *zero;
060400131120               exsr  sr_AnnullFigliEW;
060500131120             endif;
060600131120
060700131120             // -?"Annullamento" eventuale record in tab. "3CE"?
060800130423             exsr  sr_Elaboraz3CE;
060900131120
061000131120           EndIf;
061100131120
061200131120         ENDIF;
061300130220
061400130220       ENDSR;
061500140107
061600140107       //--------------------------------------------------------------
061700140107       //?Avviso dei figli NON EasySped-WEB rimasti in "3C"            ?
061800140107       //?(con unificante appena annullato in tab. "3C" e "3EW").      ?
061900140107       //--------------------------------------------------------------
062000140107       BEGSR  sr_PrtDettErr2;
062100140107
062200140108         // -?Stampa elenco figli NON EasySped-WEB?
062300140108         //  ?(da inviare via e-mail)?
062400140108         For  xx = 2  To  %elem(sch_SKC);
062500140108
062600140108           if  sch_SKC(xx) = *zero;
062700140108             leave;
062800140108           endif;
062900140108
063000140108           If  sch_SKEW(xx) <> *on;
063100140108
063200140108             clear  k03tabel00;
063300140108             k_TBLkut = 1;
063400140108             k_TBLcod = '3C';
063500140108             k_TBLkey = %editc( sch_SKC(xx) : 'X' );
063600140108             chain(e)  %kds( k03tabel00 )  TABEL;
063700140108
063800140108             If  NOT %error         and
063900140108                 %found(TABEL00F)   and   TBLflg = *blank;
064000140108
064100140108               ds3C = TBLuni;
064200140108
064300140108               if  Not %open(PrtEMAIL2);
064400140108                 open  PrtEmail2;
064500140108                 *inOE = *on;
064600140108               endif;
064700140108
064800140108               if  *inOE;
064900140108                 except  ErrTxt;
065000140108                 *inOE = *off;
065100140108               endif;
065200140108
065300140108               except  ErrDet;
065400140108
065500140108             EndIf;
065600140108
065700140108           EndIf;
065800140108
065900140108         EndFor;
066000140107
066100140107       ENDSR;
066200131120
066300131120       //--------------------------------------------------------------
066400140124       //?Annullamento logico in tab. "3C" di anche i figli "ESWEB"    ?
066500140124       //?dell'unificante appena annullato.                            ?
066600131120       //--------------------------------------------------------------
066700131120       BEGSR  sr_AnnullFigliEW;
066800131120
066900131120         // -?Elaborazione di TUTTI i figli del cliente in esame?
067000131120         FOR  xx = 2  TO  %elem(sch_SKC);
067100131120
067200131120           if  sch_SKC(xx) = *zero;
067300131120             leave;
067400131120           endif;
067500131120
067600131120           // -?...SE anch'esso con EasySpedWEB?
067700131120           If  sch_SKEW(xx) = *on;
067800131120
067900131120             clear  k03tabel00;
068000131120             k_TBLkut = 1;
068100131120             k_TBLcod = '3C';
068200131120             k_TBLkey = %editc( sch_SKC(xx) : 'X' );
068300131120             chain(e)  %kds( k03tabel00 )  TABEL;
068400131120
068500131120             // -?...SE non già annullato?
068600131120             if  NOT %error        and
068700131120                 %found(TABEL00F)  and  TBLflg = *blank;
068800131120
068900131120               ds3C = TBLuni;
069000131120
069100131120               // -?...SE ancora figlio del padre appena annullato?
069200131219               if  §3Ccba = c_EasyWEB  and
069300131219                   §3Ccks = %int( %subst( SQL_ke1 : 2 : 7 ) );
069400131120
069500131120                 // -?Stampa?
069600131120                 if  *inOF;
069700131120                   except  PrtTxt;
069800131120                   *inOF = *off;
069900131120                 endif;
070000131120                 except  PrtDet3C;
070100131120
070200131120                 // -?Annullamento tab. "3C"?
070300131120                 TBLflg = '*';
070400131120                 TBLftt = '1';
070500131120                 clear  TBLftr;
070600131120                 clear  TBLdtr;
070700140124                 //____________
070800140124                 update  TABEL;
070900140124                 //¯¯¯¯¯¯¯¯¯¯¯¯
071000131120
071100131120               endif;
071200131120
071300131120             endif;
071400131120
071500131120           EndIf;
071600131120
071700131120         ENDFOR;
071800131120
071900131120       ENDSR;
072000130423
072100130423       //--------------------------------------------------------------
072200140528       //?Annullamento logico dei records in tab. "3CE" (padre e figli)?
072300130423       //--------------------------------------------------------------
072400130423       BEGSR  sr_Elaboraz3CE;
072500130423
072600140528         // -?"Annullamento" record TNTBE00F del padre?
072700130423         clear  k05tntbe01;
072800130423         k_TBEcod = '3CE';
072900141001         k_TBEke1 = %subst( SQL_ke1 : 2 );
073000130423         chain(e)  %kds( k05tntbe01 : 2 )  TNTBE000;
073100140528         If  NOT %error         and
073200130423             %found(TNTBE01L)   and   TBEatb = *blank;
073300130423           TBEatb = 'A';
073400130423           TBEftt = 'S';
073500130423           clear  TBEftr;
073600140124           //_______________
073700140124           update  TNTBE000;
073800140124           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
073900140528         EndIf;
074000140528
074100140528         // -?Annullamento di TUTTI i figli del cliente in esame?
074200140528         //  ?(sempre in tab. "3CE")?
074300140528         clear  k05tntbe01;
074400140528         k_TBEcod = '3CE';
074500140528         For  xx = 2  TO  %elem(sch_SKC);
074600140528
074700140528           if  sch_SKC(xx) = *zero;
074800140528             leave;
074900140528           endif;
075000140528
075100140528           k_TBEke1 = %editc( sch_SKC(xx) : 'X' );
075200140528           chain(e)  %kds( k05tntbe01 : 2 )  TNTBE000;
075300140528
075400140528           // -?...SE non già annullato?
075500140528           if  NOT %error         and
075600140528               %found(TNTBE01L)   and   TBEatb = *blank;
075700140528             TBEatb = 'A';
075800140528             TBEftt = 'S';
075900140528             clear  TBEftr;
076000140528             //_______________
076100140528             update  TNTBE000;
076200140528             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
076300140528           endif;
076400140528
076500140528         EndFor;
076600130423
076700130423       ENDSR;
076800121024
076900121024       //--------------------------------------------------------------
077000121024       //?Stampa segnalazioni dell'errore rilevato.                    ?
077100121024       //--------------------------------------------------------------
077200121024       BEGSR  sr_PrintErr;
077300121113
077400121113         // -?Stampa segnalazione?
077500121119         if  Not %open(PrtEMAIL);
077600121119           open  PrtEmail;
077700121119           *inOF = *on;
077800121119         endif;
077900121119         if  *inOF;
078000121119           except  PrtTxt;
078100121119           *inOF = *off;
078200121119         endif;
078300121113         except  PrtErr;
078400121113         except  PrtEnd;
078500121024
078600121024         // -?Stampa Dump?
078700121024         Dump(A);
078800121024
078900121024         // -?Stampa Job-Log?
079000121024         Qcmd = 'DSPJOBLOG job(*) output(*print)';
079100121024         exsr  sr_ExecCmd;
079200121024
079300121024       ENDSR;
079400121024
079500121024       //--------------------------------------------------------------
079600121024       //?Operazioni finali.                                           ?
079700121024       //--------------------------------------------------------------
079800121024       BEGSR  sr_RoutEnd;
079900121024
080000121024         // -?Chiusura file stampa & Cancellazione override (TUTTE)?
080100121024         exsr sr_DeleteOvr;
080200130305
080300130305         // -?Chiusura funzioni precedentemente aperte?
080400140107         Selettore_Record_TIVSS ( 'SETONLR' );
080500130305         ubLeg3C_Close ();
080600121024
080700121024         // -?"Chiusura" *pgm?
080800121024         return;
080900121024
081000121024       ENDSR;
081100121024
081200121024       //--------------------------------------------------------------
081300121024       //?Chiusura file di stampa e cancellazione override.            ?
081400121024       //--------------------------------------------------------------
081500121025       BEGSR  sr_DeleteOvr;
081600121024
081700121024         // -?Chiusura file di stampa?
081800121024         if  %open(PrtEMAIL);
081900121024           close  PrtEMAIL;
082000121024         endif;
082100140108         if  %open(PrtEMAIL2);
082200140108           close  PrtEMAIL2;
082300140108         endif;
082400121024
082500121024         // -?Cancellazione override al file di stampa?
082600121024         Qcmd = c_CmdDltOvr;
082700121024         exsr  sr_ExecCmd;
082800121024
082900121024       ENDSR;
083000121024
083100121024       //--------------------------------------------------------------
083200121024       //?Esecuzione del comando (già impostato).                      ?
083300121024       //--------------------------------------------------------------
083400121024       BEGSR  sr_ExecCmd;
083500121024
083600121024         clear Qcap0100;
083700121024         Qcabcsdh = *off;
083800121024         Qcapa    = *off;
083900121024         Qcacmdss = *off;
084000121024         Qcaerved = *allX'00';
084100121024
084200121024         clear Qusec;
084300121024         Qusbprv  = %size(Qusec);
084400121024
084500121024         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
084600121024                           %size(Qcap0100) : 'CPOP0100' : *omit :
084700121024                           0 : 0 : Qusec);
084800121024
084900121024         //if  Qusei <> *blank;
085000121024         //  ...;
085100121024         //endif;
085200121024
085300121024       ENDSR;
085400121024
085500121024      /end-free
085600121024
085700131120       //---------------------------------------------------------------
085800140108       //?Spool di stampa (per e-mail): clienti annullati in tab. "3EW" ?
085900131120       //---------------------------------------------------------------
086000121024
086100131120       // -?Testata?
086200121024     oPrtEMAIL  e            PrtTxt            2
086300121119     o                       SDSpgm
086400121119     o                                        +   1 '-'
086500121119     o                                        +   1 'ELENCO CLIENTI AN-
086600121024     o                                              NULLATI IN TAB. "3-
086700121113     o                                              EW" (data annullam-
086800121113     o                                              ento applicativo l-
086900121113     o                                              imite:'
087000131120     o                       s_DataLim     y  +   1
087100121113     o                                        +   0 ')'
087200121119     o                                        +   2 '-'
087300131120     o                       s_Date        y  +   2
087400121119     o                                        +   2 '-'
087500131120     o                       s_Time           +   2 '0 :  :  '
087600121024     o          e            PrtTxt      0
087700140124     o                       SDSpgm
087800121119     o                                        +   1 '-'
087900121119     o                                        +   1 'ELENCO CLIENTI AN-
088000121024     o                                              NULLATI IN TAB. "3-
088100121113     o                                              EW" (data annullam-
088200121113     o                                              ento applicativo l-
088300121113     o                                              imite:'
088400131120     o                       s_DataLim     y  +   1
088500121113     o                                        +   0 ')'
088600121024     o          e            PrtTxt      2
088700121025     o                                              'Cliente        '
088800121025     o                                        +   1 'Strategi Usr Nr'
088900121119     o                                        +   3 'FlS'
089000121025     o                                        +   3 'Serie'
089100121025     o                                        +   3 '  Range  Segnacolli  '
089200121024     o          e            PrtTxt      1  1
089300121025     o                                              '--------       '
089400121025     o                                        +   1 '---------------'
089500121119     o                                        +   3 '---'
089600121025     o                                        +   3 '-----'
089700121025     o                                        +   3 '---------------------'
089800131120       // -?Unificante annullato in tab. "3EW" (e "3C")?
089900121024     o          e            PrtDet      1
090000121025     o                       SQL_ke1
090100121025     o                       SQL_ke2          +   1
090200121119     o                       §3EWfls       3  +   3
090300121025     o                       §3EWnrs       3  +   4
090400121025     o                       §3EWnsi       1  +   5
090500121025     o                                        +   1 '-'
090600121025     o                       §3EWnsf       1  +   1
090700131120       // -?Singolo Figlio annullato in tab. "3C"?
090800131120     o          e            PrtDet3C    1
090900131219     o                       TBLkey           +   1
091000131120       // -?Salto di un riga (vuota)?
091100121025     o*//       e            PrtSkip     1
091200131120       // -?Segnalazione Errore SQL (Dump & JobLog)?
091300121113     o          e            PrtErr      2
091400121113     o                                              'RILEVATO ERRORE: +
091500121113     o                                               stampati DUMP e +
091600121113     o                                               JOBLOG.'
091700121113     o          e            PrtErr      0
091800140124     o                                              'RILEVATO ERRORE: +
091900121113     o                                               stampati DUMP e +
092000121113     o                                               JOBLOG.'
092100140124       // -?Segnalazione Errore "Unificante riabilitato as EasySpedWEB"?
092200140124     o          e            PrtErr0     1
092300140505     o                                        +  16 'Il cliente risult-
092400140505     o                                              ta riabilitato ad -
092500140505     o                                              EasySpedWEB su Str-
092600140505     o                                              ategi: NON annulla-
092700140505     o                                              to in tab."3EW".'
092800131120       // -?Segnalazione Errore restituito da UBLEG3C?
092900131120     o          e            PrtErr1     1
093000140124     o                                        +  16 'Rilevato errore i-
093100131209     o                                              n fase di reperim.-
093200131209     o                                               legami in tab."3C-
093300131209     o                                              " (non annullata):'
093400140124     o                       s_EsitoLeg3C     +   1
093500131120       // -?Segnalazione Errore "Unificante con figli NON EasySpedWEB"?
093600131120     o          e            PrtErr2     1
093700140124     o                                        +  16 'Il cliente risult-
093800131120     o                                              a avere'
093900140124     o                       s_NrFigliNoEW z  +   1
094000140124     o                                        +   1 'figli NON EasySpe-
094100140124     o                                              dWEB: essi NON son-
094200140124     o                                              o stati annullati -
094300140124     o                                              in tab."3C" - vedi-
094400140124     o                                               2° mail.'
094500131120       // -?Fine lista?
094600121024     o          e            PrtEnd      2
094700121025     o                                              '***  Fine Lista  ***'
094800140124     o                                        +   5 'Totale record:'
094900140124     o                       s_TotRecDel   1  +   0
095000121025     o          e            PrtEnd      0
095100121025     o                                              '***  Fine Lista  ***'
095200140108
095300140108       //---------------------------------------------------------------
095400140108       //?Spool di stampa (per e-mail): errori rilevati.                ?
095500140108       //---------------------------------------------------------------
095600140108
095700140108       // -?Testata?
095800140108     oPrtEMAIL2 e            ErrTxt            2
095900140108     o                       SDSpgm
096000140108     o                                        +   1 '-'
096100140108     o                                        +   1 'ERRORI RILEVATI I-
096200140108     o                                              N FASE DI ANNULLAM-
096300140108     o                                              . CLIENTI in Tab."-
096400140108     o                                              3EW"'
096500140108     o                                        +   2 '-'
096600140108     o                       s_Date        y  +   2
096700140108     o                                        +   2 '-'
096800140108     o                       s_Time           +   2 '0 :  :  '
096900140108     o          e            ErrTxt      0
097000140108     o                       SDSpgm
097100140108     o                                        +   1 '-'
097200140124     o                                        +   1 'ERRORI RILEVATI I-
097300140108     o                                              N FASE DI ANNULLAM-
097400140108     o                                              . CLIENTI in Tab."-
097500140108     o                                              3EW"'
097600140108     o          e            ErrTxt      1
097700140108     o                                        +  13 '(data annull. app-
097800140108     o                                              licativo limite:'
097900140108     o                       s_DataLim     y  +   1
098000140108     o                                        +   0 ')'
098100140108     o          e            ErrTxt      0
098200140108     o                                        +  13 '(data annull. app-
098300140108     o                                              licativo limite:'
098400140108     o                       s_DataLim     y  +   1
098500140108     o                                        +   0 ')'
098600140108     o          e            ErrTxt      2
098700140108     o                                              'Cliente '
098800140108     o                                        +   3 'Unificante     '
098900140108     o                                        +   0 'Strategi Usr Nr'
099000140108     o                                        +   3 'FlS'
099100140108     o                                        +   3 'Serie'
099200140108     o                                        +   3 'Supporto'
099300140108     o          e            ErrTxt      1  1
099400140108     o                                              '--------'
099500140108     o                                        +   3 '----------     '
099600140108     o                                        +   0 '---------------'
099700140108     o                                        +   3 '---'
099800140108     o                                        +   3 '-----'
099900140108     o                                        +   3 '--------'
100000140108       // -?Figlio NON EasySpedWEB in tab. "3C"?
100100140108     o          e            ErrDet      1
100200140108     o                       TBLkey
100300140108     o                       SQL_ke1          +   3
100400140108     o                       SQL_ke2          +   0
100500140108     o                       §3Cfls        3  +   3
100600140108     o                       §3Cnrs        3  +   4
100700140108     o                       §3Ccba           +   6
100800140108       // -?Salto di un riga (vuota)?
100900140108     o*//       e            ErrSkip     1
101000140108       // -?Fine lista?
101100140108     o          e            ErrEnd      2
101200140108     o                                              '***  Fine Lista  ***'
101300140124     o                                        +   5 'Totale clienti:'
101400140124     o                       s_TotErr3C    1  +   0
101500140108     o          e            ErrEnd      0
101600140108     o                                              '***  Fine Lista  ***'
101700131120
101800131120       //--------------------------------------------------------------
101900131120       //?Definizione schiere a tempo di compilazione                  ?
102000131120       //--------------------------------------------------------------
102100131120
102200131209** -?sk_ErrEsitoLeg3C:?Esiti errati nel reperimento legami in tab. "3C" (45 char)?
102300131209Errore in apertura del file TABEL00F    1
102400131209Cliente NON trovato in tab. "3C"        2
102500131209AVVISO: cliente già annullato in tab"3C"3
102600131209Errore in chiusura del file TABEL00F    4
102700131209Errore NON previsto...                  5
