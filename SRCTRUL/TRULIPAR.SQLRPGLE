000100150413       //==============================================================
000200160923       //?TRULIPA * Creazione file AZIPA00F / WFIPA10F / WFIPA20F      ?
000300160923       //?          (Indice Pubbliche Amministrazioni)                 ?
000400150413       //==============================================================
000500150413
000600150413       //--------------------------------------------------------------
000700150413       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800150413       //--------------------------------------------------------------
000900150413
001000160916     /*PRM dbgview(*source)
001100150413     /*END
001200150413
001300150413       //--------------------------------------------------------------
001400150413       //?Specifiche di controllo.                                     ?
001500150413       //--------------------------------------------------------------
001600150413
001700150413     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800150413     h dftactgrp(*no)
001900160926     h bnddir('UBBNDDIR':'TRUL')
002000150413     h alwnull(*inputonly)
002100150413
002200150413       //--------------------------------------------------------------
002300150413       //?Dichiarazione file.                                          ?
002400150413       //--------------------------------------------------------------
002500150413
002600150413       // -?Indice delle Pubbliche Amministrazioni: Amministrazioni?
002700150414       //  ?(dati scaricati da internet)?
002800160916     fWFIPA10F  Uf   e             disk    usropn
002900150413
003000150413       // -?Indice delle Pubbliche Amministrazioni: Unità Organizzative?
003100150414       //  ?(dati scaricati da internet)?
003200160916     fWFIPA20F  Uf   e             disk    usropn
003300150414
003400150414       // -?Indice delle Pubbliche Amministrazioni (*file BRT)?
003500160916     fWFIPA00F  O    e             disk    usropn
003600160916     f                                     extdesc('UNITRAGRP/AZIPA00F')
003700160926
003800160926       // -?File di stampa errori per eventuale invio e-mail?
003900160926     fPrtEMAIL  o    f  132        printer usropn  oflind(*inOF)
004000150413
004100150413       //--------------------------------------------------------------
004200150413       //?Definizione costanti.                                        ?
004300150413       //--------------------------------------------------------------
004400160926
004500160926       // -?Comandi di override al PrtF?
004600160926     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004700160926     d                                           file(PRTEMAIL) +
004800160926     d                                           pagesize(66 132) +
004900160926     d                                           lpi(6) cpi(10) +
005000160926     d                                           ovrscope(*actgrpdfn) +
005100160926     d                                           ')
005200160926     d c_CmdDltOvr     c                   const('DLTOVR +
005300160926     d                                            file(PRTEMAIL) +
005400160926     d                                            lvl(*actgrpdfn)')
005500150413
005600150413       // -?Caratteri maiuscoli?
005700150413     d c_CharMai       c                   const('ABCDEFGHIJKLM+
005800150413     d                                            NOPQRSTUVWXYZ+
005900150413     d                                            ÀÁÂÃÄÅ+
006000150413     d                                            ÆÇ+
006100150413     d                                            ÈÉÊË+
006200150413     d                                            ÌÍÎÏ+
006300150413     d                                            ÒÓÔÕÖ+
006400150413     d                                            ÙÚÛÜ')
006500150413       // -?Caratteri minuscoli?
006600150413     d c_CharMin       c                   const('abcdefghijklm+
006700150413     d                                            nopqrstuvwxyz+
006800150413     d                                            àáâãäå+
006900150413     d                                            æç+
007000150413     d                                            èéêë+
007100150413     d                                            ìíîï+
007200150413     d                                            òóôõö+
007300150413     d                                            ùúûü')
007400160916
007500160916       // -?Costante per controllo "caratteri solo numerici"?
007600160916     d c_Digits        c                   const('0123456789')
007700150413
007800150413       //--------------------------------------------------------------
007900150413       //?Definizione schiere.                                         ?
008000150413       //--------------------------------------------------------------
008100150413
008200150512       // -?Messaggi di errore?
008300160916     d sk_Msg          s             78    dim( 7) ctdata perrcd(1)
008400150413
008500150413       //--------------------------------------------------------------
008600150413       //?Definizione aree dati.                                       ?
008700150413       //--------------------------------------------------------------
008800150413
008900150413       // -?Dati utente?
009000150413     d §AzUte        e ds                  extname(AZUTE00F)
009100150413     d                                     dtaara
009200150413     d §DatiUte      e ds                  extname(dDatiUte)
009300150413     d                                     dtaara
009400150413
009500150413       //--------------------------------------------------------------
009600150413       //?Definizione strutture dati.                                  ?
009700150413       //--------------------------------------------------------------
009800150413
009900150413       // -?Parametri ricevuti?
010000160926     d KPJBA         e ds
010100160926     d TRULIPAds     e ds
010200160926     d prm_Interatt    s              1a
010300150415
010400150415       // -?Dati elaborati via SQL?
010500160916     d WFIPA1_ds     e ds                  extname(WFIPA10F)  inz
010600160916     d WFIPA2_ds     e ds                  extname(WFIPA20F)  inz
010700160926
010800160926       // -?Tab. "MRA" = Bart-Mailing x TRULIPAR?
010900160926     d dMRAdan       e ds                  inz
011000160926     d   §MRAddes    e                     inz('*ERR Indice Pubbl. Amm.ni')
011100160926     d   §MRAdreg    e                     inz('COR')
011200160926     d   §MRAdmitt   e                     inz('ced')
011300160926     d   §MRAddest   e                     inz('antonella.gallucci+
011400160926     d                                          @brt.it;+
011500160926     d                                          stefano.merighi+
011600160926     d                                          @brt.it')
011700160926     d   §MRAdidpro  e                     inz('1')
011800160926     d   §MRAdoutqi  e                     inz('EMAILIN')
011900160926
012000160926       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
012100160926     d TRTCM1ds      e ds                  inz
012200160926       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
012300160926     d   §CM1mitt    e                     inz('ced')
012400160926       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
012500160926     d   §CM1dst     e                     inz('antonella.gallucci+
012600160926     d                                          @brt.it;+
012700160926     d                                          stefano.merighi+
012800160926     d                                          @brt.it')
012900160926       //  ?·§CM1tips = Tipo lettera via e-mail:?
013000160926       //   ?"LET" = testo allegato in corpo con logo?
013100160926       //           ?(richiede righe libere iniziali per il logo)?
013200160926       //   ?"COR" = testo integrato senza logo?
013300160926       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
013400160926     d   §CM1tips    e                     inz('COR')
013500160926       //  ?·§CM1po   = Filiale?
013600160926     d   §CM1po      e                     inz('046')
013700160926       //  ?·§CM1var  = Oggetto e-mail?
013800160926     d   §CM1var     e                     inz('*OBJM*+
013900160926     d                                     ERRORI in aggiornamento +
014000160926     d                                     Indice Pubbliche Amministra-
014100160926     d                                     zioni')
014200160926       //  ?·§CM1sts  = Stato?
014300160926     d   §CM1sts     e                     inz(*off)
014400160926       //  ?·§CM1sts  = Id processo?
014500160926     d   §CM1idp     e                     inz('1')
014600160926
014700160926       // -?Status ds?
014800160926     d Status         sds
014900160926     d   SDSpgm          *proc
015000160926     d*//SDSprm          *parms
015100160926     d*//SDSdta              191    198
015200160926     d   SDSjobName          244    253
015300160926     d   SDSjobUser          254    263
015400160926     d   SDSjobNbr           264    269s 0
015500160926
015600160926       // -?Data/Ora attuali?
015700160926     d wTime_ds        ds                  inz
015800160926     d   wDate                        8s 0 inz
015900160926     d   wTime                        6s 0 inz
016000150413
016100150413       //--------------------------------------------------------------
016200150413       //?Definizione variabili globali.                               ?
016300150413       //--------------------------------------------------------------
016400150415
016500150415       // -?Flags booleani?
016600150415     d $EoF            s               n   inz
016700150413
016800150413       // -?File di Flusso?
016900150413     d wSTMF           s             50    inz('/temp/iPA/')
017000150413
017100150413       // -?File su AS/400?
017200150415     d wLibr           s             11    inz
017300150415     d wFile           s             10    inz
017400150414
017500150414       // -?Campi di comodo?
017600150414     d wChar           s                   like(IP2camm)  inz
017700150414
017800150414       // -?Stringa SQL da eseguire?
017900150414     d wSQL            s           5000    inz  varying
018000160926
018100160926       // -?Codice errore rilevato?
018200160926     d wErr            s              3  0 inz
018300160926
018400160926       // -?Data *EUR?
018500160926     d wDate_Eur       s               d   inz  datfmt(*EUR)
018600150413
018700150413       //--------------------------------------------------------------
018800150413       //?Definizione prototipi procedure usate.                       ?
018900150413       //--------------------------------------------------------------
019000150413
019100150413       // -?Reperimento dati utente?
019200150413     d TIBS34ds      e ds                  inz
019300150413      /copy gaitrasrc/srcProtoPR,TIBS34R
019400150415
019500150415       // -?Reperimento NETA sistema AS/400 corrente?
019600150415     d currSysNeta     s              8a   inz
019700150415      /copy gaitrasrc/srcProtoPR,UBRTVNETA
019800160926
019900160926       // -?Reperimento dati tabelle?
020000160926      /copy gaitrasrc/srcProtoPI,TRULTAB
020100160926      /copy gaitrasrc/srcProtoPR,TRULTAB
020200150413
020300150413       // -?Parametri API QCAPCMD (Process Commands)?
020400150413     d Qcmd            s           2048    inz  varying
020500150413      /copy qSysInc/qRpgleSrc,QCAPCMD
020600150413       // -?API QCAPCMD (Process Commands)?
020700150413      /copy gaitrasrc/srcProtoPR,QCAPCMD
020800150413
020900150413       // -?Parametri gestione errori API.?
021000150413      /copy qsysinc/qrpglesrc,QUSEC
021100150413
021200150413       //--------------------------------------------------------------
021300150413       //?Definizione key-list.                                        ?
021400150413       //--------------------------------------------------------------
021500150413
021600150413
021700150413       //--------------------------------------------------------------
021800150413       //?Riepilogo indicatori utilizzati.                             ?
021900150413       //--------------------------------------------------------------
022000150413
022100150413
022200150413       //--------------------------------------------------------------
022300150413       //?M A I N - L I N E                                            ?
022400150413       //--------------------------------------------------------------
022500150413
022600150413     c     *Entry        plist
022700160926     c                   parm                    KPJBA
022800160926     c                   parm                    prm_Interatt
022900160926     c*//                parm                    trulIPAds
023000150413
023100150413      /free
023200150413
023300150413       // -?Operazioni iniziali?
023400150413       exsr  sr_RoutInz;
023500150512
023600151015       // -?Esecuzione delle operazioni richieste?
023700151015       exsr  sr_Execute;
023800150413
023900150413       // -?Operazioni finali?
024000150413       exsr  sr_RoutEnd;
024100150413
024200150413       //--------------------------------------------------------------
024300150413       //?Operazioni iniziali.                                         ?
024400150413       //--------------------------------------------------------------
024500150413       BEGSR  sr_RoutInz;
024600150413
024700150413         // -?Impostazione opzioni per SQL?
024800150413         exec sql   set  option  DynUsrPrf = *Owner,
024900150413                                 CloSqlCsr = *EndMod;
025000150413
025100150413         // -?Impostazione chiusura?
025200150413         *inLR = *on;
025300160926
025400160926         // -?Ricezione parametri?
025500160926         trulIPAds = KPJBU;
025600151015
025700151015         // -?Pulizia parametri di output?
025800151015         clear  ulIPAerr;
025900151015         clear  ulIPAmsg;
026000160926
026100160926         // -?Reperimento dati job?
026200160926         exsr  sr_DatiJob;
026300151015
026400151015         // -?Controllo dei parametri ricevuti?
026500151015         // ·?Verifica ricezione indirizzario (SE richiesta importazione)?
026600151015         If  ulIPAcrtF = 'S';
026700151015           select;
026800151015             // -?Indirizzario "Amministrazioni" obbligatorio?
026900151015             when  ulIPAip1 = *blank;
027000151015               ulIPAerr = *on;
027100151015               ulIPAmsg = sk_Msg(01);
027200160926               if  %parms() < 2;
027300160926                 wErr = 1;
027400160926                 exsr  sr_SendEmail;
027500160926               endif;
027600151015               exsr  sr_RoutEnd;
027700151015             // -?Indirizzario "Unità Organizzative" obbligatorio?
027800151015             when  ulIPAip2 = *blank;
027900151015               ulIPAerr = *on;
028000151015               ulIPAmsg = sk_Msg(02);
028100160926               if  %parms() < 2;
028200160926                 wErr = 1;
028300160926                 exsr  sr_SendEmail;
028400160926               endif;
028500151015               exsr  sr_RoutEnd;
028600151015           endsl;
028700151015         EndIf;
028800151015         Select;
028900151015           // ·?Richiesta import dei dati su AS/400?
029000151015           When  ulIPAcrtF <> *blank  and  ulIPAcrtF <> 'S';
029100151015             ulIPAerr = *on;
029200151015             ulIPAmsg = sk_Msg(03);
029300160926             if  %parms() < 2;
029400160926               wErr = 1;
029500160926               exsr  sr_SendEmail;
029600160926             endif;
029700151015             exsr  sr_RoutEnd;
029800160923           // ·?Richiesto aggiornamento dei dati in AZIPA00F?
029900151015           When  ulIPAipa  <> *blank  and  ulIPAipa  <> 'S';
030000151015             ulIPAerr = *on;
030100151015             ulIPAmsg = sk_Msg(04);
030200160926             if  %parms() < 2;
030300160926               wErr = 1;
030400160926               exsr  sr_SendEmail;
030500160926             endif;
030600151015             exsr  sr_RoutEnd;
030700160923           // ·?Richiesta cancellazione nuovi dati su AS/400?
030800160916           When  ulIPAdltI <> *blank  and  ulIPAdltI <> 'S';
030900151015             ulIPAerr = *on;
031000160916             ulIPAmsg = sk_Msg(05);
031100160926             if  %parms() < 2;
031200160926               wErr = 1;
031300160926               exsr  sr_SendEmail;
031400160926             endif;
031500151015             exsr  sr_RoutEnd;
031600151015         EndSl;
031700160923         // ·?NON richiesta alcuna attività?
031800160916         if  ulIPAcrtF = *blank  and  ulIPAipa  = *blank
031900160916                                 and  ulIPAdltI = *blank;
032000151015           ulIPAerr = *on;
032100160916           ulIPAmsg = sk_Msg(06);
032200160926           if  %parms() < 2;
032300160926             wErr = 1;
032400160926             exsr  sr_SendEmail;
032500160926           endif;
032600151015           exsr  sr_RoutEnd;
032700151015         endif;
032800150415
032900150415         // -?Verifica del sistema AS/400 corrente?
033000150415         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
033100150415           exsr  sr_RoutEnd;
033200150415         endif;
033300160916
033400160916         // -?Impostazione nome della libreria dei *file WF/AZIPA?
033500160916         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
033600160916           wLibr = 'UNITRAGRU/';
033700160916         else;
033800160916           wLibr = 'UNITRAGRP/';
033900160916         endif;
034000150413
034100150413       ENDSR;
034200150413
034300150413       //--------------------------------------------------------------
034400150413       //?Reperimento Dati del job (Utente/Operativi).                 ?
034500150413       //--------------------------------------------------------------
034600150413       BEGSR  sr_DatiJob;
034700150413
034800150413         in(e) §AzUte;
034900150413         if NOT %error;
035000150413           in(e) §DatiUte;
035100150413         endif;
035200150413         if %error or RSut = *blank;
035300150413           tibs34r ( tibs34ds );
035400150413           in §AzUte;
035500150413           in §DatiUte;
035600150413         endif;
035700150413
035800150413       ENDSR;
035900150512
036000150512       //--------------------------------------------------------------
036100150513       //?Esecuzione delle operazioni richieste.                       ?
036200150512       //--------------------------------------------------------------
036300150512       BEGSR  sr_Execute;
036400150512
036500160916         // -?Import dei nuovi dati delle Pubbliche Amministrazioni?
036600150512
036700151015         If  ulIPAcrtF = 'S';
036800150512
036900160916           // -?Creazione nuovo file WFIPA10F?
037000151015           wSTMF = ulIPAip1;
037100160916           wFile = 'WFIPA10F';
037200150512           exsr  sr_Import;
037300151015           if  ulIPAerr = *on;
037400150512             leavesr;
037500150512           endif;
037600160916           // -?Aggiornamento WFIPA10F.IP1COD (tutto maiuscolo)?
037700150512           exsr  sr_UpdIP1cod;
037800151015           if  ulIPAerr = *on;
037900150512             leavesr;
038000150512           endif;
038100150512
038200160916           // -?Creazione nuovo file WFIPA20F?
038300151015           wSTMF = ulIPAip2;
038400160916           wFile = 'WFIPA20F';
038500150512           exsr  sr_Import;
038600151015           if  ulIPAerr = *on;
038700150512             leavesr;
038800150512           endif;
038900160916           // -?Aggiornamento WFIPA20F.IP2CAMM (tutto maiuscolo)?
039000150512           exsr  sr_UpdIP2camm;
039100151015           if  ulIPAerr = *on;
039200150512             leavesr;
039300150512           endif;
039400150512
039500150512         EndIf;
039600150512
039700160923         // -?Copia dell'attuale file WFIPA00F in AZIPA00F?
039800160916
039900151015         If  ulIPAipa = 'S';
040000160919
040100160919           // -?Scrittura rec. nel file WFIPA00F?
040200160919           exsr  sr_WrtWFIPA;
040300160919           if  ulIPAerr = *on;
040400160919             leavesr;
040500160919           endif;
040600160916
040700160919           // -?Copia dal file WFIPA00F al file AZIPA00F   e?
040800160919           //  ?Cancellazione del work-file WFIPA00F?
040900160916           exsr  sr_CpyF;
041000160916           if  ulIPAerr = *on;
041100160916             leavesr;
041200160916           endif;
041300160916
041400150512         EndIf;
041500151015
041600160916         // -?Cancellazione dati nei work-file WFIPAx0F?
041700160916
041800160916         If  ulIPAdltI = 'S';
041900151015
042000160916           // -?Cancellazione dati nel work-file WFIPA10F?
042100160916           wFile = 'WFIPA10F ';
042200151015           Qcmd  = 'CLRPFM file(' + %trimR(wLibr) + %trimR(wFile) + ') +
042300151015                           mbr(*first)';
042400151015           exsr  sr_ExecCmd;
042500151015           if  Qusei <> *blank;
042600151015             exsr  sr_PrintErr;
042700151015             leavesr;
042800151015           endif;
042900160916           // -?Cancellazione dati nel work-file WFIPA20F?
043000160916           wFile = 'WFIPA20F ';
043100151015           Qcmd  = 'CLRPFM file(' + %trimR(wLibr) + %trimR(wFile) + ') +
043200151015                           mbr(*first)';
043300151015           exsr  sr_ExecCmd;
043400151015           if  Qusei <> *blank;
043500151015             exsr  sr_PrintErr;
043600151015             leavesr;
043700151015           endif;
043800151015
043900151015         EndIf;
044000150512
044100150512       ENDSR;
044200150413
044300150413       //--------------------------------------------------------------
044400150413       //?Import dei nuovi dati delle Pubbliche Amministrazioni.       ?
044500150413       //--------------------------------------------------------------
044600150413       BEGSR  sr_Import;
044700150415
044800150415         Qcmd = 'CLRPFM file(' + %trimR(wLibr) + %trimR(wFile) + ')';
044900150415         exsr  sr_ExecCmd;
045000160923
045100150415         if  Qusei <> *blank;
045200150415           exsr  sr_PrintErr;
045300150512           leavesr;
045400150415         endif;
045500150413
045600150415         Qcmd = 'CPYFRMIMPF fromstmf(''' + %trimR( wSTMF ) + ''') +
045700150415                            tofile(' + %trimR(wLibr) + %trimR(wFile) + ') +
045800160920                            mbropt(*ADD) +
045900160920                            rcddlm(*all) +
046000160920                            strdlm(*none) +
046100160920                            rmvblank(*none) +
046200160920                            flddlm(*tab) +
046300160920                            rplNullVal(*flddft)';
046400150413         exsr  sr_ExecCmd;
046500160923
046600150415         if  Qusei <> *blank;
046700150415           exsr  sr_PrintErr;
046800150512           leavesr;
046900150415         endif;
047000150413
047100150413       ENDSR;
047200150413
047300150413       //--------------------------------------------------------------
047400160916       //?Aggiornamento Codice Ente in WFIPA10F.                       ?
047500150413       //--------------------------------------------------------------
047600150414       BEGSR  sr_UpdIP1cod;
047700150413
047800160916         // -?Ciclo di elaborazione file WFIPA10F?
047900160916         open  WFIPA10F;
048000160916         read  WFIPA100;
048100150414
048200160916         DoW  not %eof(WFIPA10F);
048300150414
048400150414           // -?Conversione caratteri del codice da Maiuscoli a Minuscoli?
048500160916           //  ?in WFIPA10F?
048600150415           wChar = %xlate( c_CharMin : c_CharMai : IP1cod : 1 );
048700150414
048800150414           If  IP1cod <> wChar;
048900150414             IP1cod = wChar;
049000150414             //_______________
049100160916             update  WFIPA100;
049200150414             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
049300150414           Else;
049400160916             unlock  WFIPA10F;
049500150414           EndIf;
049600150414
049700160916           read  WFIPA100;
049800150414
049900150414         EndDo;
050000150415
050100160916         close  WFIPA10F;
050200150413
050300150413       ENDSR;
050400150413
050500150413       //--------------------------------------------------------------
050600160916       //?Aggiornamento Codice Ente in WFIPA20F.                       ?
050700150413       //--------------------------------------------------------------
050800150414       BEGSR  sr_UpdIP2camm;
050900150413
051000160916         // -?Ciclo di elaborazione file WFIPA20F?
051100160916         open  WFIPA20F;
051200160916         read  WFIPA200;
051300150414
051400160916         DoW  not %eof(WFIPA20F);
051500150414
051600150414           // -?Conversione caratteri del codice da Maiuscoli a Minuscoli?
051700160916           //  ?in WFIPA20F?
051800150415           wChar = %xlate( c_CharMin : c_CharMai : IP2camm : 1 );
051900150414
052000150414           If  IP2camm <> wChar;
052100150414             IP2camm = wChar;
052200150414             //_______________
052300160916             update  WFIPA200;
052400150414             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
052500150414           Else;
052600160916             unlock  WFIPA20F;
052700150414           EndIf;
052800150414
052900160916           read  WFIPA200;
053000150414
053100150414         EndDo;
053200150415
053300160916         close  WFIPA20F;
053400150413
053500150413       ENDSR;
053600150413
053700150413       //--------------------------------------------------------------
053800160916       //?Scrittura del file WFIPA00F.                                 ?
053900150413       //--------------------------------------------------------------
054000160916       BEGSR  sr_WrtWFIPA;
054100160916
054200160923         // -?Creazione file WFIPA00F (vuoto)?
054300160916         Qcmd = 'DLTF file(' + %trimR(wLibr) + 'WFIPA00F)';
054400160916         exsr  sr_ExecCmd;
054500160919
054600160916         Qcmd = 'CRTDUPOBJ obj(AZIPA00F) +
054700160916                           fromlib(' + %subst( wLibr : 1 :
054800160916                                   %len(%trimR(wLibr)) - 1 ) + ') +
054900160916                           objtype(*file) +
055000160916                           tolib(*fromLib) +
055100160916                           newobj(WFIPA00F) +
055200160916                           data(*no)';
055300160916         exsr  sr_ExecCmd;
055400160919
055500160916         if  Qusei <> *blank;
055600160916           exsr  sr_PrintErr;
055700160916           leavesr;
055800160916         endif;
055900150414
056000150414         // -?Preparazione SQL per estrazione dati?
056100150414         exsr  sr_PrepSQL;
056200150414
056300160916         // -?Apertura cursore e *file di output?
056400150414         exsr  sr_OpenCursor;
056500160916         open  WFIPA00F;
056600150414
056700160916         // -?Ciclo di elaborazione?
056800150414         DoW  Not $EoF;
056900150414           exsr  sr_ReadCursor;
057000150414         EndDo;
057100150414
057200160916         // -?Chiusura cursore e *file di output?
057300160916         close WFIPA00F;
057400150414         exsr  sr_CloseCursor;
057500150414
057600150414       ENDSR;
057700150414
057800150414       //--------------------------------------------------------------
057900150414       //?Preparazione SQL.                                            ?
058000150414       //--------------------------------------------------------------
058100150414       BEGSR  sr_PrepSQL;
058200150414
058300160916         // -?Preparazione SQL per lettura WFIPA20F/WFIPA10F?
058400150414         clear  wSQL;
058500150414         wSQL = 'select * +
058600160916                   from WFIPA20F inner join WFIPA10F +
058700160916                     on WFIPA20F.IP2camm = WFIPA10F.IP1cod +
058800150414                    for fetch only';
058900150414
059000150414         exec sql   prepare S1   from :wSQL;
059100150414
059200150414         // -?Dichiarazione cursore?
059300150414         exec sql   declare C1   cursor for S1;
059400150414
059500150414         if  SQLcode < *zero;
059600150414           $EoF = *on;
059700150414           exsr  sr_PrintErr;
059800150512           leavesr;
059900150414         endif;
060000150414
060100150414       ENDSR;
060200150414
060300150414       //--------------------------------------------------------------
060400150414       //?Apertura cursore.                                            ?
060500150414       //--------------------------------------------------------------
060600150414       BEGSR  sr_OpenCursor;
060700150414
060800150414         // -?Apertura del cursore?
060900150414         exec sql   open C1;
061000150414
061100150414         if  SQLcode < *zero;
061200150414           $EoF = *on;
061300150414           exsr  sr_PrintErr;
061400150512           leavesr;
061500150414         endif;
061600150414
061700150414       ENDSR;
061800150414
061900150414       //--------------------------------------------------------------
062000150414       //?Lettura cursore.                                             ?
062100150414       //--------------------------------------------------------------
062200150414       BEGSR  sr_ReadCursor;
062300150413
062400160916         clear  WFIPA1_ds;
062500160916         clear  WFIPA2_ds;
062600150413
062700160916         exec sql   fetch next   from C1   into :WFIPA2_ds, :WFIPA1_ds;
062800150413
062900150413         Select;
063000150413
063100150413           // -?Fine File?
063200150413           When  SQLcode = 100;
063300150413             $EoF = *on;
063400150413
063500150413           // -?Errore SQL?
063600150413           When  SQLcode < *zero;
063700150413             $EoF = *on;
063800150413             exsr  sr_PrintErr;
063900150512             leavesr;
064000150415
064100160916           //*·// -?Record da scartare x mancanza del C.F.?
064200160916           //*·When  IP1cf  = *blank  or  IP1cf = 'null ';
064300160916           //*·  leavesr;
064400160916           // -?Record da scartare x C.F. non numerico?
064500160916           When  %check( c_Digits : IP1cf ) > *zero;
064600160916             leavesr;
064700150415
064800150415           // -?Record da scartare x mancanza dell'UFFICIO?
064900150415           When  IP2uff = *blank  or  IP2uff = 'null ';
065000150415             leavesr;
065100150413
065200150413           // -?Elaborazione singolo record?
065300150413           Other;
065400150415             exsr  sr_WrtRec;
065500150413
065600150413         EndSl;
065700150413
065800150413       ENDSR;
065900150413
066000150413       //--------------------------------------------------------------
066100150413       //?Chiusura cursore.                                            ?
066200150413       //--------------------------------------------------------------
066300150413       BEGSR  sr_CloseCursor;
066400150413
066500150413         // -?Chiusura del cursore?
066600150413         exec sql   close C1;
066700150413
066800150413       ENDSR;
066900150413
067000150413       //--------------------------------------------------------------
067100160916       //?Elaborazione singolo record da WFIPA10F/WFIPA20F.            ?
067200150413       //--------------------------------------------------------------
067300150415       BEGSR  sr_WrtRec;
067400150413
067500160916         clear  AZIPA000;
067600150415
067700150415         IPAcod   = IP1cod;
067800150415         IPAdes   = IP1des;
067900150415         IPAloc   = IP1loc;
068000150415         IPAcap   = IP1cap;
068100150415         IPAprv   = IP1prv;
068200150415         IPAind   = IP1ind;
068300160923         //*·IPAcat   = IP1cat;
068400160923         //*·IPAtip   = IP1tip;
068500150415         IPAvcf   = IP1vcf;
068600150415         IPAcf    = IP1cf;
068700150415
068800160923         //*·if  IP2cod <> 'null ';
068900160923         //*·  IPAuoCod = IP2cod;
069000160923         //*·endif;
069100160923         //*·if  IP2aoo <> 'null ';
069200160923         //*·  IPAaoo   = IP2aoo;
069300160923         //*·endif;
069400150415         IPAuff   = IP2uff;
069500150415         if  IP2des <> 'null ';
069600150415           IPAuoDes = IP2des;
069700150415         endif;
069800150415         if  IP2loc <> 'null ';
069900150415           IPAuoLoc = IP2loc;
070000150415         endif;
070100150415         if  IP2cap <> 'null ';
070200150415           IPAuoCap = IP2cap;
070300150415         endif;
070400150415         if  IP2prv <> 'null';
070500150415           IPAuoPrv = IP2prv;
070600150415         endif;
070700150415         if  IP2ind <> 'null ';
070800150415           IPAuoInd = IP2ind;
070900150415         endif;
071000150415         if  IP2mail1 <> 'null ';
071100150415           IPAuoEml = IP2mail1;
071200150415         endif;
071300150415
071400160916         // -?Scrittura record in WFIPA00F?
071500150415         //______________
071600150415         write  AZIPA000;
071700150415         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
071800150413
071900150413       ENDSR;
072000160916
072100160916       //--------------------------------------------------------------
072200160916       //?Copia del work-file WFIPA00F appena creato in AZIPA00F.      ?
072300160916       //--------------------------------------------------------------
072400160916       BEGSR  sr_CpyF;
072500160916
072600160923         // -?Copia dei dati dal work-file WFIPA00F in AZIPA00F?
072700160916         Qcmd = 'CPYF fromfile(' + %trimR(wLibr) + 'WFIPA00F) +
072800160916                        tofile(' + %trimR(wLibr) + 'AZIPA00F) +
072900160916                      mbropt(*replace) crtfile(*no)';
073000160916
073100160916         exsr  sr_ExecCmd;
073200160916
073300160916         if  Qusei <> *blank;
073400160916           exsr  sr_PrintErr;
073500160916           leavesr;
073600160916         endif;
073700160916
073800160923         // -?Cancellazione del work-file WFIPA00F?
073900160916         Qcmd = 'DLTF file(' + %trimR(wLibr) + 'WFIPA00F)';
074000160916         exsr  sr_ExecCmd;
074100160916
074200160916       ENDSR;
074300150413
074400150413       //--------------------------------------------------------------
074500150413       //?Stampa segnalazione dell'errore rilevato via SQL             ?
074600150413       //--------------------------------------------------------------
074700150413       BEGSR  sr_PrintErr;
074800150413
074900151015         ulIPAerr = *on;
075000160916         ulIPAmsg = sk_Msg(07);
075100150512
075200150413         // -?Stampa del Dump?
075300150413         Dump(A);
075400150413
075500150413         // -?Stampa del Job-Log?
075600150413         Qcmd = 'DSPJOBLOG job(*) output(*print)';
075700150413         exsr  sr_ExecCmd;
075800160926
075900160926         // -?Invio della e-Mail?
076000160926         if  %parms() < 2;
076100160926           wErr = 2;
076200160926           exsr  sr_SendEmail;
076300160926         endif;
076400150413
076500160916         //*·// -?Chiusura del programma?
076600160916         //*·exsr  sr_RoutEnd;
076700150413
076800150413       ENDSR;
076900160926
077000160926       //--------------------------------------------------------------
077100160926       //?Invio e-Mail.                                                ?
077200160926       //--------------------------------------------------------------
077300160926       BEGSR  sr_SendEmail;
077400160926
077500160926         // -?Reperimento data e ora attuali?
077600160926         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
077700160926         wDate_Eur = %date( wDate : *Iso );
077800160926         wDate     = %dec( wDate_Eur );
077900160926
078000160926         // -?Override al file di stampa ed apertura dello stesso?
078100160926         If  NOT  %open( PrtEMAIL );
078200160926
078300160926           // -?Reperimento regole per mailing da tab. "MRA"?
078400160926           clear  dMRAdan;
078500160926           if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
078600160926                            *omit : *omit :
078700160926                            *omit : *omit :
078800160926                            *omit : *omit : *omit : *omit :
078900160926                            *omit : *omit : *omit : *omit :
079000160926                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
079100160926                            = *zero      AND
079200160926                 ds_TNTBE.TBEatb = *blank;
079300160926             dMRAdan = ds_TNTBE.TBEuni;
079400160926           endif;
079500160926
079600160926           // -?Impostazione regole per mailing?
079700160926           §CM1mitt = %trim( §MRAdmitt );
079800160926           §CM1dst  = %trim( §MRAddest );
079900160926           §CM1tips = §MRAdreg;
080000160926           §CM1var  = '*OBJM*' + §MRAddes;
080100160926           §CM1idp  = §MRAdidpro;
080200160926
080300160926           // -?Esecuzione override al file di stampa (per mailing)?
080400160926           Qcmd = c_CmdOvrPrtF
080500160926                + ' outq(' + %trim( §MRAdoutqi ) + ')'
080600160926                + ' usrdfndta(''' + TRTCM1ds + ''')';
080700160926           exsr  sr_ExecCmd;
080800160926
080900160926           // -?Apertura file di stampa?
081000160926           open  PrtEMAIL;
081100160926           *inOF = *on;
081200160926
081300160926         EndIf;
081400160926
081500160926         // -?Preparazione e-Mail per il singolo errore rilevato?
081600160926         if  *inOF;
081700160926           except  PRTtxt;
081800160926           *inOF = *off;
081900160926         endif;
082000160926
082100160926         // -?Stampa del messaggio di errore?
082200160926         Select;
082300160926           When  wErr = 1;
082400160926             except  PRTerr01;
082500160926           When  wErr = 2;
082600160926             except  PRTerr01;
082700160926             except  PRTerr02;
082800160926         EndSl;
082900160926
083000160926       ENDSR;
083100150413
083200150413       //--------------------------------------------------------------
083300150413       //?Operazioni finali.                                           ?
083400150413       //--------------------------------------------------------------
083500150413       BEGSR  sr_RoutEnd;
083600160926
083700160926         // -?Chiusura applicazioni precedentemente aperte?
083800160926         cloTabella ( c_Tntbe );
083900160926
084000160926         // -?Chiusura del file di stampa e cancellazione override?
084100160926         if  %open( PrtEMAIL );
084200160926           except  PRTend;
084300160926           close  PrtEMAIL;
084400160926           Qcmd = c_CmdDltOvr;
084500160926           exsr  sr_ExecCmd;
084600160926         endif;
084700160926
084800160926         // -?Restituzione parametri?
084900160926         KPJBU = trulIPAds;
085000150413
085100150413         // -?Uscita dal *pgm?
085200150413         return;
085300150413
085400150413       ENDSR;
085500160926
085600160926       //--------------------------------------------------------------
085700160926       //?Esecuzione del comando (già impostato).                      ?
085800160926       //--------------------------------------------------------------
085900160926       BEGSR  sr_ExecCmd;
086000160926
086100160926         clear Qcap0100;
086200160926         Qcabcsdh = *off;
086300160926         Qcapa    = *off;
086400160926         Qcacmdss = *off;
086500160926         Qcaerved = *allX'00';
086600160926
086700160926         clear Qusec;
086800160926         Qusbprv  = %size(Qusec);
086900160926
087000160926         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
087100160926                           %size(Qcap0100) : 'CPOP0100' : *omit :
087200160926                           0 : 0 : Qusec);
087300160926
087400160926         //*·if  Qusei <> *blank;
087500160926         //*·  exsr  sr_PrintErr;
087600160926         //*·endif;
087700160926
087800160926       ENDSR;
087900150413
088000150413      /end-free
088100160926
088200160926       //--------------------------------------------------------------
088300160926       //?Spool di stampa (per e-mail).                                ?
088400160926       //--------------------------------------------------------------
088500160926
088600160926     oPrtEMAIL  e            PRTtxt            2
088700160926     o                       RSUt
088800160926     o                                        +   3 'LISTA ERRORI RILEVATI -
088900160926     o                                              IN FASE DI CREAZIONE'
089000160926     o                       SDSpgm           +   3
089100160926     o                       wDate         y  +   3
089200160926     o          e            PRTtxt      1
089300160926     o                       KNSIF
089400160926     o                       KNMUS            +   1
089500160926     o                                        +   2 '  NUOVO  INDICE PUBBLI-
089600160926     o                                              CA AMMINISTRAZIONE  '
089700160926     o                       wTime            +  16 '  :  :  '
089800160926      *
089900160926     o          e            PRTtxt      3
090000160926     o                                              'Messaggio di Errore'
090100160926     o          e            PRTtxt      1
090200160926     o                                              '==================='
090300160926      *
090400160926     o          e            PRTerr01    2
090500160926     o                       ulIPAmsg
090600160926      *
090700160926     o          e            PRTerr02    2
090800160926     o                                              'Consultare le sta-
090900160926     o                                              mpe del lavoro'
091000160926     o                       SDSjobNbr     3  +   1
091100160926     o                                        +   0 '/'
091200160926     o                       SDSjobUser       +   0
091300160926     o                                        +   0 '/'
091400160926     o                       SDSjobName       +   0
091500160926      *
091600160926     o          e            PRTend      3
091700160926     o                                        +  23 '***  Fine Lista  ***'
091800150512
091900150512       //--------------------------------------------------------------
092000150512       //?Definizione schiere a tempo di compilazione.                 ?
092100150512       //--------------------------------------------------------------
092200150512
092300151015** --?sk_Msg:?Messaggi di Errore?---------------------------------------------*
092400160923Indirizzario "Amministrazioni" obbligatorio?                                   1
092500160923Indirizzario "Unità Organizzative" obbligatorio?                               2
092600151015Richiesta "Importare dati su AS/400" con valore errato                         3
092700151015Richiesta "Aggiornare dati nel file AZIPA00F" con valore errato                4
092800160916Richiesta "Cancellare dati importati su AS/400" con valore errato              5
092900160926Impostare almeno una selezione: Importare e/o Aggiornare e/o Cancellare        6
093000160926Rilevato errore: controllare la stampa                                         7
