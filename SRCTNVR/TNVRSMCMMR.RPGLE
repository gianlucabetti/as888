000100130719       //==============================================================
000200130719       //?TNVRSMCMMR - Copia da tab."01"/"HTM" in file AZCMM/AZNTC00F  ?
000300130719       //==============================================================
000400130719
000500130719       //--------------------------------------------------------------
000600130719       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130719       //--------------------------------------------------------------
000800130719
000900130719     /*PRM dbgview(*source)
001000130719     /*END
001100130719
001200130719       //--------------------------------------------------------------
001300130719       //?Specifiche di controllo.                                     ?
001400130719       //--------------------------------------------------------------
001500130719
001600130719     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130719
001800130719       //--------------------------------------------------------------
001900130719       //?Dichiarazione file.                                          ?
002000130719       //--------------------------------------------------------------
002100130719
002200130719       // -?Tab. "01" = Commerciali?
002300130719     fTABEL00F  if   e           k disk
002400130719       // -?Tabella "HTM"?
002500130719     fTNTBE01L  if   e           k disk
002600130719
002700130719       // -?Anagrafica Commerciali?
002800130722     fAZCMM01L  Uf A e           k disk    usropn
002900130719       // -?Note/Rubrica Commerciali?
003000130722     fAZNTC01L  Uf A e           k disk    usropn
003100130719
003200130719       // -?Video Gestione?
003300130719     fTNVRSMCMMDcf   e             workstn
003400130719     f                                     indds(IndDspF)
003500130719     f                                     infds(InfDspF)
003600130719
003700130719       //--------------------------------------------------------------
003800130719       //?Definizione costanti.                                        ?
003900130719       //--------------------------------------------------------------
004000130719
004100130719       // -?Tasti funzionali a video?
004200130719     d c_F01           c                   const(x'31')
004300130719     d c_F02           c                   const(x'32')
004400130719     d c_F03           c                   const(x'33')
004500130719     d c_F04           c                   const(x'34')
004600130719     d c_F05           c                   const(x'35')
004700130719     d c_F06           c                   const(x'36')
004800130719     d c_F07           c                   const(x'37')
004900130719     d c_F08           c                   const(x'38')
005000130719     d c_F09           c                   const(x'39')
005100130719     d c_F10           c                   const(x'3A')
005200130719     d c_F11           c                   const(x'3B')
005300130719     d c_F12           c                   const(x'3C')
005400130719     d c_F13           c                   const(x'B1')
005500130719     d c_F14           c                   const(x'B2')
005600130719     d c_F15           c                   const(x'B3')
005700130719     d c_F16           c                   const(x'B4')
005800130719     d c_F17           c                   const(x'B5')
005900130719     d c_F18           c                   const(x'B6')
006000130719     d c_F19           c                   const(x'B7')
006100130719     d c_F20           c                   const(x'B8')
006200130719     d c_F21           c                   const(x'B9')
006300130719     d c_F22           c                   const(x'BA')
006400130719     d c_F23           c                   const(x'BB')
006500130719     d c_F24           c                   const(x'BC')
006600130719     d c_Enter         c                   const(x'F1')
006700130719     d c_RollDown      c                   const(x'F4')
006800130719     d c_RollUp        c                   const(x'F5')
006900130719
007000130719       //--------------------------------------------------------------
007100130719       //?Definizione schiere.                                         ?
007200130719       //--------------------------------------------------------------
007300130719
007400130719
007500130719       //--------------------------------------------------------------
007600130719       //?Definizione aree dati.                                       ?
007700130719       //--------------------------------------------------------------
007800130719
007900130719       // -?Dati utente?
008000130719     d §AzUte        e ds                  extname(AZUTE00F)
008100130719     d                                     dtaara
008200130719     d §DatiUte      e ds                  extname(dDatiUte)
008300130719     d                                     dtaara
008400130719
008500130719       //--------------------------------------------------------------
008600130719       //?Definizione strutture dati.                                  ?
008700130719       //--------------------------------------------------------------
008800130719
008900130719       // -?Status ds?
009000130719     d Status         sds
009100130719     d   SDSpgm          *proc
009200130719
009300130719       // -?InfDS?
009400130719     d InfDspF         ds
009500130719     d   dsp_aid             369    369a
009600130719
009700130719       // -?Indicatori su DspF?
009800130719     d IndDspF         ds                  inz
009900130719         // ·?Emissione messaggio di errore?
010000130719     d   ErrMessage                    n   overlay(IndDspF : 28)
010100130719         // ·?Riemissione videata?
010200130719     d   ErrGenerico                   n   overlay(IndDspF : 99)
010300130719
010400130719       // -?Parametri ricevuti?
010500130719     d KPJBA         e ds
010600130719
010700130719       // -?Tabelle usate:?
010800130719       // ·?"01" = Agenti Commerciali?
010900130719     d ds01          e ds                  inz
011000130719       // ·?"dHTM" = Default per testo HTML?
011100130719     d dHTM          e ds                  inz
011200130719
011300130719       //--------------------------------------------------------------
011400130719       //?Definizione variabili globali.                               ?
011500130719       //--------------------------------------------------------------
011600130719
011700130719       // -?Flags booleani?
011800130719     d $Fine           s               n   inz
011900130722     d $InzW01         s               n   inz(*on)
012000130719
012100130719       // -?Variabili per la gestione del video?
012200130719     d $Video          s              2    inz('W1')
012300130719
012400130719       // -?Campi di comodo?
012500130719     d wDate           s              8  0 inz
012600130719     d wDate_Amg       s               d   inz  datfmt(*YMD)
012700130719     d wDate_Iso       s               d   inz  datfmt(*ISO)
012800130719
012900130719       //--------------------------------------------------------------
013000130719       //?Definizione procedure usate.                                 ?
013100130719       //--------------------------------------------------------------
013200130719
013300130719       // -?Reperimento dati utente?
013400130719     d TIBS34ds      e ds                  inz
013500130719      /copy gaitrasrc/srcProtoPR,TIBS34R
013600130719
013700130719       // -?Parametri API QCAPCMD (Process Commands)?
013800130719     d Qcmd            s           2048    inz  varying
013900130719      /copy qSysInc/qRpgleSrc,QCAPCMD
014000130719       // -?API QCAPCMD (Process Commands)?
014100130719      /copy gaitrasrc/srcProtoPR,QCAPCMD
014200130719
014300130719       // -?Parametri gestione errori API.?
014400130719      /copy qsysinc/qrpglesrc,QUSEC
014500130719
014600130719       //--------------------------------------------------------------
014700130719       //?Definizione key-list.                                        ?
014800130719       //--------------------------------------------------------------
014900130719
015000130719       // -?File TABEL00F?
015100130719     d k03tabel00    e ds                  extname(TABEL00F : *key)
015200130719     d                                     prefix(k_)   inz
015300130719
015400130719       // -?File TNTBE01L?
015500130719     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
015600130719     d                                     prefix(k_)   inz
015700130719
015800130719       // -?File AZCMM01L?
015900130719     d k01azcmm01    e ds                  extname(AZCMM01L : *key)
016000130719     d                                     prefix(k_)   inz
016100130719
016200130719       // -?File AZNTC01L?
016300130719     d k02azntc01    e ds                  extname(AZNTC01L : *key)
016400130719     d                                     prefix(k_)   inz
016500130719
016600130719       //--------------------------------------------------------------
016700130719       //?Riepilogo indicatori usati.                                  ?
016800130719       //--------------------------------------------------------------
016900130719       //--------------------------------------------------------------
017000130719
017100130719       //--------------------------------------------------------------
017200130719       //?M A I N - L I N E.                                           ?
017300130719       //--------------------------------------------------------------
017400130719
017500130719     c     *Entry        plist
017600130719     c                   parm                    KPJBA
017700130719
017800130719      /free
017900130719
018000130719       // -?Operazioni iniziali?
018100130719       exsr  sr_RoutInz;
018200130719
018300130719       // -?Gestione videate?
018400130719       DoW  $Fine = *off;
018500130719
018600130719         select;
018700130719           // -?Gestione videata D1 (richiesta parametri)?
018800130719           when $Video = 'W1';
018900130719             exsr sr_GesW01;
019000130719           // -?? ? ? ? ??
019100130719           other;
019200130719             $Fine = *on;
019300130719         endsl;
019400130719
019500130719       enddo;
019600130719
019700130719       // -?Operazioni finali?
019800130719       exsr sr_RoutEnd;
019900130719
020000130719       //--------------------------------------------------------------
020100130719       //?Operazioni iniziali.                                         ?
020200130719       //--------------------------------------------------------------
020300130719       BEGSR  sr_RoutInz;
020400130719
020500130719         *inLR = *on;
020600130719
020700130719         // -?Reperimento dati job?
020800130719         exsr  sr_DatiJob;
020900130719
021000130719         // -?Reperimento data odierna?
021100130719         wDate = %int( %subst( %char( %dec( %timestamp() ) )
021200130719                               : 1 : 8 ) );
021300130719
021400130719         // -?Impostazione iniziale dei campi chiave?
021500130719         clear  k03tabel00;
021600130719         k_TBLkut = 1;
021700130719         k_TBLcod = '01';
021800130719         clear  k05tntbe01;
021900130719         k_TBEcod = 'HTM';
022000130719         k_TBEke2 = 'CC';
022100130719         clear  k01azcmm01;
022200130719         clear  k02azntc01;
022300130719
022400130719       ENDSR;
022500130719
022600130719       //--------------------------------------------------------------
022700130719       //?Gestione filtro di lancio.                                   ?
022800130719       //--------------------------------------------------------------
022900130719       BEGSR  sr_GesW01;
023000130719
023100130719         // -?Inizializzazione videata?
023200130719         if  $InzW01 = *on;
023300130722           T1Cpgm = SDSpgm;
023400130719           VIDclr = 'S';
023500130719           VIDwrt = ' ';
023600130719           $InzW01 = *off;
023700130719         endif;
023800130719
023900130719         // -?Emissione Window?
024000130719         write  TNVRCMMW;
024100130719         exfmt  TNVRCMMD;
024200130719
024300130719         clear  VIDmsg;
024400130719         clear  ErrMessage;
024500130719         clear  ErrGenerico;
024600130719
024700130719         SELECT;
024800130719
024900130719           // -?F3=Fine?
025000130719           WHEN  dsp_aid = c_F03;
025100130719             $Fine  = *on;
025200130719
025300130719           // -?Invio, F6=Conferma?
025400130719           OTHER;
025500130719             exsr  sr_CtrD01;
025600130719             if  ErrGenerico = *on  or  dsp_aid <> c_F06;
025700130719               leavesr;
025800130719             endif;
025900130719             exsr  sr_Copy_CMM;
026000130719
026100130719         ENDSL;
026200130719
026300130719       ENDSR;
026400130719
026500130719       //--------------------------------------------------------------
026600130719       //?Reperimento Dati del job (Utente/Operativi).                 ?
026700130719       //--------------------------------------------------------------
026800130719       BEGSR  sr_DatiJob;
026900130719
027000130719         in(E) §AzUte;
027100130719         if NOT %error;
027200130719           in(E) §DatiUte;
027300130719         endif;
027400130719         if %error or RSut = *blanks;
027500130719           clear TIBS34ds;
027600130719           tibs34r(tibs34ds);
027700130719           in §AzUte;
027800130719           in §DatiUte;
027900130719         endif;
028000130719
028100130719       ENDSR;
028200130719
028300130719       //--------------------------------------------------------------
028400130719       //?Controllo dati a video nel fmt D01.                          ?
028500130719       //--------------------------------------------------------------
028600130719       BEGSR  sr_CtrD01;
028700130719
028800130719         // -?Spegnimento indicatori di errore & posizionamento cursore?
028900130719         %subst(IndDspF : 28) = *off;
029000130719
029100130719       ENDSR;
029200130719
029300130719       //--------------------------------------------------------------
029400130719       //?Copia dei commerciali da tab. "01"/"HTM" in file AZCMM/AZNTC ?
029500130719       //--------------------------------------------------------------
029600130719       BEGSR  sr_Copy_CMM;
029700130719
029800130719         // -?Richiesta pulizia iniziale dei files AZCMM e AZNTC?
029900130719         If  VIDclr = 'S';
030000130719           exsr  sr_PuliziaFileAZ;
030100130722           if  ErrGenerico;
030200130722             leavesr;
030300130722           endif;
030400130719         EndIf;
030500130722
030600130722         if  not  %open(AZCMM01L);
030700130722           open  AZCMM01L;
030800130722         endif;
030900130722         if  not  %open(AZNTC01L);
031000130722           open  AZNTC01L;
031100130722         endif;
031200130719
031300130719         // -?Ciclo di lettura tab. "01" (e "HTM")?
031400130719         k_TBLkey = *loval;
031500130719         setll  %kds(k03tabel00)  TABEL;
031600130719         reade  %kds(k03tabel00 : 2)  TABEL;
031700130719
031800130719         DoW  Not  %eof(TABEL00F);
031900130719
032000130722           If  TBLflg = *blank;
032100130722
032200130722             ds01 = TBLuni;
032300130722
032400130722             // -?Reperimento altri dati da tab. "HTM"?
032500130722             exsr  sr_GetTabHTM;
032600130722
032700130722             // -?Scrittura o Aggiornamento files AZCMM e AZNTC?
032800130722             exsr  sr_WrtUpd_AZCMM;
032900130722             exsr  sr_WrtUpd_AZNTC01;
033000130722             exsr  sr_WrtUpd_AZNTC02;
033100130722
033200130722           EndIf;
033300130719
033400130719           // -?Lettura commerciale successivo in tab. "01"?
033500130719           reade  %kds(k03tabel00 : 2)  TABEL;
033600130719
033700130719         EndDo;
033800130722
033900130722         // -?Uscita dal programma?
034000130722         $Fine = *on;
034100130719
034200130719       ENDSR;
034300130719
034400130719       //--------------------------------------------------------------
034500130719       //?Pulizia files AZCMM00F e AZNTC00F.                           ?
034600130719       //--------------------------------------------------------------
034700130719       BEGSR  sr_PuliziaFileAZ;
034800130722
034900130722         if  %open(AZCMM01L);
035000130722           close  AZCMM01L;
035100130722         endif;
035200130722         if  %open(AZNTC01L);
035300130722           close  AZNTC01L;
035400130722         endif;
035500130719
035600130719         // -?Pulizia file AZCMM00F?
035700130719         Qcmd = 'CLRPFM file(*libl/AZCMM00F)';
035800130719         exsr  sr_ExecCmd;
035900130719         if  Qusei <> *blank;
036000130719           ErrGenerico = *on;
036100130719           ErrMessage  = *on;
036200130719           VIDmsg = 'Rilevato errore nella pulizia del file AZCMM00F';
036300130719           leavesr;
036400130719         endif;
036500130719
036600130719         // -?Pulizia file AZNTC00F?
036700130719         Qcmd = 'CLRPFM file(*libl/AZNTC00F)';
036800130719         exsr  sr_ExecCmd;
036900130719         if  Qusei <> *blank;
037000130719           ErrGenerico = *on;
037100130719           ErrMessage  = *on;
037200130719           VIDmsg = 'Rilevato errore nella pulizia del file AZNTC00F';
037300130719           leavesr;
037400130719         endif;
037500130719
037600130719       ENDSR;
037700130719
037800130719       //--------------------------------------------------------------
037900130719       //?Reperimento dati da tab. "HTM" per commerciale in esame.     ?
038000130719       //--------------------------------------------------------------
038100130719       BEGSR  sr_GetTabHTM;
038200130719
038300130719         clear  dHTM;
038400130719
038500130719         k_TBEke1 = TBLkey;
038600130719         chain  %kds(k05tntbe01 : 3)  TNTBE000;
038700130719
038800130719         if  %found(TNTBE01L)  and
038900130719             TBLflg = *blank   and  TBEatb = *blank;
039000130719           dHTM = TBEuni;
039100130719         endif;
039200130719
039300130719       ENDSR;
039400130719
039500130719       //--------------------------------------------------------------
039600130719       //?Scrittura/Aggiornamento singolo record in AZCMM00F.          ?
039700130719       //--------------------------------------------------------------
039800130719       BEGSR  sr_WrtUpd_AZCMM;
039900130719
040000130722         k_CMMcod = %int( %trimr( TBLkey ) );
040100130722
040200130719         // -?Verifica esistenza commerciale nel file AZCMM00F?
040300130719         If  VIDclr = *blank;
040400130719
040500130719           if  VIDwrt = 'S';
040600130719             setll  %kds(k01azcmm01)  AZCMM000;
040700130719           else;
040800130719             chain  %kds(k01azcmm01)  AZCMM000;
040900130719           endif;
041000130719
041100130719         EndIf;
041200130719
041300130719         Select;
041400130719
041500130719           // -?Richiesta la pulizia iniziale dei file AZCMM/AZNTC:?
041600130719           //  ?tutti i commerciali saranno da scrivere?
041700130719           When  VIDclr = 'S';
041800130719
041900130719           // -?Richiesta solo la copia dei commerciali mancanti  e?
042000130719           //  ?commerciale già esistente in AZCMM00F:  => uscita?
042100130719           When  VIDwrt = 'S'  and  %equal(AZCMM01L);
042200130719             leavesr;
042300130719
042400130719         EndSl;
042500130719
042600130719         // -?Pulizia del trk SE record da aggiungre?
042700130719         If  VIDclr = 'S'  or  VIDwrt = 'S';
042800130719           clear  AZCMM000;
042900130719           CMMcod = k_CMMcod;
043000130719         EndIf;
043100130719
043200130719         // -?Valorizzazione dei vari campi nel record?
043300130719         clear  CMMatb;
043400130719         CMMdes   = §01age;
043500130719         CMMpar   = §01par;
043600130719         CMMfun   = §01fun;
043700130719         CMMpli   = §01pcf;
043800130719         CMMple   = §01pce;
043900130719         if  §01dtDec > *zero;
044000130719           CMMdtIni = %int(§01dtDec);
044100130719         else;
044200130719           clear  CMMdtIni;
044300130719         endif;
044400130719         if  §01dtFin > *zero;
044500130719           CMMdtFin = %int(§01dtFin);
044600130719         else;
044700130719           clear  CMMdtFin;
044800130719         endif;
044900130722         if  %subst(TBLuni : 39 : 5) >= *zero;
045000130722           CMMdip   = §01dip;
045100130722         endif;
045200130719         CMMsoc   = §01soc;
045300130719         if  §01fil > *zero;
045400130719           CMMfil = %int(§01fil);
045500130719         else;
045600130719           clear  CMMfil;
045700130719         endif;
045800130719         CMMuni   = §01rgf;
045900130719         //clear  CMMfil10;
046000130719         CMMdtIns = wDate;
046100130722         //if  TBLdtr > *zero;
046200130722         //  wDate_Amg = %date( TBLdtr : *ymd );
046300130722         //  wDate_Iso = wDate_Amg;
046400130722         //  CMMdtMod = %dec(wDate_Iso);
046500130722         //endif;
046600130722         CMMdtMod = wDate;
046700130722         CMMute   = 'EDPCED';
046800130719
046900130719         // -?Scrittura/Aggiornamento del singolo record?
047000130722         If  VIDclr = 'S'   or
047100130719            (VIDwrt = 'S'  and  Not %equal(AZCMM01L));
047200130719           //_______________
047300130719           write   AZCMM000;
047400130719           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
047500130719         Else;
047600130719           //_______________
047700130719           update  AZCMM000;
047800130719           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
047900130719         EndIf;
048000130719
048100130719       ENDSR;
048200130719
048300130719       //--------------------------------------------------------------
048400130719       //?Scrittura/Aggiornamento singolo record "01" in AZNTC00F.     ?
048500130719       //--------------------------------------------------------------
048600130719       BEGSR  sr_WrtUpd_AZNTC01;
048700130719
048800130719         // -?Verifica esistenza commerciale nel file AZNTC00F?
048900130719         If  VIDclr = *blank;
049000130719
049100130719           k_NTCcmm = CMMcod;
049200130719           k_NTCtnt = '01';
049300130719           if  VIDwrt = 'S';
049400130719             setll  %kds(k02azntc01)  AZNTC000;
049500130719           else;
049600130719             chain  %kds(k02azntc01)  AZNTC000;
049700130719           endif;
049800130719
049900130719         EndIf;
050000130719
050100130719         Select;
050200130719
050300130719           // -?Richiesta la pulizia iniziale dei file AZCMM/AZNTC:?
050400130719           //  ?tutti i dati saranno da scrivere?
050500130719           When  VIDclr = 'S'  and  §HTMnmc <> *blank;
050600130719
050700130719           // -?Richiesta solo la copia dei commerciali mancanti  e?
050800130719           //  ?commerciale già esistente in AZNTC00F "01":  => uscita?
050900130719           When  VIDwrt = 'S'  and  %equal(AZNTC01L);
051000130719             leavesr;
051100130719
051200130722           // -?Nominativo vuoto in tab. "HTM":  => uscita?
051300130722           When  §HTMnmc = *blank;
051400130719             leavesr;
051500130719
051600130719         EndSl;
051700130719
051800130719         // -?Pulizia del trk SE record da aggiungre?
051900130719         If  VIDclr = 'S'  or  VIDwrt = 'S';
052000130719           clear  AZNTC000;
052100130719           NTCcmm = CMMcod;
052200130719           NTCtnt = '01';
052300130719         EndIf;
052400130719
052500130719         // -?Valorizzazione dei vari campi nel record?
052600130719         clear  NTCatb;
052700130719         NTCrnt = §HTMnmc;
052800130719
052900130719         // -?Scrittura/Aggiornamento del singolo record?
053000130722         If  VIDclr = 'S'   or
053100130719            (VIDwrt = 'S'  and  Not %equal(AZNTC01L));
053200130719           //_______________
053300130719           write   AZNTC000;
053400130719           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
053500130719         Else;
053600130719           //_______________
053700130719           update  AZNTC000;
053800130719           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
053900130719         EndIf;
054000130719
054100130719       ENDSR;
054200130719
054300130719       //--------------------------------------------------------------
054400130719       //?Scrittura/Aggiornamento singolo record "02" in AZNTC00F.     ?
054500130719       //--------------------------------------------------------------
054600130719       BEGSR  sr_WrtUpd_AZNTC02;
054700130719
054800130719         // -?Verifica esistenza commerciale nel file AZNTC00F?
054900130719         If  VIDclr = *blank;
055000130719
055100130719           k_NTCcmm = CMMcod;
055200130719           k_NTCtnt = '02';
055300130719           if  VIDwrt = 'S';
055400130719             setll  %kds(k02azntc01)  AZNTC000;
055500130719           else;
055600130719             chain  %kds(k02azntc01)  AZNTC000;
055700130719           endif;
055800130719
055900130719         EndIf;
056000130719
056100130719         Select;
056200130719
056300130719           // -?Richiesta la pulizia iniziale dei file AZCMM/AZNTC:?
056400130719           //  ?tutti i dati saranno da scrivere?
056500130719           When  VIDclr = 'S'  and  §HTMind <> *blank;
056600130719
056700130719           // -?Richiesta solo la copia dei commerciali mancanti  e?
056800130722           //  ?commerciale già esistente in AZNTC00F "02":  => uscita?
056900130719           When  VIDwrt = 'S'  and  %equal(AZNTC01L);
057000130719             leavesr;
057100130719
057200130722           // -?Indirizzo e-mail vuoto in tab. "HTM":  => uscita?
057300130722           When  §HTMind = *blank;
057400130719             leavesr;
057500130719
057600130719         EndSl;
057700130719
057800130719         // -?Pulizia del trk SE record da aggiungre?
057900130719         If  VIDclr = 'S'  or  VIDwrt = 'S';
058000130719           clear  AZNTC000;
058100130719           NTCcmm = CMMcod;
058200130719           NTCtnt = '02';
058300130719         EndIf;
058400130719
058500130719         // -?Valorizzazione dei vari campi nel record?
058600130719         clear  NTCatb;
058700130719         NTCrnt = §HTMind;
058800130719
058900130719         // -?Scrittura/Aggiornamento del singolo record?
059000130722         If  VIDclr = 'S'  or
059100130719            (VIDwrt = 'S'  and  Not %equal(AZNTC01L));
059200130719           //_______________
059300130719           write   AZNTC000;
059400130719           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
059500130719         Else;
059600130719           //_______________
059700130719           update  AZNTC000;
059800130719           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
059900130719         EndIf;
060000130719
060100130719       ENDSR;
060200130719
060300130719       //--------------------------------------------------------------
060400130719       //?Operazioni finali.                                           ?
060500130719       //--------------------------------------------------------------
060600130719       BEGSR  sr_RoutEnd;
060700130719
060800130719         return;
060900130719
061000130719       ENDSR;
061100130719
061200130719       //--------------------------------------------------------------
061300130719       //?Esecuzione del comando (già impostato).                      ?
061400130719       //--------------------------------------------------------------
061500130719       BEGSR  sr_ExecCmd;
061600130719
061700130719         clear Qcap0100;
061800130719         Qcabcsdh = *off;
061900130719         Qcapa    = *off;
062000130719         Qcacmdss = *off;
062100130719         Qcaerved = *allX'00';
062200130719
062300130719         clear Qusec;
062400130719         Qusbprv  = %size(Qusec);
062500130719
062600130719         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
062700130719                           %size(Qcap0100) : 'CPOP0100' : *omit :
062800130719                           0 : 0 : Qusec);
062900130719
063000130719         //if  Qusei <> *blank;
063100130719         //  ...;
063200130719         //endif;
063300130719
063400130719       ENDSR;
063500130719
063600130719      /end-free
