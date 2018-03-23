000100160928       //==============================================================
000200160928       //?TNTAPAR * Interrogaz./Selez. indice Pubbliche Amministazioni ?
000300160928       //==============================================================
000400160928
000500160928       //--------------------------------------------------------------
000600160928       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160928       //--------------------------------------------------------------
000800160928
000900160928     /*PRM  dbgview(*source)
001000160928     /*END
001100160928
001200160928       //--------------------------------------------------------------
001300160928       //?Specifiche di controllo.                                     ?
001400160928       //--------------------------------------------------------------
001500160928
001600160928     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160928     h dftactgrp(*no)
001800160928     h alwnull(*inputonly)
001900160928
002000160928       //--------------------------------------------------------------
002100160928       //?Dichiarazione file.                                          ?
002200160928       //--------------------------------------------------------------
002300160928
002400160928       // -?Indice Pubbliche Amministrazioni?
002500160928     fAZIPA00F  if   e             disk    usropn
002600160928
002700160928       // -?Video Gestione?
002800160928     fTNTAPAD   cf   e             workstn
002900160928     f                                     indds( IndDspF )
003000160928     f                                     infds( InfDspF )
003100160928     f                                     sfile( TAPAS01 : S01nrr )
003200160928
003300160928       //--------------------------------------------------------------
003400160928       //?Definizione costanti.                                        ?
003500160928       //--------------------------------------------------------------
003600160928
003700160928       // -?Numero di record in ciascuna videata di subfile?
003800160928     d c_SflPag        c                   const(16)
003900160928
004000160928       // -?Numero massimo di record gestibili in un SubFile?
004100160928     d c_MaxRec        c                   const(9999)
004200160928
004300160928       // -?Costante per controllo "caratteri solo numerici"?
004400160928     d c_Digits        c                   const('0123456789')
004500160928
004600160928       // -?Tasti funzionali a video?
004700160928     d c_F01           c                   const(x'31')
004800160928     d c_F02           c                   const(x'32')
004900160928     d c_F03           c                   const(x'33')
005000160928     d c_F04           c                   const(x'34')
005100160928     d c_F05           c                   const(x'35')
005200160928     d c_F06           c                   const(x'36')
005300160928     d c_F07           c                   const(x'37')
005400160928     d c_F08           c                   const(x'38')
005500160928     d c_F09           c                   const(x'39')
005600160928     d c_F10           c                   const(x'3A')
005700160928     d c_F11           c                   const(x'3B')
005800160928     d c_F12           c                   const(x'3C')
005900160928     d c_F13           c                   const(x'B1')
006000160928     d c_F14           c                   const(x'B2')
006100160928     d c_F15           c                   const(x'B3')
006200160928     d c_F16           c                   const(x'B4')
006300160928     d c_F17           c                   const(x'B5')
006400160928     d c_F18           c                   const(x'B6')
006500160928     d c_F19           c                   const(x'B7')
006600160928     d c_F20           c                   const(x'B8')
006700160928     d c_F21           c                   const(x'B9')
006800160928     d c_F22           c                   const(x'BA')
006900160928     d c_F23           c                   const(x'BB')
007000160928     d c_F24           c                   const(x'BC')
007100160928     d c_Enter         c                   const(x'F1')
007200160928     d c_RollDown      c                   const(x'F4')
007300160928     d c_RollUp        c                   const(x'F5')
007400160928
007500160928       //--------------------------------------------------------------
007600160928       //?Definizione schiere.                                         ?
007700160928       //--------------------------------------------------------------
007800160928
007900160928       // -?Messaggi di errore?
008000160928     d sk_Msg          s             78    dim( 3)  ctdata  perrcd(1)
008100160928
008200160928       //--------------------------------------------------------------
008300160928       //?Definizione aree dati.                                       ?
008400160928       //--------------------------------------------------------------
008500160928
008600160928       // -?Dati utente?
008700160928     d §AzUte        e ds                  extname(AZUTE00F)
008800160928     d                                     dtaara
008900160928     d §DatiUte      e ds                  extname(dDatiUte)
009000160928     d                                     dtaara
009100160928
009200160928       //--------------------------------------------------------------
009300160928       //?Definizione strutture dati.                                  ?
009400160928       //--------------------------------------------------------------
009500160928
009600160928       // -?Status ds?
009700160928     d Status         sds
009800160928     d   SDSpgm          *proc
009900160928     d*//SDSprm          *parms
010000160928     d*//SDSdta              191    198
010100160928     d   SDSjobName          244    253
010200160928     d   SDSjobUser          254    263
010300160928     d   SDSjobNbr           264    269s 0
010400160928
010500160928       // -?InfDS?
010600160928     d InfDspF         ds
010700160928     d   dsp_aid             369    369a
010800160928
010900160928       // -?Indicatori su DspF?
011000160928     d IndDspF         ds                  inz
011100160928         // -?Abilitazione tasti funzionali?
011200160928     d   $F8Attivo                     n   overlay( IndDspF : 08 )
011300160928     d   $F12Attivo                    n   overlay( IndDspF : 12 )
011400160928         // ·?Emissione messaggio di errore?
011500160928     d   $ErrMessage                   n   overlay( IndDspF : 28 )
011600160928         // ·?Indicatori di gestione del subfile?
011700160928     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
011800160928     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
011900160928     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
012000160928     d   $SflEnd                       n   overlay( IndDspF : 33 )
012100160928         // ·?Indicatori per Attibuti di visualizzazione?
012200160928     d   $Ord_x_Des                    n   overlay( IndDspF : 41 )
012300160928     d   $Ord_x_CF                     n   overlay( IndDspF : 42 )
012400160928     d   $Ord_x_UODes                  n   overlay( IndDspF : 43 )
012500160928     d   $Ord_x_Uff                    n   overlay( IndDspF : 44 )
012600160928     d   $Ord_x_Loc                    n   overlay( IndDspF : 45 )
012700160928     d   $Ord_x_Prv                    n   overlay( IndDspF : 46 )
012800160928     d   $Ord_x_UOLoc                  n   overlay( IndDspF : 47 )
012900160928     d   $Ord_x_UOPrv                  n   overlay( IndDspF : 48 )
013000160928         // ·?Posizionamento cursore & Segnalazione errore?
013100160928     d   $PosCurOpz                    n   overlay( IndDspF : 50 )
013200160928         // ·?Riemissione videata?
013300160928     d   $ErrGenerico                  n   overlay( IndDspF : 90 )
013400160928
013500160928       // -?Parametri ricevuti?
013600160928     d KPJBA         e ds
013700160928     d TNTAPAds      e ds                  qualified
013800160928     d                                     inz
013900160928
014000160928       // -?Dati estratti via SQL?
014100160928     d AZIPAds       e ds                  extname( AZIPA00F )
014200160928     d                                     qualified
014300160928     d                                     inz
014400161003     d wNrrAZIPA       s             10i 0 inz
014500160928
014600160928       //--------------------------------------------------------------
014700160928       //?Definizione variabili globali.                               ?
014800160928       //--------------------------------------------------------------
014900160928
015000160928       // -?Flags booleani?
015100160928     d $Called         s               n   inz
015200160928     d $Fine           s               n   inz
015300160928     d $EoF            s               n   inz
015400160928     d $InzD01         s               n   inz(*on)
015500160928     d $InzD02         s               n   inz(*on)
015600160928     d $InzS01         s               n   inz(*on)
015700161003     d $First          s               n   inz(*on)
015800160928
015900160928       // -?Variabili per la gestione del video?
016000160928     d $Video          s              2    inz('D1')
016100160928
016200160928       // -?Variabili per la gestione dei subfile?
016300160928     d S01nrr          s              5i 0 inz
016400160928
016500160928       // -?Stringa SQL da eseguire?
016600160928     d wSql            s          32740    inz   varying
016700160928
016800160928       // -?Campi di comodo per la gestione del subfile?
016900160928     d SavC01pos       s             50    inz
017000160928
017100160928       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
017200160928
017300160928       //--------------------------------------------------------------
017400160928       // -?Constants?
017500160928       //--------------------------------------------------------------
017600160928       // -?MaxKey - Maximum number of key fields allowed by this program?
017700160928     d c_MaxKey        c                   const(2)
017800160928       // -?Sort order:?
017900160928       //  ?1 = Sort in an ascending sequence?
018000160928       //  ?2 = Sort in a descending sequence?
018100160928     d c_Ascendente    c                   const(1)
018200160928     d c_Discendente   c                   const(2)
018300160928       // -?Key data type:?
018400160928       //  ? 0 = Signed binary?
018500160928       //  ? 2 = Signed zoned decimal?
018600160928       //  ? 3 = Signed packed decimal?
018700160928       //  ? 6 = Character with no national language sort sequence applied,?
018800160928       //  ?     if specified?
018900160928       //  ? 7 = Unsigned packed decimal?
019000160928       //  ?     All numbers will have the sign forced positive ('F'X).?
019100160928       //  ? 8 = Unsigned zoned decimal?
019200160928       //  ?     All numbers will have the sign forced positive ('F'X).?
019300160928       //  ? 9 = Unsigned binary?
019400160928       //  ?14 = Date in form of DD/MM/YY?
019500160928       //  ?15 = Date in form of DD.MM.YYYY?
019600160928     d c_Numero        c                   const(2)
019700160928     d c_Carattere     c                   const(6)
019800160928     d c_NumIntero     c                   const(8)
019900160928       //
020000160928     d c_Put           c                   const(1)
020100160928     d c_EndPut        c                   const(2)
020200160928     d c_Get           c                   const(3)
020300160928
020400160928       //--------------------------------------------------------------
020500160928       // -?Data Structures?
020600160928       //  ?SflRcd     - The entire subfile record to pass to the sort?
020700160928       //  ?QLGSCB     - The sort request block for the QLGSORT API?
020800160928       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
020900160928       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
021000160928       //  ?QUSEC      - Error structure for the QLGSORT API?
021100160928       //--------------------------------------------------------------
021200160928     d SflRcd          ds                  inz
021300160928     d   SH1des                            inz
021400160928     d   SH1loc                            inz
021500160928     d   SH1uoDes                          inz
021600160928     d   SH1uoLoc                          inz
021700160928     d   S01opz                            inz
021800160928     d   S01des                            inz
021900160928     d   S01cf                             inz
022000160928     d   S01uoDes                          inz
022100160928     d   S01uff                            inz
022200160928     d   S01loc                            inz
022300160928     d   S01prv                            inz
022400160928     d   S01uoLoc                          inz
022500160928     d   S01uoPrv                          inz
022600160928     d   SH1rrn                            inz
022700160928     d   Selected                     1a   inz
022800160928      /copy QSYSINC/QRPGLESRC,QLGSORT
022900160928     d QLGKL                         16    dim(c_MaxKey)
023000160928     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
023100160928     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
023200160928     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
023300160928     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
023400160928      /copy QSYSINC/QRPGLESRC,QLGSRTIO
023500160928      /copy QSYSINC/QRPGLESRC,QUSEC
023600160928
023700160928       //--------------------------------------------------------------
023800160928       // -?Standalone fields?
023900160928       //  ?Nrr        - Relative record number for subfile?
024000160928       //  ?SizeList   - Size of list?
024100160928       //  ?ReturnSize - Size of list returned by sort API?
024200160928       //--------------------------------------------------------------
024300160928     d NotUsed         s             16a   inz
024400170215     d ReturnSize      s             10i 0 inz
024500170215     d SizeList        s             10i 0 inz
024600160928     d RrnLast         s              5i 0 inz
024700160928     d Nrr             s              5i 0 inz
024800160928
024900160928       //--------------------------------------------------------------
025000160928       //?Definizione prototipi procedure usate.                       ?
025100160928       //--------------------------------------------------------------
025200160928
025300160928       // -?Reperimento dati utente?
025400160928     d TIBS34ds      e ds                  inz
025500160928      /copy gaitrasrc/srcProtoPR,TIBS34R
025600160928
025700160928       // -?Ordinamento subfile?
025800160928      /copy gaitrasrc/srcProtoPR,QLGSRTIO
025900160928
026000160928       //--------------------------------------------------------------
026100160928       //?Definizione key-list.                                        ?
026200160928       //--------------------------------------------------------------
026300160928
026400160928
026500160928       //--------------------------------------------------------------
026600160928       //?Riepilogo indicatori utilizzati.                             ?
026700160928       //--------------------------------------------------------------
026800160928
026900160928
027000160928       //--------------------------------------------------------------
027100160928       //?M A I N - L I N E                                            ?
027200160928       //--------------------------------------------------------------
027300160928
027400160928     c     *Entry        plist
027500160928     c                   parm                    KPJBA
027600160928
027700160928      /free
027800160928
027900160928       // -?Operazioni iniziali?
028000160928       exsr  sr_RoutInz;
028100160928
028200160928       // -?Gestione videata?
028300160928       DoW  Not $Fine;
028400160928
028500160928         select;
028600160928
028700160928           // -?Filtro di lancio?
028800160928           when  $Video = 'D1';
028900160928             exsr  sr_GesD01;
029000160928
029100160928           // -?SubFile?
029200160928           when  $Video = 'S1';
029300160928             exsr  sr_GesS01;
029400160928
029500160928           // -?Dettaglio?
029600160928           when  $Video = 'D2';
029700160928             exsr  sr_GesD02;
029800160928
029900160928           // -?? ? ??
030000160928           other;
030100160928             $Fine = *on;
030200160928
030300160928         endsl;
030400160928
030500160928       EndDo;
030600160928
030700160928       // -?Operazioni finali?
030800160928       exsr  sr_RoutEnd;
030900160928
031000160928       //--------------------------------------------------------------
031100160928       //?Operazioni iniziali.                                         ?
031200160928       //--------------------------------------------------------------
031300160928       BEGSR  sr_RoutInz;
031400160928
031500160928         // -?Impostazione opzioni per SQL?
031600160928         exec sql   set  option  DynUsrPrf = *Owner,
031700160928                                 CloSqlCsr = *EndMod;
031800160928
031900160928         // -?Verifica SE ricevuti parametri?
032000161003         reset  $InzD01;
032100161003         reset  $InzD02;
032200161003         reset  $InzS01;
032300160928         reset  $Called;
032400160928         if  KPJBU <> *blank;
032500160928           $Called  = *on;
032600160928           TNTAPAds = kpjbu;
032700160928           exsr  sr_InzD01;
032800160928           exsr  sr_CtrD01;
032900160928           if  Not $ErrGenerico;
033000160928             $Video   = 'S1';
033100160928             $InzS01  = *on;
033200160928           endif;
033300161003         else;
033400161003           tntaPAds.iPAtla = 'L';
033500160928         endif;
033600160928
033700160928         // -?Impostazione chiusura?
033800161003         reset  $Fine;
033900161003         reset  $EoF;
034000160928         select;
034100160928           when  tntaPAds.iPAtla = *blank;
034200160928             *inRT = *on;
034300160928           when  tntaPAds.iPAtla = 'L';
034400160928             *inLR = *on;
034500160928           when  tntaPAds.iPAtla = 'C';
034600160928             *inLR = *on;
034700160928             $Fine = *on;
034800160928             leavesr;
034900160928         endsl;
035000160928
035100160928         // -?Reperimento dati job?
035200160928         exsr  sr_DatiJob;
035300160928
035400160928         // -?Impostazione nome programma a video?
035500160928         V1Tpgm = SDSpgm;
035600160928
035700160928       ENDSR;
035800160928
035900160928       //--------------------------------------------------------------
036000160928       //?Reperimento Dati del job (Utente/Operativi).                 ?
036100160928       //--------------------------------------------------------------
036200160928       BEGSR  sr_DatiJob;
036300160928
036400160928         in(e) §AzUte;
036500160928         if NOT %error;
036600160928           in(e) §DatiUte;
036700160928         endif;
036800160928         if %error or RSut = *blank;
036900160928           tibs34r ( tibs34ds );
037000160928           in §AzUte;
037100160928           in §DatiUte;
037200160928         endif;
037300160928
037400160928       ENDSR;
037500160928
037600160928       //--------------------------------------------------------------
037700160928       //?Gestione videata D01.                                        ?
037800160928       //--------------------------------------------------------------
037900160928       BEGSR  sr_GesD01;
038000160928
038100160928         // -?Inizializzazione videata?
038200160928         if  $InzD01 = *on;
038300160928           exsr  sr_InzD01;
038400160928           $InzD01 = *off;
038500160928         endif;
038600160928
038700160928         // -?(Dis)Attivazione tasti funzionali a video?
038800160928         $F8Attivo   = *off;
038900160928         $F12Attivo  = *off;
039000160928
039100160928         // -?Emissione Testata e Piede con tasti funzionali abilitati?
039200160928         write  TAPAT01;
039300160928         write  TAPAP01;
039400160928
039500160928         // -?Emissione videata?
039600160928         exfmt  TAPAD01;
039700160928
039800160928         clear  VIDmsg;
039900160928         reset  $ErrMessage;
040000160928         reset  $ErrGenerico;
040100160928
040200160928         Select;
040300160928
040400160928           // -?F3=Fine?
040500160928           When  dsp_aid = c_F03;
040600160928             $Fine  = *on;
040700160928
040800160928           // -?Enter?
040900160928           Other;
041000160928             exsr  sr_CtrD01;
041100160928             if  $ErrGenerico;
041200160928               leavesr;
041300160928             endif;
041400160928             $InzS01 = *on;
041500160928             $Video  = 'S1';
041600160928
041700160928         EndSl;
041800160928
041900160928       ENDSR;
042000160928
042100160928       //--------------------------------------------------------------
042200160928       //?Inizializzazione videata D01.                                ?
042300160928       //--------------------------------------------------------------
042400160928       BEGSR  sr_InzD01;
042500160928
042600160928         // -?Spegnimento indicatori di errore e posizionamento cursore?
042700160928         %subst( IndDspF : 28 ) = *off;
042800160928
042900160928         // -?Pulizia videata?
043000160928         clear  TAPAD01;
043100160928
043200160928         // -?Impostazione iniziale dei campi a video?
043300160928         //  ?(SE ricevuti parametri)?
043400160928         if  $Called;
043500160928           D01cf    = tntaPAds.IPAcf;
043600160928           D01uoCap = tntaPAds.IPAuoCap;
043700160928           D01uoLoc = tntaPAds.IPAuoLoc;
043800160928           D01uoPrv = tntaPAds.IPAuoPrv;
043900160928         endif;
044000160928
044100160928       ENDSR;
044200160928
044300160928       //--------------------------------------------------------------
044400160928       //?Inizializzazione videata D01.                                ?
044500160928       //--------------------------------------------------------------
044600160928       BEGSR  sr_CtrD01;
044700160928
044800160928         // -?Spegnimento indicatori di errore e posizionamento cursore?
044900160928         %subst(IndDspF : 28) = *off;
045000160928
045100160928       ENDSR;
045200160928
045300160928       //--------------------------------------------------------------
045400160928       //?Gestione subfile S01.                                        ?
045500160928       //--------------------------------------------------------------
045600160928       BEGSR  sr_GesS01;
045700160928
045800160928         // -?Inizializzazione subfile S01?
045900160928         if  $InzS01 = *on;
046000160928           exsr  sr_InzS01;
046100160928           $InzS01 = *off;
046200160928           // -?Verifica raggiungimento del limite di records?
046300160928           //  ?registrabili nel subfile (9999)?
046400161003           if  S01nrr >= c_MaxRec;
046500160928             $ErrGenerico = *on;
046600160928             $ErrMessage  = *on;
046700160928             VIDmsg = sk_Msg(01);
046800160928           endif;
046900160928         endif;
047000160928
047100160928         // -?(Dis)Attivazione tasti funzionali a video?
047200160928         $F8Attivo   = ( S01nrr > 1 );
047300160928         $F12Attivo  = *on;
047400160928
047500160928         // -?Posizionamento cursore?
047600160928         //  ?C1CsrRrn contiene il numero di riga del SubFile su cui?
047700160928         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
047800160928         //  ?si visualizza la pagina che era visualizzata quando?
047900160928         //  ?l'utente ha premuto l'ultimo tasto.?
048000160928         if  C1CsrRrn > *zero;
048100160928           C1RcdNbr = C1CsrRrn;
048200160928         else;
048300160928           C1RcdNbr = 1;
048400160928           write  TAPAS00;          //(no rec.)?
048500160928         endif;
048600160928
048700160928         // -?Emissione Testata e Piede con tasti funzionali abilitati?
048800160928         write  TAPAT01;
048900160928         write  TAPAP01;
049000160928
049100160928         // -?Emissione videata?
049200160928         exfmt  TAPAC01;
049300160928
049400160928         clear  VIDmsg;
049500160928         reset  $ErrMessage;
049600160928         reset  $ErrGenerico;
049700160928
049800160928         Select;
049900160928
050000160928           // -?F3=Fine?
050100160928           When  dsp_aid = c_F03;
050200160928             exsr  sr_CloseCursor;
050300160928             $Fine = *on;
050400160928             if  $Called;
050500160928               tntaPAds.oPAfxx = '1';
050600160928             endif;
050700160928
050800160928           // -?F8=Ordinamento?
050900160928           When  dsp_aid = c_F08;
051000160928             exsr  sr_F08S01;
051100160928
051200160928           // -?F12=Ritorno?
051300160928           When  dsp_aid = c_F12;
051400160928             exsr  sr_CloseCursor;
051500160928             if  $Called;
051600160928               $Fine  = *on;
051700160928               tntaPAds.oPAfxx = '2';
051800160928             else;
051900160928               $Video = 'D1';
052000160928             endif;
052100160928
052200160928           // -?SubFile vuoto?
052300160928           When  S01nrr = *zero;
052400160928
052500160928           // -?Invio?
052600160928           Other;
052700160928             // -?Gestione opzioni?
052800160928             exsr  sr_OpzS01;
052900160928             if  $ErrGenerico;
053000160928               leavesr;
053100160928             endif;
053200160928
053300160928         EndSl;
053400160928
053500160928       ENDSR;
053600160928
053700160928       //--------------------------------------------------------------
053800160928       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
053900160928       //?F08 = Cambia ordinamento Sfl                                 ?
054000160928       //--------------------------------------------------------------
054100160928       BEGSR  sr_F08S01;
054200160928
054300160928         Select;
054400160928           When  Not $F8Attivo;
054500160928             clear  $Ord_x_Des;
054600160928             clear  $Ord_x_CF;
054700160928             clear  $Ord_x_UODes;
054800160928             clear  $Ord_x_Uff;
054900160928             clear  $Ord_x_Loc;
055000160928             clear  $Ord_x_Prv;
055100160928             clear  $Ord_x_UOLoc;
055200160928             clear  $Ord_x_UOPrv;
055300160928           When  $Ord_x_Des;
055400160928             $Ord_x_Des   = *off;
055500160928             $Ord_x_CF    = *on;
055600160928             $Ord_x_UODes = *off;
055700160928             $Ord_x_Uff   = *off;
055800160928             $Ord_x_Loc   = *off;
055900160928             $Ord_x_Prv   = *off;
056000160928             $Ord_x_UOLoc = *off;
056100160928             $Ord_x_UOPrv = *off;
056200160928           When  $Ord_x_CF;
056300160928             $Ord_x_Des   = *off;
056400160928             $Ord_x_CF    = *off;
056500160928             $Ord_x_UODes = *on;
056600160928             $Ord_x_Uff   = *off;
056700160928             $Ord_x_Loc   = *off;
056800160928             $Ord_x_Prv   = *off;
056900160928             $Ord_x_UOLoc = *off;
057000160928             $Ord_x_UOPrv = *off;
057100160928           When  $Ord_x_UODes;
057200160928             $Ord_x_Des   = *off;
057300160928             $Ord_x_CF    = *off;
057400160928             $Ord_x_UODes = *off;
057500160928             $Ord_x_Uff   = *on;
057600160928             $Ord_x_Loc   = *off;
057700160928             $Ord_x_Prv   = *off;
057800160928             $Ord_x_UOLoc = *off;
057900160928             $Ord_x_UOPrv = *off;
058000160928           When  $Ord_x_Uff;
058100160928             $Ord_x_Des   = *off;
058200160928             $Ord_x_CF    = *off;
058300160928             $Ord_x_UODes = *off;
058400160928             $Ord_x_Uff   = *off;
058500160928             $Ord_x_Loc   = *on;
058600160928             $Ord_x_Prv   = *off;
058700160928             $Ord_x_UOLoc = *off;
058800160928             $Ord_x_UOPrv = *off;
058900160928           When  $Ord_x_Loc;
059000160928             $Ord_x_Des   = *off;
059100160928             $Ord_x_CF    = *off;
059200160928             $Ord_x_UODes = *off;
059300160928             $Ord_x_Uff   = *off;
059400160928             $Ord_x_Loc   = *off;
059500160928             $Ord_x_Prv   = *on;
059600160928             $Ord_x_UOLoc = *off;
059700160928             $Ord_x_UOPrv = *off;
059800160928           When  $Ord_x_Prv;
059900160928             $Ord_x_Des   = *off;
060000160928             $Ord_x_CF    = *off;
060100160928             $Ord_x_UODes = *off;
060200160928             $Ord_x_Uff   = *off;
060300160928             $Ord_x_Loc   = *off;
060400160928             $Ord_x_Prv   = *off;
060500160928             $Ord_x_UOLoc = *on;
060600160928             $Ord_x_UOPrv = *off;
060700160928           When  $Ord_x_UOLoc;
060800160928             $Ord_x_Des   = *off;
060900160928             $Ord_x_CF    = *off;
061000160928             $Ord_x_UODes = *off;
061100160928             $Ord_x_Uff   = *off;
061200160928             $Ord_x_Loc   = *off;
061300160928             $Ord_x_Prv   = *off;
061400160928             $Ord_x_UOLoc = *off;
061500160928             $Ord_x_UOPrv = *on;
061600160928           When  $Ord_x_UOPrv;
061700160928             $Ord_x_Des   = *on;
061800160928             $Ord_x_CF    = *off;
061900160928             $Ord_x_UODes = *off;
062000160928             $Ord_x_Uff   = *off;
062100160928             $Ord_x_Loc   = *off;
062200160928             $Ord_x_Prv   = *off;
062300160928             $Ord_x_UOLoc = *off;
062400160928             $Ord_x_UOPrv = *off;
062500160928         EndSl;
062600160928
062700160928         select;
062800160928           // -?Subfile vuoto: nessun record da ordinare?
062900160928           when  S01nrr <= 1;
063000160928           // -?Subfile già parzializzato: occorre ricaricarlo?
063100160928           when  ($Ord_x_Des              and
063200160928                  SavC01pos   <> C01des)   or
063300160928                 ($Ord_x_Cf               and
063400160928                  SavC01pos   <> C01cf)    or
063500160928                 ($Ord_x_UODes            and
063600160928                  SavC01pos   <> C01uoDes) or
063700160928                 ($Ord_x_Uff              and
063800160928                  SavC01pos   <> C01uff)   or
063900160928                 ($Ord_x_Loc              and
064000160928                  SavC01pos   <> C01loc)   or
064100160928                 ($Ord_x_Prv              and
064200160928                  SavC01pos   <> C01prv)   or
064300160928                 ($Ord_x_UOLoc            and
064400160928                  SavC01pos   <> C01uoLoc) or
064500160928                 ($Ord_x_UOPrv            and
064600160928                  SavC01pos   <> C01uoPrv);
064700160928             $InzS01 = *on;
064800160928           // -?Altrimenti: basta riordinarlo?
064900160928           other;
065000160928             exsr  sr_SortSfl;
065100160928         endsl;
065200160928
065300160928       ENDSR;
065400160928
065500160928       //--------------------------------------------------------------
065600160928       //?Inizializzazione subfile S01.                                ?
065700160928       //--------------------------------------------------------------
065800160928       BEGSR  sr_InzS01;
065900160928
066000160928         // -?Pulizia subfile?
066100160928         $SflDsp_N    = *on;
066200160928         $SflDspCtl_N = *on;
066300160928         write  TAPAC01;
066400160928         $SflDspCtl_N = *off;
066500160928         $SflEnd      = *off;
066600160928
066700160928         $SflNxtChg   = *off;
066800160928
066900160928         clear  C1RcdNbr;
067000160928         clear  C1CsrRrn;
067100160928         clear  S01nrr;
067200160928         clear  VIDmsg;
067300160928         $ErrMessage  = *off;
067400160928         $ErrGenerico = *off;
067500160928
067600160928         // -?Spegnimento degli indicatori di errore?
067700160928         %subst( IndDspF : 50 ) = *off;
067800160928
067900160928         // -?Descrizione Opzioni?
068000160928         if  $Called;
068100160928           C01opzD = '1=Selezione, 5=Visualizzazione.';
068200160928         else;
068300160928           C01opzD = '5=Visualizzazione.';
068400160928         endif;
068500161003
068600161003         if  Not $Ord_x_Des    and
068700161003             Not $Ord_x_CF     and
068800161003             Not $Ord_x_UODes  and
068900161003             Not $Ord_x_Uff    and
069000161003             Not $Ord_x_Loc    and
069100161003             Not $Ord_x_Prv    and
069200161003             Not $Ord_x_UOLoc  and
069300161003             Not $Ord_x_UOPrv;
069400161003           $Ord_x_Des = *on;
069500161003         endif;
069600160928
069700160928         select;
069800160928           when  $Ord_x_Des;
069900160928             SavC01pos   = C01des;
070000160928           when  $Ord_x_CF;
070100160928             SavC01pos   = C01cf;
070200160928           when  $Ord_x_UODes;
070300160928             SavC01pos   = C01uoDes;
070400160928           when  $Ord_x_Uff;
070500160928             SavC01pos   = C01uff;
070600160928           when  $Ord_x_Loc;
070700160928             SavC01pos   = C01loc;
070800160928           when  $Ord_x_Prv;
070900160928             SavC01pos   = C01prv;
071000160928           when  $Ord_x_UOLoc;
071100160928             SavC01pos   = C01uoLoc;
071200160928           when  $Ord_x_UOPrv;
071300160928             SavC01pos   = C01uoPrv;
071400160928         endsl;
071500160928
071600160928         // -?Caricamento completo dei dati nel subfile?
071700160928         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
071800160928         //  ?l'eventuale ordinamento)?
071900170203         reset  $First;
072000170203         reset  $EoF;
072100160928         // -?Preparazione stringa SQL?
072200160928         exsr  sr_PrepSQL;
072300160928         // -?Apertura del cursore?
072400160928         exsr  sr_OpenCursor;
072500160928         // -?Lettura del file?
072600160928         exsr  sr_ReadCursor;
072700161003         DoW  $EoF   = *off    and
072800161003              S01nrr < c_MaxRec;
072900160928           exsr  sr_CaricaS01;
073000160928         EndDo;
073100160928
073200160928         // -?Visualizzazione del SFL (se ci sono dati)?
073300160928         $SflDsp_N = ( S01nrr <= *zero );
073400160928
073500160928         // -?Attivazione del SFLEND?
073600160928         $SflEnd   = ( $EoF = *on );
073700160928
073800160928         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
073900160928         RrnLast  = S01nrr;
074000160928
074100160928         // -?Impaginazione subfile?
074200160928         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
074300160928         if  S01nrr > *zero;
074400160928           C1RcdNbr = 1;
074500160928           C1CsrRrn = 1;
074600160928         else;
074700160928           clear  C1RcdNbr;
074800160928         endif;
074900160928
075000160928         // -?(Dis)Abilitazione tasti funzionali?
075100160928         // ·?F8 = Ordinamento x ...?
075200160928         $F8Attivo  = ( S01nrr > 1 );
075300160928
075400160928         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
075500160928         Select;
075600160928           When  Not $F8Attivo;
075700160928             clear  P01f08;
075800160928           When  $Ord_x_Des;
075900160928             P01f08 = 'F8=Ord. x C.F.   ';
076000160928           When  $Ord_x_CF;
076100160928             P01f08 = 'F8=Ord. x UnitàOr';
076200160928           When  $Ord_x_UODes;
076300160928             P01f08 = 'F8=Ord. x Ufficio';
076400160928           When  $Ord_x_Uff;
076500160928             P01f08 = 'F8=Ord. x Local. ';
076600160928           When  $Ord_x_Loc;
076700160928             P01f08 = 'F8=Ord. x Prov.  ';
076800160928           When  $Ord_x_Prv;
076900160928             P01f08 = 'F8=Ord. x Loc. UO';
077000160928           When  $Ord_x_UOLoc;
077100160928             P01f08 = 'F8=Ord. x Prov.UO';
077200160928           When  $Ord_x_UOPrv;
077300160928             P01f08 = 'F8=Ord. x Descr. ';
077400160928         EndSl;
077500160928
077600160928       ENDSR;
077700160928
077800160928       //--------------------------------------------------------------
077900160928       //?Caricamento singola pagine SubFile S01.                      ?
078000160928       //--------------------------------------------------------------
078100160928       BEGSR  sr_CaricaS01;
078200160928
078300160928         $SflNxtChg = *off;
078400160928
078500160928         // -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
078600160928         //  ?(occorre caricare il SFL al COMPLETO per riuscire a gestirne?
078700160928         //  ?l'eventuale ordinamento)?
078800160928         DoW  $EoF   = *off    and
078900161003              S01nrr < c_MaxRec;
079000160928
079100160928           // -?Caricamento dati nel record del subfile?
079200160928           exsr  sr_RieRecS01;
079300160928           S01nrr += 1;
079400160928           write  TAPAS01;
079500160928
079600160928           // -?Lettura record successivo nell'archivio?
079700160928           exsr  sr_ReadCursor;
079800160928
079900160928         EndDo;
080000160928
080100160928         // -?Memorizzazione posizionamento effettuato?
080200160928         select;
080300160928           when  $Ord_x_Des;
080400160928             SavC01pos = C01des;
080500160928           when  $Ord_x_CF;
080600160928             SavC01pos = C01cf;
080700160928           when  $Ord_x_UOdes;
080800160928             SavC01pos = C01uoDes;
080900160928           when  $Ord_x_Uff;
081000160928             SavC01pos = C01uff;
081100160928           when  $Ord_x_Loc;
081200160928             SavC01pos = C01loc;
081300160928           when  $Ord_x_Prv;
081400160928             SavC01pos = C01prv;
081500160928           when  $Ord_x_UOloc;
081600160928             SavC01pos = C01uoLoc;
081700160928           when  $Ord_x_UOprv;
081800160928             SavC01pos = C01uoPrv;
081900160928         endsl;
082000160928
082100160928         // -?Visualizzazione del SFL (se ci sono dati)?
082200160928         $SflDsp_N = ( S01nrr <= *zero );
082300160928
082400160928         // -?Attivazione (eventuale) del SFLEND?
082500160928         $SflEnd   = ( $EoF = *on );
082600160928
082700160928         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
082800160928         if  S01nrr > *zero;
082900160928           C1RcdNbr = 1;
083000160928         else;
083100160928           clear  C1RcdNbr;
083200160928         endif;
083300160928
083400160928         C1CsrRrn = C1RcdNbr;
083500160928
083600160928       ENDSR;
083700160928
083800160928       //--------------------------------------------------------------
083900160928       //?Caricamento singolo record nel SubFile S01.                  ?
084000160928       //--------------------------------------------------------------
084100160928       BEGSR  sr_RieRecS01;
084200160928
084300160928         // -?La pulizia spegnerebbe l'indicatore dell'ordinamento?
084400160928         //*·clear  TAPAS01;
084500160928
084600160928         // -?Campi Hidden?
084700160928         // ·?RRN?
084800160928         SH1rrn   = wNrrAZIPA;
084900160928         // ·?Denominazione Ente?
085000160928         SH1des   = AZIPAds.IPAdes;
085100160928         // ·?Comune sede legale Ente?
085200160928         SH1loc   = AZIPAds.IPAloc;
085300160928         // ·?Denominazione Unità Organizzativa?
085400160928         SH1uoDes = AZIPAds.IPAuoDes;
085500160928         // ·?Comune Unità Organizzativa?
085600160928         SH1uoLoc = AZIPAds.IPAuoLoc;
085700160928
085800160928         // -?Campi Output?
085900160928         // ·?Denominazione Ente?
086000160928         S01des   = AZIPAds.IPAdes;
086100160928         // ·?Codice Fiscale?
086200160928         S01cf    = AZIPAds.IPAcf;
086300160928         // ·?Denominazione Unità Organizzativa?
086400160928         S01uoDes = AZIPAds.IPAuoDes;
086500160928         // ·?Codice univoco Ufficio?
086600160928         S01uff   = AZIPAds.IPAuff;
086700160928         // ·?Comune sede legale Ente?
086800160928         S01loc   = AZIPAds.IPAloc;
086900160928         // ·?Provincia sede legale Ente?
087000160928         S01prv   = AZIPAds.IPAprv;
087100160928         // ·?Comune sede Unità Organizzativa?
087200160928         S01uoLoc = AZIPAds.IPAuoLoc;
087300160928         // ·?Provincia sede Unità Organizzativa?
087400160928         S01uoPrv = AZIPAds.IPAuoPrv;
087500160928
087600160928         // -?Attributi di Visualizzazione?
087700160928         // ·?Campo di Ordinamento?
087800160928         if  Not $Ord_x_Des    and
087900160928             Not $Ord_x_CF     and
088000160928             Not $Ord_x_UOdes  and
088100160928             Not $Ord_x_Uff    and
088200160928             Not $Ord_x_Loc    and
088300160928             Not $Ord_x_Prv    and
088400160928             Not $Ord_x_UOloc  and
088500160928             Not $Ord_x_UOprv;
088600160928           $Ord_x_Des = *on;
088700160928         endif;
088800160928
088900160928       ENDSR;
089000160928
089100160928       //--------------------------------------------------------------
089200160928       //?Gestione opzioni nel SubFile S01.                            ?
089300160928       //--------------------------------------------------------------
089400160928       BEGSR  sr_OpzS01;
089500160928
089600160928         // -?Ciclo di lettura subfile?
089700160928         readc  TAPAS01;
089800160928
089900160928         DOW  NOT  %eof(TNTAPAD);
090000160928
090100160928           %subst( IndDspF : 50 ) = *off;
090200160928           $SflNxtChg  = *off;
090300160928           C1RcdNbr    = S01nrr;
090400160928
090500160928           Select;
090600160928
090700160928             // -?Nessuna opzione?
090800160928             When  S01opz = *blank;
090900160928
091000160928             // -?5 = Visualizzazione BarCode per Profilo Spunta?
091100160928             When  S01opz = '5';
091200160928               $Video  = 'D2';
091300160928               $InzD02 = *on;
091400160928               DoW  $Video = 'D2';
091500160928                 exsr  sr_GesD02;
091600160928               EndDo;
091700160928               if  dsp_aid = c_F03;
091800160928                 $ErrGenerico = *on;
091900160928               endif;
092000160928               $Video = 'S1';
092100160928
092200160928             // -?? = Opzione NON valida?
092300160928             Other;
092400160928               $ErrGenerico = *on;
092500160928               $ErrMessage  = *on;
092600160928               $PosCurOpz   = *on;
092700160928               VIDmsg = sk_Msg(02);
092800160928
092900160928           EndSl;
093000160928
093100160928           // -?Aggiornamento subfile?
093200160928           Select;
093300160928             When  $ErrMessage;
093400160928               $SflNxtChg = *on;
093500160928               C1CsrRrn   = C1RcdNbr;
093600160928             When  $ErrGenerico;
093700160928               $SflNxtChg = *on;
093800160928               C1CsrRrn   = C1RcdNbr;
093900160928               clear  S01opz;
094000160928             Other;
094100160928               C1CsrRrn   = C1RcdNbr;
094200160928               clear  S01opz;
094300160928           EndSl;
094400160928
094500160928           update  TAPAS01;
094600160928
094700160928           // -?Uscita dal ciclo di lettura?
094800160928           if  $ErrMessage  or  $ErrGenerico;
094900160928             clear  $InzS01;
095000160928             leave;
095100160928           endif;
095200160928
095300160928           // -?Lettura rec. variato sucessivo?
095400160928           readc  TAPAS01;
095500160928
095600160928         EndDo;
095700160928
095800160928         // -?Se richiesto il RI-caricamento del subfile:?
095900160928         //  ?occorre chiudere il cursore SQL?
096000160928         if  $InzS01  and  Not $errGenerico;
096100160928           exsr  sr_CloseCursor;
096200160928         endif;
096300160928
096400160928       ENDSR;
096500160928
096600160928       //--------------------------------------------------------------
096700160928       //?Gestione videata D02.                                        ?
096800160928       //--------------------------------------------------------------
096900160928       BEGSR  sr_GesD02;
097000160928
097100160928         // -?Inizializzazione videata?
097200160928         if  $InzD02 = *on;
097300160928           exsr  sr_InzD02;
097400160928           $InzD02 = *off;
097500160928         endif;
097600160928
097700160928         // -?(Dis)Attivazione tasti funzionali a video?
097800160928         $F8Attivo   = *off;
097900160928         $F12Attivo  = *on;
098000160928
098100160928         // -?Emissione Testata e Piede con tasti funzionali abilitati?
098200160928         write  TAPAT01;
098300160928         write  TAPAP01;
098400160928
098500160928         // -?Emissione videata?
098600160928         exfmt  TAPAD02;
098700160928
098800160928         clear  VIDmsg;
098900160928         reset  $ErrMessage;
099000160928         reset  $ErrGenerico;
099100160928
099200160928         Select;
099300160928
099400160928           // -?F3=Fine?
099500160928           When  dsp_aid = c_F03;
099600160928             $Video = 'S1';
099700160928
099800160928           // -?F12=Ritorno?
099900160928           When  dsp_aid = c_F12;
100000160928             $Video = 'S1';
100100160928
100200160928         EndSl;
100300160928
100400160928       ENDSR;
100500160928
100600160928       //--------------------------------------------------------------
100700160928       //?Inizializzazione videata D02.                                ?
100800160928       //--------------------------------------------------------------
100900160928       BEGSR  sr_InzD02;
101000160928
101100160928         // -?Spegnimento indicatori di errore e posizionamento cursore?
101200160928         %subst( IndDspF : 50 ) = *off;
101300160928
101400160928         // -?Pulizia videata?
101500160928         clear  TAPAD02;
101600160928
101700160928         // -?Reperimento dati?
101800160928         if  not %open(AZIPA00F);
101900160928           open  AZIPA00F;
102000160928         endif;
102100160928         chain  SH1rrn  AZIPA000;
102200160928         if  Not %found(AZIPA00F);
102300160928           leavesr;
102400160928         endif;
102500160928
102600160928         // -?Impostazione iniziale dei campi a video?
102700160928         //  ?(SE ricevuti parametri)?
102800160928         D02cod   = IPAcod;
102900160928         D02des   = IPAdes;
103000160928         D02ind   = IPAind;
103100160928         D02cap   = IPAcap;
103200160928         D02loc   = IPAloc;
103300160928         D02prv   = IPAprv;
103400160928         D02cf    = IPAcf;
103500160928         D02vcf   = IPAvcf;
103600160928         D02uff   = IPAuff;
103700160928         D02uoDes = IPAuoDes;
103800160928         D02uoInd = IPAuoInd;
103900160928         D02uoCap = IPAuoCap;
104000160928         D02uoLoc = IPAuoLoc;
104100160928         D02uoPrv = IPAuoPrv;
104200160928         D02uoEml = IPAuoEml;
104300160928
104400160928       ENDSR;
104500160928
104600160928       //--------------------------------------------------------------
104700160928       //?Ordinamento subfile                                          ?
104800160928       //?  This subroutine sorts the subfile records.                 ?
104900160928       //--------------------------------------------------------------
105000160928       BEGSR  sr_SortSfl;
105100160928
105200160928         // -?Impostazione NRR dell'ultimo record del subfile          ?
105300160928         //  ?(già fatto in sr_InzS01:                                 ?
105400160928         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
105500160928         //*·RrnLast  = S01nrr;
105600160928
105700160928         C1RcdNbr = 1;
105800160928         clear  C1CsrRrn;
105900160928         clear  S01nrr;
106000160928         clear  VIDmsg;
106100160928         $SflNxtChg   = *off;
106200160928         $ErrMessage  = *off;
106300160928         $ErrGenerico = *off;
106400160928         %subst(IndDspF : 50) = *off;
106500160928
106600160928         //?___________________________________________________________?
106700160928         //?Initialize the key fields to sort on.?
106800160928         //?There is one extra field not in the subfile -- Selected --?
106900160928         //?that is added to the record. This field is used to place?
107000160928         //?selected records in front of nonselected records.?
107100160928         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
107200160928
107300160928         clear  QLGKL;
107400160928         clear  QLGSKL;
107500160928         clear  QLGSCB;
107600160928         clear  QLGSCB00;
107700160928
107800160928         // -?Gestione della scelta ordinamento:?
107900160928         Select;
108000160928
108100160928           // -?Ordinamento per Codice Commerciale?
108200160928           When  $Ord_x_Des;
108300160928             // -?2 campi chiave?
108400160928             QLGNBRK  = 2;
108500160928             // -?1° campo: Denominazione Ente (SH1des)?
108600160928             //            ?a posizone    1, 70 byte, char, ascending.?
108700160928             QLGSP    = 1;
108800160928             QLGSS    = %size(SH1des);
108900160928             QLGDT    = c_Carattere;
109000160928             QLGSO    = c_Ascendente;
109100160928             QLGKL(1) = QLGSKL;
109200160928             // -?2° campo: Codice Unico Ufficio (S01uff)?
109300160928             //            ?a posizone  243,  6 byte, char, ascending.?
109400160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
109500160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
109600160928                        %size(S01opz)   + %size(S01des)   +
109700160928                        %size(S01cf)    + %size(S01uoDes) + 1;
109800160928             QLGSS    = %size(S01uff);
109900160928             QLGDT    = c_Carattere;
110000160928             QLGSO    = c_Ascendente;
110100160928             QLGKL(2) = QLGSKL;
110200160928
110300160928           // -?Ordinamento per Codice Fiscale Ente (S01cf)?
110400160928           when  $Ord_x_CF;
110500160928             // -?2 campi chiave?
110600160928             QLGNBRK  = 2;
110700160928             // -?1° campo: Codice Fiscale Ente (S01cf)?
110800160928             //            ?a posizone  207, 11 byte, char, ascending.
110900160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
111000160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
111100160928                        %size(S01opz)   + %size(S01des)   + 1;
111200160928             QLGSS    = %size(S01cf);
111300160928             QLGDT    = c_Carattere;
111400160928             QLGSO    = c_Ascendente;
111500160928             QLGKL(1) = QLGSKL;
111600160928             // -?2° campo: Codice Unico Ufficio (S01uff)?
111700160928             //            ?a posizone  243,  6 byte, char, ascending.?
111800160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
111900160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
112000160928                        %size(S01opz)   + %size(S01des)   +
112100160928                        %size(S01cf)    + %size(S01uoDes) + 1;
112200160928             QLGSS    = %size(S01uff);
112300160928             QLGDT    = c_Carattere;
112400160928             QLGSO    = c_Ascendente;
112500160928             QLGKL(2) = QLGSKL;
112600160928
112700160928           // -?Ordinamento per Denominazione Unità Organizzativa?
112800160928           when  $Ord_x_UODes;
112900160928             // -?2 campi chiave?
113000160928             QLGNBRK  = 2;
113100160928             // -?1° campo: Denominazione Unità Organizzativa (SH1uoDes)?
113200160928             //            ?a posizone  106, 40 byte, char, ascending.?
113300160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   + 1;
113400160928             QLGSS    = %size(SH1uoDes);
113500160928             QLGDT    = c_Carattere;
113600160928             QLGSO    = c_Ascendente;
113700160928             QLGKL(1) = QLGSKL;
113800160928             // -?2° campo: cod. comm.le (S1Ccmm)?
113900160928             //            ?a posizone    3,  7 byte, int,  ascending.?
114000160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
114100160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
114200160928                        %size(S01opz)   + %size(S01des)   +
114300160928                        %size(S01cf)    + %size(S01uoDes) + 1;
114400160928             QLGSS    = %size(S01uff);
114500160928             QLGDT    = c_Carattere;
114600160928             QLGSO    = c_Ascendente;
114700160928             QLGKL(2) = QLGSKL;
114800160928
114900160928           // -?Ordinamento per Codice Univoco Ufficio?
115000160928           When  $Ord_x_Uff;
115100160928             // -?1 campo chiave?
115200160928             QLGNBRK  = 1;
115300160928             // -?1° campo: Codice Unico Ufficio (S01uff)?
115400160928             //            ?a posizone  243,  6 byte, char, ascending.?
115500160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
115600160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
115700160928                        %size(S01opz)   + %size(S01des)   +
115800160928                        %size(S01cf)    + %size(S01uoDes) + 1;
115900160928             QLGSS    = %size(S01uff);
116000160928             QLGDT    = c_Carattere;
116100160928             QLGSO    = c_Ascendente;
116200160928             QLGKL(1) = QLGSKL;
116300160928
116400160928           // -?Ordinamento per Comune sede legale Ente?
116500160928           When  $Ord_x_Loc;
116600160928             // -?2 campi chiave?
116700160928             QLGNBRK  = 2;
116800160928             // -?1° campo: Comune sede legale Ente (SH1loc)?
116900160928             //            ?a posizone   71, 35 byte, char, ascending.?
117000160928             QLGSP    = %size(SH1des)   + 1;
117100160928             QLGSS    = %size(SH1loc);
117200160928             QLGDT    = c_Carattere;
117300160928             QLGSO    = c_Ascendente;
117400160928             QLGKL(1) = QLGSKL;
117500160928             // -?2° campo: Codice Unico Ufficio (S01uff)?
117600160928             //            ?a posizone  243,  6 byte, char, ascending.?
117700160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
117800160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
117900160928                        %size(S01opz)   + %size(S01des)   +
118000160928                        %size(S01cf)    + %size(S01uoDes) + 1;
118100160928             QLGSS    = %size(S01uff);
118200160928             QLGDT    = c_Carattere;
118300160928             QLGSO    = c_Ascendente;
118400160928             QLGKL(2) = QLGSKL;
118500160928
118600160928           // -?Ordinamento per Provincia sede legale Ente?
118700160928           When  $Ord_x_Prv;
118800160928             // -?2 campi chiave?
118900160928             QLGNBRK  = 2;
119000160928             // -?1° campo: Comune sede legale Ente (SH1loc)?
119100160928             //            ?a posizone   71, 35 byte, char, ascending.?
119200160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
119300160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
119400160928                        %size(S01opz)   + %size(S01des)   +
119500160928                        %size(S01cf)    + %size(S01uoDes) +
119600160928                        %size(S01uff)   + %size(S01Loc)   + 1;
119700160928             QLGSS    = %size(S01prv);
119800160928             QLGDT    = c_Carattere;
119900160928             QLGSO    = c_Ascendente;
120000160928             QLGKL(1) = QLGSKL;
120100160928             // -?2° campo: Codice Unico Ufficio (S01uff)?
120200160928             //            ?a posizone  243,  6 byte, char, ascending.?
120300160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
120400160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
120500160928                        %size(S01opz)   + %size(S01des)   +
120600160928                        %size(S01cf)    + %size(S01uoDes) + 1;
120700160928             QLGSS    = %size(S01uff);
120800160928             QLGDT    = c_Carattere;
120900160928             QLGSO    = c_Ascendente;
121000160928             QLGKL(2) = QLGSKL;
121100160928
121200160928           // -?Ordinamento per Comune sede Unità Organizzativa?
121300160928           When  $Ord_x_UOLoc;
121400160928             // -?2 campi chiave?
121500160928             QLGNBRK  = 2;
121600160928             // -?1° campo: Comune sede legale Ente (SH1loc)?
121700160928             //            ?a posizone   71, 35 byte, char, ascending.?
121800160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
121900160928                        %size(SH1uoDes) + 1;
122000160928             QLGSS    = %size(SH1uoLoc);
122100160928             QLGDT    = c_Carattere;
122200160928             QLGSO    = c_Ascendente;
122300160928             QLGKL(1) = QLGSKL;
122400160928             // -?2° campo: Codice Unico Ufficio (S01uff)?
122500160928             //            ?a posizone  243,  6 byte, char, ascending.?
122600160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
122700160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
122800160928                        %size(S01opz)   + %size(S01des)   +
122900160928                        %size(S01cf)    + %size(S01uoDes) + 1;
123000160928             QLGSS    = %size(S01uff);
123100160928             QLGDT    = c_Carattere;
123200160928             QLGSO    = c_Ascendente;
123300160928             QLGKL(2) = QLGSKL;
123400160928
123500160928           // -?Ordinamento per Provincia sede Unità Organizzativa?
123600160928           When  $Ord_x_UOPrv;
123700160928             // -?2 campi chiave?
123800160928             QLGNBRK  = 2;
123900160928             // -?1° campo: Comune sede legale Ente (SH1loc)?
124000160928             //            ?a posizone   71, 35 byte, char, ascending.?
124100160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
124200160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
124300160928                        %size(S01opz)   + %size(S01des)   +
124400160928                        %size(S01cf)    + %size(S01uoDes) +
124500160928                        %size(S01uff)   + %size(S01Loc)   +
124600160928                        %size(S01prv)   + %size(S01uoLoc) + 1;
124700160928             QLGSS    = %size(S01uoPrv);
124800160928             QLGDT    = c_Carattere;
124900160928             QLGSO    = c_Ascendente;
125000160928             QLGKL(1) = QLGSKL;
125100160928             // -?2° campo: Codice Unico Ufficio (S01uff)?
125200160928             //            ?a posizone  243,  6 byte, char, ascending.?
125300160928             QLGSP    = %size(SH1des)   + %size(SH1loc)   +
125400160928                        %size(SH1uoDes) + %size(SH1uoLoc) +
125500160928                        %size(S01opz)   + %size(S01des)   +
125600160928                        %size(S01cf)    + %size(S01uoDes) + 1;
125700160928             QLGSS    = %size(S01uff);
125800160928             QLGDT    = c_Carattere;
125900160928             QLGSO    = c_Ascendente;
126000160928             QLGKL(2) = QLGSKL;
126100160928
126200160928         EndSl;
126300160928
126400160928         // -?Load other sort parameters.?
126500160928         QLGLB   = 80 + 16 * c_MaxKey;
126600160928         QLGRL   = %size(SflRcd) - 1;
126700160928         QLGRT   = 8;
126800160928         QLGOKL  = 80;
126900160928         QLGLKE  = 16;
127000160928         QLGLSS  = 290;
127100160928         // -?Initialize Sort I/O API fields.?
127200160928         QLGRL00 = QLGRL;
127300160928         QLGRC00 = 1;
127400160928         clear  QUSEI;
127500160928         QUSBPRV = %size(QUSEC);
127600160928
127700160928         // -?First step - Initialize the sort routine.?
127800160928         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
127900160928                      SizeList : ReturnSize : QUSEC );
128000160928
128100160928         // -?Next step - Write records to I/O routine.?
128200160928         QLGRT00  = c_Put;
128300160928         For  Nrr = 1  To  RrnLast;
128400160928           chain  Nrr  TAPAS01;
128500160928           // -?Solo le righe con Selected = 'Y' sono riordinate,?
128600160928           //  ?quindi per fare un ordinamento di tutte le righe?
128700160928           //  ?metto 'Y' sempre.?
128800160928           Selected = 'Y';
128900160928           clear  QUSEI;
129000160928           QUSBPRV  = %size(QUSEC);
129100160928           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
129200160928                         SizeList : NotUsed : QUSEC );
129300160928         EndFor;
129400160928
129500160928         // -?Next step - Signal end of input, clear subfile for reload.?
129600160928         QLGRT00 = c_EndPut;
129700160928         clear  QUSEI;
129800160928         QUSBPRV = %size(QUSEC);
129900160928         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
130000160928                       SizeList : NotUsed : QUSEC );
130100160928
130200160928         // -?Pulizia subfile?
130300160928         $SflDsp_N    = *on;
130400160928         $SflDspCtl_N = *on;
130500160928         write  TAPAC01;
130600160928         $SflDspCtl_N = *off;
130700160928         $SflEnd      = *off;
130800160928
130900160928         // -?Final step - Write the records back to the subfile.?
131000160928         QLGRT00  = c_Get;
131100160928         For  Nrr = 1  To RrnLast;
131200160928           clear  QUSEI;
131300160928           QUSBPRV = %size(QUSEC);
131400160928           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
131500160928                         QLGRL00  : NotUsed : QUSEC );
131600160928           // -?Ripristino indicatori utilizzati nel sfl rec?
131700160928           $SflNxtChg = ( S01opz <> *zero );
131800160928           S01nrr = Nrr;
131900160928           write  TAPAS01;
132000160928         EndFor;
132100160928
132200161003         C1CsrRrn  = 1;
132300160928
132400160928         // -?Visualizzazione del SFL (se ci sono dati)?
132500160928         $SflDsp_N = ( S01nrr <= *zero );
132600160928
132700160928         // -?Attivazione del SFLEND?
132800161003         $SflEnd   = ( $EoF = *on );
132900160928
133000160928         // -?(Dis)Abilitazione tasti funzionali?
133100160928         // ·?F8 = Ordinamento x ...?
133200161003         $F8Attivo = ( S01nrr > 1 );
133300160928
133400160928         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
133500160928         Select;
133600160928           When  Not $F8Attivo;
133700160928             clear  P01f08;
133800160928           When  $Ord_x_Des;
133900160928             P01f08 = 'F8=Ord. x C.F.   ';
134000160928           When  $Ord_x_CF;
134100160928             P01f08 = 'F8=Ord. x UnitàOr';
134200160928           When  $Ord_x_UODes;
134300160928             P01f08 = 'F8=Ord. x Ufficio';
134400160928           When  $Ord_x_Uff;
134500160928             P01f08 = 'F8=Ord. x Local. ';
134600160928           When  $Ord_x_Loc;
134700160928             P01f08 = 'F8=Ord. x Prov.  ';
134800160928           When  $Ord_x_Prv;
134900160928             P01f08 = 'F8=Ord. x Loc. UO';
135000160928           When  $Ord_x_UOLoc;
135100160928             P01f08 = 'F8=Ord. x Prov.UO';
135200160928           When  $Ord_x_UOPrv;
135300160928             P01f08 = 'F8=Ord. x Descr. ';
135400160928         EndSl;
135500160928
135600160928       ENDSR;
135700160928
135800160928       //--------------------------------------------------------------
135900160928       //?Preparazione stringa SQL per estrazione dati da FIPSL00F.    ?
136000160928       //--------------------------------------------------------------
136100160928       BEGSR  sr_PrepSQL;
136200160928
136300160928         wSQL = 'select Rrn(AZIPA00F), AZIPA00F.* +
136400161003                   from AZIPA00F';
136500160928
136600160928         if  D01cf    <> *blank;
136700161003           if  Not $First;
136800161003             wSQL +=  ' and';
136900161003           else;
137000161003             wSQL +=  ' where';
137100161003             $First = *off;
137200161003           endif;
137300161003           wSQL +=    ' IPAcf  = '''      + D01cf    + '''';
137400160928         endif;
137500160928
137600160928         //*·if  D01uff   <> *blank;
137700160928         //*·  if  D01cf    <> *blank;
137800160928         //*·    wSQL +=  ' and';
137900160928         //*·  endif;
138000160928         //*·  wSQL +=    ' IPAuff = '''      + D01uff   + '''';
138100160928         //*·endif;
138200160928
138300160928         if  D01uoLoc <> *blank;
138400161003           if  Not $First;
138500161003             wSQL +=  ' and';
138600161003           else;
138700161003             wSQL +=  ' where';
138800161003             $First = *off;
138900161003           endif;
139000160928           wSQL +=    ' IPAuoLoc like ''' + D01uoLoc + '%''';
139100160928         endif;
139200160928
139300160928         if  D01uoCap <> *blank;
139400161003           if  Not $First;
139500161003             wSQL +=  ' and';
139600161003           else;
139700161003             wSQL +=  ' where';
139800161003             $First = *off;
139900161003           endif;
140000160928           wSQL +=    ' IPAuoCap = '''    + D01uoCap + '''';
140100160928         endif;
140200160928
140300160928         if  D01uoPrv <> *blank;
140400161003           if  Not $First;
140500161003             wSQL +=  ' and';
140600161003           else;
140700161003             wSQL +=  ' where';
140800161003             $First = *off;
140900161003           endif;
141000160928           wSQL +=    ' IPAuoPrv = '''    + D01uoPrv + '''';
141100160928         endif;
141200160928
141300160928         wSQL +=    ' +
141400160928                  order by ';
141500160928         select;
141600160928           when  $Ord_x_Des;
141700160928             wSQL +=   'IPAdes, IPAuff';
141800160928           when  $Ord_x_CF;
141900160928             wSQL +=   'IPAcf, IPAuff';
142000160928           when  $Ord_x_UODes;
142100160928             wSQL +=   'IPAuoDes, IPAuff';
142200160928           when  $Ord_x_Uff;
142300160928             wSQL +=   'IPAuff';
142400160928           when  $Ord_x_Loc;
142500160928             wSQL +=   'IPAloc, IPAuff';
142600160928           when  $Ord_x_Prv;
142700160928             wSQL +=   'IPAprv, IPAuff';
142800160928           when  $Ord_x_UOLoc;
142900160928             wSQL +=   'IPAuoLoc, IPAuff';
143000160928           when  $Ord_x_UOPrv;
143100160928             wSQL +=   'IPAuoPrv, IPAuff';
143200160928         endsl;
143300160928
143400160928         wSQL +=  ' for fetch only';
143500160928
143600160928       ENDSR;
143700160928
143800160928       //--------------------------------------------------------------
143900160928       //?Apertura cursore.                                            ?
144000160928       //--------------------------------------------------------------
144100160928       BEGSR  sr_OpenCursor;
144200160928
144300160928         // -?Dichiarazione cursore?
144400160928         exec sql   prepare S1   from :wSQL;
144500160928         exec sql   declare C1   cursor for S1;
144600160928
144700160928         if  SQLcode < *zero;
144800160928           $EoF  = *on;
144900160928           if  $Called;
145000160928             $Fine = *on;
145100160928             tntaPAds.oPAerr = *on;
145200160928             tntaPAds.oPAmsg = sk_Msg(03);
145300160928           else;
145400160928             VIDmsg = sk_Msg(03);
145500160928             $ErrMessage  = *on;
145600160928             $ErrGenerico = *on;
145700160928           endif;
145800160928         endif;
145900160928
146000160928         // -?Apertura del cursore?
146100160928         exec sql   open C1;
146200160928
146300160928         if  SQLcode < *zero;
146400160928           $EoF  = *on;
146500160928           if  $Called;
146600160928             $Fine = *on;
146700160928             tntaPAds.oPAerr = *on;
146800160928             tntaPAds.oPAmsg = sk_Msg(03);
146900160928           else;
147000160928             VIDmsg = sk_Msg(03);
147100160928             $ErrMessage  = *on;
147200160928             $ErrGenerico = *on;
147300160928           endif;
147400160928         endif;
147500160928
147600160928       ENDSR;
147700160928
147800160928       //--------------------------------------------------------------
147900160928       //?Chiusura cursore.                                            ?
148000160928       //--------------------------------------------------------------
148100160928       BEGSR  sr_CloseCursor;
148200160928
148300160928         // -?Chiusura del cursore?
148400160928         exec sql   close C1;
148500160928
148600160928       ENDSR;
148700160928
148800160928       //--------------------------------------------------------------
148900160928       //?Lettura cursore.                                             ?
149000160928       //--------------------------------------------------------------
149100160928       BEGSR  sr_ReadCursor;
149200160928
149300160928         $EoF  = *off;
149400160928         clear  AZIPAds;
149500160928
149600160928         exec sql   fetch next   from C1   into :wNrrAZIPA, :AZIPAds;
149700160928
149800160928         select;
149900160928           when  SQLcode = 100;
150000160928             $EoF  = *on;
150100160928           when  SQLcode < *zero;
150200160928             $EoF  = *on;
150300160928             if  $Called;
150400160928               $Fine = *on;
150500160928               tntaPAds.oPAerr = *on;
150600160928               tntaPAds.oPAmsg = sk_Msg(03);
150700160928             else;
150800160928               VIDmsg = sk_Msg(03);
150900160928               $ErrMessage  = *on;
151000160928               $ErrGenerico = *on;
151100160928             endif;
151200160928         endsl;
151300160928
151400160928       ENDSR;
151500160928
151600160928       //--------------------------------------------------------------
151700160928       //?Operazioni finali.                                           ?
151800160928       //--------------------------------------------------------------
151900160928       BEGSR  sr_RoutEnd;
152000160928
152100160928         // -?Restituzione parametri al chiamante?
152200160928         if  $Called;
152300160928           KPJBU = TNTAPAds;
152400160928         endif;
152500160928
152600160928         // -?Uscita dal *pgm?
152700160928         return;
152800160928
152900160928       ENDSR;
153000160928
153100160928      /end-free
153200160928
153300160928       //--------------------------------------------------------------
153400160928       //?Definizione schiere a tempo di compilazione                  ?
153500160928       //--------------------------------------------------------------
153600160928
153700160928** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
153800160928Raggiunto il n° massimo degli uffici di P.A. interrogabili a video             1
153900160928Opzione errata                                                                 2
154000160928Rilevato errore nell'estrazione dei dati della P.A.  -  Conttatare il CED      3
