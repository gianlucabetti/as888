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
001700160928     h dftactgrp(*no) bnddir('TRUL')
001800160928     h alwnull(*inputonly)
001900160928
002000160928       //--------------------------------------------------------------
002100160928       //?Dichiarazione file.                                          ?
002200160928       //--------------------------------------------------------------
002300160928
002400160928       // -?Indice Pubbliche Amministrazioni?
002500160928     fAZIPA00F  if   e           k disk    usropn
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
008000160928     d sk_Msg          s             78    dim( 2)  ctdata  perrcd(1)
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
014400160928
014500160928       //--------------------------------------------------------------
014600160928       //?Definizione variabili globali.                               ?
014700160928       //--------------------------------------------------------------
014800160928
014900160928       // -?Flags booleani?
015000160928     d $Called         s               n   inz
015100160928     d $Fine           s               n   inz
015200160928     d $EoF            s               n   inz
015300160928     d $InzD01         s               n   inz(*on)
015400160928     d $InzD02         s               n   inz(*on)
015500160928     d $InzS01         s               n   inz(*on)
015600160928
015700160928       // -?Variabili per la gestione del video?
015800160928     d $Video          s              2    inz('D1')
015900160928
016000160928       // -?Variabili per la gestione dei subfile?
016100160928     d S01nrr          s              5i 0 inz
016200160928
016300160928       // -?Stringa SQL da eseguire?
016400160928     d wSql            s          32740    inz   varying
016500160928
016600160928       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
016700160928
016800160928       //--------------------------------------------------------------
016900160928       // -?Constants?
017000160928       //--------------------------------------------------------------
017100160928       // -?MaxKey - Maximum number of key fields allowed by this program?
017200160928     d c_MaxKey        c                   const(2)
017300160928       // -?Sort order:?
017400160928       //  ?1 = Sort in an ascending sequence?
017500160928       //  ?2 = Sort in a descending sequence?
017600160928     d c_Ascendente    c                   const(1)
017700160928     d c_Discendente   c                   const(2)
017800160928       // -?Key data type:?
017900160928       //  ? 0 = Signed binary?
018000160928       //  ? 2 = Signed zoned decimal?
018100160928       //  ? 3 = Signed packed decimal?
018200160928       //  ? 6 = Character with no national language sort sequence applied,?
018300160928       //  ?     if specified?
018400160928       //  ? 7 = Unsigned packed decimal?
018500160928       //  ?     All numbers will have the sign forced positive ('F'X).?
018600160928       //  ? 8 = Unsigned zoned decimal?
018700160928       //  ?     All numbers will have the sign forced positive ('F'X).?
018800160928       //  ? 9 = Unsigned binary?
018900160928       //  ?14 = Date in form of DD/MM/YY?
019000160928       //  ?15 = Date in form of DD.MM.YYYY?
019100160928     d c_Numero        c                   const(2)
019200160928     d c_Carattere     c                   const(6)
019300160928     d c_NumIntero     c                   const(8)
019400160928       //
019500160928     d c_Put           c                   const(1)
019600160928     d c_EndPut        c                   const(2)
019700160928     d c_Get           c                   const(3)
019800160928
019900160928       //--------------------------------------------------------------
020000160928       // -?Data Structures?
020100160928       //  ?SflRcd     - The entire subfile record to pass to the sort?
020200160928       //  ?QLGSCB     - The sort request block for the QLGSORT API?
020300160928       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
020400160928       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
020500160928       //  ?QUSEC      - Error structure for the QLGSORT API?
020600160928       //--------------------------------------------------------------
020700160928     d SflRcd          ds                  inz
020800160928     d   S01opz                            inz
020900160928     d   S01des                            inz
021000160928     d   S01cmm                            inz
021100160928     d   S01cf                             inz
021200160928     d   S01uoDes                          inz
021300160928     d   S01uff                            inz
021400160928     d   S01loc                            inz
021500160928     d   S01prv                            inz
021600160928     d   S01uoLoc                          inz
021700160928     d   S01uoPrv                          inz
021800160928     d   SH1des                            inz
021900160928     d   SH1loc                            inz
022000160928     d   SH1uoDes                          inz
022100160928     d   SH1uoLoc                          inz
022200160928     d   Selected                     1a   inz
022300160928      /copy QSYSINC/QRPGLESRC,QLGSORT
022400160928     d QLGKL                         16    dim(c_MaxKey)
022500160928     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
022600160928     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
022700160928     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
022800160928     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
022900160928      /copy QSYSINC/QRPGLESRC,QLGSRTIO
023000160928      /copy QSYSINC/QRPGLESRC,QUSEC
023100160928
023200160928       //--------------------------------------------------------------
023300160928       // -?Standalone fields?
023400160928       //  ?Nrr        - Relative record number for subfile?
023500160928       //  ?SizeList   - Size of list?
023600160928       //  ?ReturnSize - Size of list returned by sort API?
023700160928       //--------------------------------------------------------------
023800160928     d NotUsed         s             16a   inz
023900160928     d ReturnSize      s              9b 0 inz
024000160928     d SizeList        s              9b 0 inz
024100160928     d RrnLast         s              5i 0 inz
024200160928     d Nrr             s              5i 0 inz
024300160928
024400160928       //--------------------------------------------------------------
024500160928       //?Definizione prototipi procedure usate.                       ?
024600160928       //--------------------------------------------------------------
024700160928
024800160928       // -?Reperimento dati utente?
024900160928     d TIBS34ds      e ds                  inz
025000160928      /copy gaitrasrc/srcProtoPR,TIBS34R
025100160928
025200160928       //--------------------------------------------------------------
025300160928       //?Definizione key-list.                                        ?
025400160928       //--------------------------------------------------------------
025500160928
025600160928
025700160928       //--------------------------------------------------------------
025800160928       //?Riepilogo indicatori utilizzati.                             ?
025900160928       //--------------------------------------------------------------
026000160928
026100160928
026200160928       //--------------------------------------------------------------
026300160928       //?M A I N - L I N E                                            ?
026400160928       //--------------------------------------------------------------
026500160928
026600160928     c     *Entry        plist
026700160928     c                   parm                    KPJBA
026800160928
026900160928      /free
027000160928
027100160928       // -?Operazioni iniziali?
027200160928       exsr  sr_RoutInz;
027300160928
027400160928       // -?Gestione videata?
027500160928       DoW  Not $Fine;
027600160928
027700160928         select;
027800160928
027900160928           // -?Filtro di lancio?
028000160928           when  $Video = 'D1';
028100160928             exsr  sr_GesD01;
028200160928
028300160928           // -?SubFile?
028400160928           when  $Video = 'S1';
028500160928             exsr  sr_GesS01;
028600160928
028700160928           // -?Dettaglio?
028800160928           when  $Video = 'D2';
028900160928             exsr  sr_GesD02;
029000160928
029100160928           // -?? ? ??
029200160928           other;
029300160928             $Fine = *on;
029400160928
029500160928         endsl;
029600160928
029700160928       EndDo;
029800160928
029900160928       // -?Operazioni finali?
030000160928       exsr  sr_RoutEnd;
030100160928
030200160928       //--------------------------------------------------------------
030300160928       //?Operazioni iniziali.                                         ?
030400160928       //--------------------------------------------------------------
030500160928       BEGSR  sr_RoutInz;
030600160928
030700160928         // -?Impostazione opzioni per SQL?
030800160928         exec sql   set  option  DynUsrPrf = *Owner,
030900160928                                 CloSqlCsr = *EndMod;
031000160928
031100160928         // -?Impostazione chiusura?
031200160928         *inLR = *on;
031300160928
031400160928         // -?Reperimento dati job?
031500160928         exsr  sr_DatiJob;
031600160928
031700160928         // -?Impostazione nome programma a video?
031800160928         V1Tpgm = SDSpgm;
031900160928
032000160928         // -?Verifica SE ricevuti parametri?
032100160928         reset  $Called;
032200160928         if  KPJBU <> *blank;
032300160928           $Called  = *on;
032400160928           TNTAPAds = kpjbu;
032500160928           exsr  sr_InzD01;
032600160928           exsr  sr_CtrD01;
032700160928           if  Not $ErrGenerico;
032800160928             $Video   = 'S1';
032900160928             $InzS01  = *on;
033000160928           endif;
033100160928         endif;
033200160928
033300160928       ENDSR;
033400160928
033500160928       //--------------------------------------------------------------
033600160928       //?Reperimento Dati del job (Utente/Operativi).                 ?
033700160928       //--------------------------------------------------------------
033800160928       BEGSR  sr_DatiJob;
033900160928
034000160928         in(e) §AzUte;
034100160928         if NOT %error;
034200160928           in(e) §DatiUte;
034300160928         endif;
034400160928         if %error or RSut = *blank;
034500160928           tibs34r ( tibs34ds );
034600160928           in §AzUte;
034700160928           in §DatiUte;
034800160928         endif;
034900160928
035000160928       ENDSR;
035100160928
035200160928       //--------------------------------------------------------------
035300160928       //?Gestione videata D01.                                        ?
035400160928       //--------------------------------------------------------------
035500160928       BEGSR  sr_GesD01;
035600160928
035700160928         // -?Inizializzazione videata?
035800160928         if  $InzD01 = *on;
035900160928           exsr  sr_InzD01;
036000160928           $InzD01 = *off;
036100160928         endif;
036200160928
036300160928         // -?(Dis)Attivazione tasti funzionali a video?
036400160928         $F8Attivo   = *off;
036500160928         $F12Attivo  = *off;
036600160928
036700160928         // -?Emissione Testata e Piede con tasti funzionali abilitati?
036800160928         write  TAPAT01;
036900160928         write  TAPAP01;
037000160928
037100160928         // -?Emissione videata?
037200160928         exfmt  TAPAD01;
037300160928
037400160928         clear  VIDmsg;
037500160928         reset  $ErrMessage;
037600160928         reset  $ErrGenerico;
037700160928
037800160928         Select;
037900160928
038000160928           // -?F3=Fine?
038100160928           When  dsp_aid = c_F03;
038200160928             $Fine  = *on;
038300160928
038400160928           // -?Enter?
038500160928           Other;
038600160928             exsr  sr_CtrD01;
038700160928             if  $ErrGenerico;
038800160928               leavesr;
038900160928             endif;
039000160928             $InzS01 = *on;
039100160928             $Video  = 'S1';
039200160928
039300160928         EndSl;
039400160928
039500160928       ENDSR;
039600160928
039700160928       //--------------------------------------------------------------
039800160928       //?Inizializzazione videata D01.                                ?
039900160928       //--------------------------------------------------------------
040000160928       BEGSR  sr_InzD01;
040100160928
040200160928         // -?Spegnimento indicatori di errore e posizionamento cursore?
040300160928         %subst( IndDspF : 28 ) = *off;
040400160928
040500160928         // -?Pulizia videata?
040600160928         clear  TAPAD01;
040700160928
040800160928         // -?Impostazione iniziale dei campi a video?
040900160928         //  ?(SE ricevuti parametri)?
041000160928         if  $Called;
041100160928           D01cod   = tntaPAds.IPAcod;
041200160928           D01loc   = tntaPAds.IPAloc;
041300160928           D01cap   = tntaPAds.IPAcap;
041400160928           D01cf    = tntaPAds.IPAcf;
041500160928           D01uff   = tntaPAds.IPAuff;
041600160928           D01uoLoc = tntaPAds.IPAuoLoc;
041700160928           D01uoCap = tntaPAds.IPAuoCap;
041800160928         endif;
041900160928
042000160928       ENDSR;
042100160928
042200160928       //--------------------------------------------------------------
042300160928       //?Inizializzazione videata D01.                                ?
042400160928       //--------------------------------------------------------------
042500160928       BEGSR  sr_CtrD01;
042600160928
042700160928         // -?Spegnimento indicatori di errore e posizionamento cursore?
042800160928         %subst(IndDspF : 28) = *off;
042900160928
043000160928       ENDSR;
043100160928
043200160928       //--------------------------------------------------------------
043300160928       //?Gestione subfile S01.                                        ?
043400160928       //--------------------------------------------------------------
043500160928       BEGSR  sr_GesS01;
043600160928
043700160928         // -?Inizializzazione subfile S01?
043800160928         if  $InzS01 = *on;
043900160928           exsr  sr_InzS01;
044000160928           $InzS01 = *off;
044100160928         endif;
044200160928
044300160928         // -?(Dis)Attivazione tasti funzionali a video?
044400160928         $F8Attivo   = ( S01nrr > 1 );
044500160928         $F12Attivo  = *on;
044600160928
044700160928         // -?Posizionamento cursore?
044800160928         //  ?C1CsrRrn contiene il numero di riga del SubFile su cui?
044900160928         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
045000160928         //  ?si visualizza la pagina che era visualizzata quando?
045100160928         //  ?l'utente ha premuto l'ultimo tasto.?
045200160928         if  C1CsrRrn > *zero;
045300160928           C1RcdNbr = C1CsrRrn;
045400160928         else;
045500160928           C1RcdNbr = 1;
045600160928           write  TAPAS00;          //(no rec.)?
045700160928         endif;
045800160928
045900160928         // -?Emissione Testata e Piede con tasti funzionali abilitati?
046000160928         write  TAPAT01;
046100160928         write  TAPAP01;
046200160928
046300160928         // -?Emissione videata?
046400160928         exfmt  TAPAC01;
046500160928
046600160928         clear  VIDmsg;
046700160928         reset  $ErrMessage;
046800160928         reset  $ErrGenerico;
046900160928
047000160928         Select;
047100160928
047200160928           // -?F3=Fine?
047300160928           When  dsp_aid = c_F03;
047400160928             exsr  sr_CloseCursor;
047500160928             $Fine = *on;
047600160928             if  $Called;
047700160928               oPAfxx = '1';
047800160928             endif;
047900160928
048000160928           // -?F8=Ordinamento?
048100160928           When  dsp_aid = c_F08;
048200160928             exsr  sr_F08S01;
048300160928
048400160928           // -?F12=Ritorno?
048500160928           When  dsp_aid = c_F12;
048600160928             exsr  sr_CloseCursor;
048700160928             if  $Called;
048800160928               $Fine  = *on;
048900160928               oPAfxx = '2';
049000160928             else;
049100160928               $Video = 'D1';
049200160928             endif;
049300160928
049400160928           // -?Roll-Up?
049500160928           When  dsp_aid = c_RollUp;
049600160928             // -?Verifica raggiungimento del limite di records?
049700160928             //  ?registrabili nel subfile (9999)?
049800160928             if  S01nrr = *hival;
049900160928               $ErrGenerico = *on;
050000160928               $ErrMessage  = *on;
050100160928               VIDmsg = sk_Msg(01);
050200160928               leavesr;
050300160928             endif;
050400160928             exsr sr_RollUpS01;
050500160928
050600160928           // -?SubFile vuoto?
050700160928           When  S01nrr = *zero;
050800160928
050900160928           // -?Invio?
051000160928           Other;
051100160928             // -?Gestione opzioni?
051200160928             exsr  sr_OpzS01;
051300160928             if  $ErrGenerico;
051400160928               leavesr;
051500160928             endif;
051600160928
051700160928         EndSl;
051800160928
051900160928       ENDSR;
052000160928
052100160928       //--------------------------------------------------------------
052200160928       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
052300160928       //?F08 = Cambia ordinamento Sfl                                 ?
052400160928       //--------------------------------------------------------------
052500160928       BEGSR  sr_F08S01;
052600160928
052700160928         Select;
052800160928           When $Ord_x_CodCmm;
052900160928             $Ord_x_CodCmm = *off;
053000160928             $Ord_x_DesCmm = *on;
053100160928             $Ord_x_CodUni = *off;
053200160928           When $Ord_x_DesCmm;
053300160928             $Ord_x_CodCmm = *off;
053400160928             $Ord_x_DesCmm = *off;
053500160928             $Ord_x_CodUni = *on;
053600160928           When $Ord_x_CodUni;
053700160928             $Ord_x_CodCmm = *on;
053800160928             $Ord_x_DesCmm = *off;
053900160928             $Ord_x_CodUni = *off;
054000160928         EndSl;
054100160928         Select;
054200160928           When  Not F8Attivo;
054300160928             clear  $Ord_x_Des;
054400160928             clear  $Ord_x_CF;
054500160928             clear  $Ord_x_UODes;
054600160928             clear  $Ord_x_Uff;
054700160928             clear  $Ord_x_Loc;
054800160928             clear  $Ord_x_Prv;
054900160928             clear  $Ord_x_UOLoc;
055000160928             clear  $Ord_x_UOPrv;
055100160928           When  $Ord_x_Des;
055200160928             $Ord_x_Des   = *off;
055300160928             $Ord_x_CF    = *on;
055400160928             $Ord_x_UODes = *off;
055500160928             $Ord_x_Uff   = *off;
055600160928             $Ord_x_Loc   = *off;
055700160928             $Ord_x_Prv   = *off;
055800160928             $Ord_x_UOLoc = *off;
055900160928             $Ord_x_UOPrv = *off;
056000160928           When  $Ord_x_CF;
056100160928             $Ord_x_Des   = *off;
056200160928             $Ord_x_CF    = *off;
056300160928             $Ord_x_UODes = *on;
056400160928             $Ord_x_Uff   = *off;
056500160928             $Ord_x_Loc   = *off;
056600160928             $Ord_x_Prv   = *off;
056700160928             $Ord_x_UOLoc = *off;
056800160928             $Ord_x_UOPrv = *off;
056900160928           When  $Ord_x_UODes;
057000160928             $Ord_x_Des   = *off;
057100160928             $Ord_x_CF    = *off;
057200160928             $Ord_x_UODes = *off;
057300160928             $Ord_x_Uff   = *on;
057400160928             $Ord_x_Loc   = *off;
057500160928             $Ord_x_Prv   = *off;
057600160928             $Ord_x_UOLoc = *off;
057700160928             $Ord_x_UOPrv = *off;
057800160928           When  $Ord_x_Uff;
057900160928             $Ord_x_Des   = *off;
058000160928             $Ord_x_CF    = *off;
058100160928             $Ord_x_UODes = *off;
058200160928             $Ord_x_Uff   = *off;
058300160928             $Ord_x_Loc   = *on;
058400160928             $Ord_x_Prv   = *off;
058500160928             $Ord_x_UOLoc = *off;
058600160928             $Ord_x_UOPrv = *off;
058700160928           When  $Ord_x_Loc;
058800160928             $Ord_x_Des   = *off;
058900160928             $Ord_x_CF    = *off;
059000160928             $Ord_x_UODes = *off;
059100160928             $Ord_x_Uff   = *off;
059200160928             $Ord_x_Loc   = *off;
059300160928             $Ord_x_Prv   = *on;
059400160928             $Ord_x_UOLoc = *off;
059500160928             $Ord_x_UOPrv = *off;
059600160928           When  $Ord_x_Prv;
059700160928             $Ord_x_Des   = *off;
059800160928             $Ord_x_CF    = *off;
059900160928             $Ord_x_UODes = *off;
060000160928             $Ord_x_Uff   = *off;
060100160928             $Ord_x_Loc   = *off;
060200160928             $Ord_x_Prv   = *off;
060300160928             $Ord_x_UOLoc = *on;
060400160928             $Ord_x_UOPrv = *off;
060500160928           When  $Ord_x_UOLoc;
060600160928             $Ord_x_Des   = *off;
060700160928             $Ord_x_CF    = *off;
060800160928             $Ord_x_UODes = *off;
060900160928             $Ord_x_Uff   = *off;
061000160928             $Ord_x_Loc   = *off;
061100160928             $Ord_x_Prv   = *off;
061200160928             $Ord_x_UOLoc = *off;
061300160928             $Ord_x_UOPrv = *on;
061400160928           When  $Ord_x_UOPrv;
061500160928             $Ord_x_Des   = *on;
061600160928             $Ord_x_CF    = *off;
061700160928             $Ord_x_UODes = *off;
061800160928             $Ord_x_Uff   = *off;
061900160928             $Ord_x_Loc   = *off;
062000160928             $Ord_x_Prv   = *off;
062100160928             $Ord_x_UOLoc = *off;
062200160928             $Ord_x_UOPrv = *off;
062300160928         EndSl;
062400160928
062500160928         select;
062600160928           // -?Subfile vuoto: nessun record da ordinare?
062700160928           when  S01nrr <= 1;
062800160928           // -?Subfile già parzializzato: occorre ricaricarlo?
062900160928           when  SavC01des   <> C01des    or
063000160928                 SavC01cf    <> C01cf     or
063100160928                 SavC01uoDes <> C01uoDes  or
063200160928                 SavC01uff   <> C01uff    or
063300160928                 SavC01loc   <> C01loc    or
063400160928                 SavC01prv   <> C01prv    or
063500160928                 SavC01uoLoc <> C01uoLoc  or
063600160928                 SavC01uoPrv <> C01uoPrv;
063700160928             $InzS01 = *on;
063800160928           // -?Altrimenti: basta riordinarlo?
063900160928           other;
064000160928             exsr  sr_SortSfl;
064100160928         endsl;
064200160928
064300160928       ENDSR;
064400160928
064500160928       //--------------------------------------------------------------
064600160928       //?Inizializzazione subfile S01.                                ?
064700160928       //--------------------------------------------------------------
064800160928       BEGSR  sr_InzS01;
064900160928
065000160928         // -?Pulizia subfile?
065100160928         $SflDsp_N    = *on;
065200160928         $SflDspCtl_N = *on;
065300160928         write  TAPAC01;
065400160928         $SflDspCtl_N = *off;
065500160928         $SflEnd      = *off;
065600160928
065700160928         $SflNxtChg   = *off;
065800160928
065900160928         clear  C1RcdNbr;
066000160928         clear  C1CsrRrn;
066100160928         clear  S01nrr;
066200160928         clear  VIDmsg;
066300160928         $ErrMessage  = *off;
066400160928         $ErrGenerico = *off;
066500160928
066600160928         // -?Spegnimento degli indicatori di errore?
066700160928         %subst( IndDspF : 50 ) = *off;
066800160928
066900160928         // -?Descrizione Opzioni?
067000160928         if  $Called;
067100160928           C01opzD = '1=Selezione, 5=Visualizzazione.';
067200160928         else;
067300160928           C01opzD = '5=Visualizzazione.';
067400160928         endif;
067500160928
067600160928         SavC01des   = C01des;
067700160928         SavC01cf    = C01cf;
067800160928         SavC01uoDes = C01uoDes;
067900160928         SavC01uff   = C01uff;
068000160928         SavC01loc   = C01loc;
068100160928         SavC01prv   = C01prv;
068200160928         SavC01uoLoc = C01uoLoc;
068300160928         SavC01uoPrv = C01uoPrv;
068400160928
068500160928         // -?Caricamento completo dei dati nel subfile?
068600160928         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
068700160928         //  ?l'eventuale ordinamento)?
068800160928         // -?Preparazione stringa SQL?
068900160928         exsr  sr_PrepSQL;
069000160928         // -?Apertura del cursore?
069100160928         exsr  sr_OpenCursor;
069200160928         // -?Lettura del file?
069300160928         exsr  sr_ReadCursor;
069400160928         DoW  Not $EoF;
069500160928           exsr  sr_CaricaS01;
069600160928         EndDo;
069700160928
069800160928         // -?Visualizzazione del SFL (se ci sono dati)?
069900160928         $SflDsp_N = ( S01nrr <= *zero );
070000160928
070100160928         // -?Attivazione del SFLEND?
070200160928         $SflEnd   = ( $EoF = *on );
070300160928
070400160928         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
070500160928         RrnLast  = S01nrr;
070600160928
070700160928         // -?Impaginazione subfile?
070800160928         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
070900160928         if  S01nrr > *zero;
071000160928           C1RcdNbr = 1;
071100160928           C1CsrRrn = 1;
071200160928         else;
071300160928           clear  C1RcdNbr;
071400160928         endif;
071500160928
071600160928         // -?(Dis)Abilitazione tasti funzionali?
071700160928         // ·?F8 = Ordinamento x ...?
071800160928         F8Attivo  = ( S01nrr > 1 );
071900160928
072000160928         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
072100160928         Select;
072200160928           When  Not F8Attivo;
072300160928             clear  P01f08;
072400160928           When  $Ord_x_Des;
072500160928             P01f08 = 'F8=Ord. x C.F.   ';
072600160928           When  $Ord_x_CF;
072700160928             P01f08 = 'F8=Ord. x UnitàOr';
072800160928           When  $Ord_x_UODes;
072900160928             P01f08 = 'F8=Ord. x Ufficio';
073000160928           When  $Ord_x_Uff;
073100160928             P01f08 = 'F8=Ord. x Local. ';
073200160928           When  $Ord_x_Loc;
073300160928             P01f08 = 'F8=Ord. x Prov.  ';
073400160928           When  $Ord_x_Prv;
073500160928             P01f08 = 'F8=Ord. x Loc. UO';
073600160928           When  $Ord_x_UOLoc;
073700160928             P01f08 = 'F8=Ord. x Prov.UO';
073800160928           When  $Ord_x_UOPrv;
073900160928             P01f08 = 'F8=Ord. x Descr. ';
074000160928         EndSl;
074100160928
074200160928       ENDSR;
074300160928
074400160928       //--------------------------------------------------------------
074500160928       //?Caricamento singola pagine SubFile S01.                      ?
074600160928       //--------------------------------------------------------------
074700160928       BEGSR  sr_CaricaS01;
074800160928
074900160928         $SflNxtChg = *off;
075000160928
075100160928         // -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
075200160928         //  ?(occorre caricare il SFL al COMPLETO per riuscire a gestirne?
075300160928         //  ?l'eventuale ordinamento)?
075400160928         DoW  $EoF   = *off    and
075500160928              S01nrr < *hival;
075600160928
075700160928           // -?Caricamento dati nel record del subfile?
075800160928           exsr  sr_RieRecS01;
075900160928           S01nrr += 1;
076000160928           write  TAPAS01;
076100160928
076200160928           // -?Lettura record successivo nell'archivio?
076300160928           exsr  sr_ReadCursor;
076400160928
076500160928         EndDo;
076600160928
076700160928         // -?Memorizzazione posizionamento effettuato?
076800160928         select;
076900160928           when  $Ord_x_Des;
077000160928             SavC01pos = C01des;
077100160928           when  $Ord_x_CF;
077200160928             SavC01pos = C01cf;
077300160928           when  $Ord_x_UOdes;
077400160928             SavC01pos = C01uoDes;
077500160928           when  $Ord_x_Uff;
077600160928             SavC01pos = C01uff;
077700160928           when  $Ord_x_Loc;
077800160928             SavC01pos = C01loc;
077900160928           when  $Ord_x_Prv;
078000160928             SavC01pos = C01prv;
078100160928           when  $Ord_x_UOloc;
078200160928             SavC01pos = C01uoLoc;
078300160928           when  $Ord_x_UOprv;
078400160928             SavC01pos = C01uoPrv;
078500160928         endsl;
078600160928
078700160928         // -?Visualizzazione del SFL (se ci sono dati)?
078800160928         $SflDsp_N = ( S01nrr <= *zero );
078900160928
079000160928         // -?Attivazione (eventuale) del SFLEND?
079100160928         $SflEnd   = ( $EoF = *on );
079200160928
079300160928         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
079400160928         if  S01nrr > *zero;
079500160928           C1RcdNbr = 1;
079600160928         else;
079700160928           clear  C1RcdNbr;
079800160928         endif;
079900160928
080000160928         C1CsrRrn = C1RcdNbr;
080100160928
080200160928       ENDSR;
080300160928
080400160928       //--------------------------------------------------------------
080500160928       //?Caricamento singolo record nel SubFile S01.                  ?
080600160928       //--------------------------------------------------------------
080700160928       BEGSR  sr_RieRecS01;
080800160928
080900160928         // -?La pulizia spegnerebbe l'indicatore dell'ordinamento?
081000160928         //*·clear  TAPAS01;
081100160928
081200160928         // -?Campi Hidden?
081300160928         // ·?Denominazione Ente?
081400160928         SH1des   = AZIPAds.IPAdes;
081500160928         // ·?Comune sede legale Ente?
081600160928         SH1loc   = AZIPAds.IPAloc;
081700160928         // ·?Denominazione Unità Organizzativa?
081800160928         SH1uoDes = AZIPAds.IPAuoDes;
081900160928         // ·?Comune Unità Organizzativa?
082000160928         SH1uoLoc = AZIPAds.IPAuoLoc;
082100160928
082200160928         // -?Campi Output?
082300160928         // ·?Denominazione Ente?
082400160928         S01des   = AZIPAds.IPAdes;
082500160928         // ·?Codice Fiscale?
082600160928         S01cf    = AZIPAds.IPAcf;
082700160928         // ·?Denominazione Unità Organizzativa?
082800160928         S01uoDes = AZIPAds.IPAuoDes;
082900160928         // ·?Codice univoco Ufficio?
083000160928         S01uff   = AZIPAds.IPAuff;
083100160928         // ·?Comune sede legale Ente?
083200160928         S01loc   = AZIPAds.IPAloc;
083300160928         // ·?Provincia sede legale Ente?
083400160928         S01prv   = AZIPAds.IPAprv;
083500160928         // ·?Comune sede Unità Organizzativa?
083600160928         S01uoLoc = AZIPAds.IPAuoLoc;
083700160928         // ·?Provincia sede Unità Organizzativa?
083800160928         S01uoPrv = AZIPAds.IPAuoPrv;
083900160928
084000160928         // -?Attributi di Visualizzazione?
084100160928         // ·?Campo di Ordinamento?
084200160928         if  Not $Ord_x_Des    and
084300160928             Not $Ord_x_CF     and
084400160928             Not $Ord_x_UOdes  and
084500160928             Not $Ord_x_Uff    and
084600160928             Not $Ord_x_Loc    and
084700160928             Not $Ord_x_Prv    and
084800160928             Not $Ord_x_UOloc  and
084900160928             Not $Ord_x_UOprv;
085000160928           $Ord_x_Des = *on;
085100160928         endif;
085200160928
085300160928       ENDSR;
085400160928
085500160928       //--------------------------------------------------------------
085600160928       //?Gestione opzioni nel SubFile S01.                            ?
085700160928       //--------------------------------------------------------------
085800160928       BEGSR  sr_OpzS01;
085900160928
086000160928         // -?Ciclo di lettura subfile?
086100160928         readc  TAPAS01;
086200160928
086300160928         DOW  NOT  %eof(FITAPAD);
086400160928
086500160928           %subst( IndDspF : 50 ) = *off;
086600160928           $SflNxtChg  = *off;
086700160928           C1RcdNbr    = S01nrr;
086800160928
086900160928           Select;
087000160928
087100160928             // -?Nessuna opzione?
087200160928             When  S01opz = *blank;
087300160928
087400160928             // -?5 = Visualizzazione BarCode per Profilo Spunta?
087500160928             When  S01opz = '5';
087600160928               $Video  = 'D2';
087700160928               $InzD02 = *on;
087800160928               DoW  $Video = 'D2';
087900160928                 exsr  sr_GesD02;
088000160928               EndDo;
088100160928               if  dsp_aid = c_F03;
088200160928                 $ErrGenerico = *on;
088300160928               endif;
088400160928               $Video = 'S1';
088500160928
088600160928             // -?? = Opzione NON valida?
088700160928             Other;
088800160928               $ErrGenerico = *on;
088900160928               $ErrMessage  = *on;
089000160928               $PosCurOpz   = *on;
089100160928               VIDmsg = sk_Msg(02);
089200160928
089300160928           EndSl;
089400160928
089500160928           // -?Aggiornamento subfile?
089600160928           Select;
089700160928             When  $ErrMessage;
089800160928               $SflNxtChg = *on;
089900160928               C1CsrRrn   = C1RcdNbr;
090000160928             When  $ErrGenerico;
090100160928               $SflNxtChg = *on;
090200160928               C1CsrRrn   = C1RcdNbr;
090300160928               clear  S01opz;
090400160928             Other;
090500160928               C1CsrRrn   = C1RcdNbr;
090600160928               clear  S01opz;
090700160928           EndSl;
090800160928
090900160928           update  TAPAS01;
091000160928
091100160928           // -?Uscita dal ciclo di lettura?
091200160928           if  $ErrMessage  or  $ErrGenerico;
091300160928             clear  $InzS01;
091400160928             leave;
091500160928           endif;
091600160928
091700160928           // -?Lettura rec. variato sucessivo?
091800160928           readc  TAPAS01;
091900160928
092000160928         EndDo;
092100160928
092200160928         // -?Se richiesto il RI-caricamento del subfile:?
092300160928         //  ?occorre chiudere il cursore SQL?
092400160928         if  $InzS01  and  Not $errGenerico;
092500160928           exsr  sr_CloseCursor;
092600160928         endif;
092700160928
092800160928       ENDSR;
092900160928
093000160928       //--------------------------------------------------------------
093100160928       //?Gestione videata D02.                                        ?
093200160928       //--------------------------------------------------------------
093300160928       BEGSR  sr_GesD02;
093400160928
093500160928         // -?Inizializzazione videata?
093600160928         if  $InzD02 = *on;
093700160928           exsr  sr_InzD02;
093800160928           $InzD02 = *off;
093900160928         endif;
094000160928
094100160928         // -?(Dis)Attivazione tasti funzionali a video?
094200160928         $F8Attivo   = *off;
094300160928         $F12Attivo  = *on;
094400160928
094500160928         // -?Emissione Testata e Piede con tasti funzionali abilitati?
094600160928         write  TAPAT01;
094700160928         write  TAPAP01;
094800160928
094900160928         // -?Emissione videata?
095000160928         exfmt  TAPAD02;
095100160928
095200160928         clear  VIDmsg;
095300160928         reset  $ErrMessage;
095400160928         reset  $ErrGenerico;
095500160928
095600160928         Select;
095700160928
095800160928           // -?F3=Fine?
095900160928           When  dsp_aid = c_F03;
096000160928             $Video = 'S1';
096100160928
096200160928           // -?F12=Ritorno?
096300160928           When  dsp_aid = c_F12;
096400160928             $Video = 'S1';
096500160928
096600160928         EndSl;
096700160928
096800160928       ENDSR;
096900160928
097000160928       //--------------------------------------------------------------
097100160928       //?Inizializzazione videata D02.                                ?
097200160928       //--------------------------------------------------------------
097300160928       BEGSR  sr_InzD02;
097400160928
097500160928         // -?Spegnimento indicatori di errore e posizionamento cursore?
097600160928         %subst( IndDspF : 50 ) = *off;
097700160928
097800160928         // -?Pulizia videata?
097900160928         clear  TAPAD02;
098000160928
098100160928         // -?Reperimento dati?
098200160928         chain  wNrrAZIPA  AZIPA000;
098300160928         if  Not %found(AZIPA00F);
098400160928           leavesr;
098500160928         endif;
098600160928
098700160928         // -?Impostazione iniziale dei campi a video?
098800160928         //  ?(SE ricevuti parametri)?
098900160928         D02cod   = IPAcod;
099000160928         D02des   = IPAdes;
099100160928         D02ind   = IPAind;
099200160928         D02cap   = IPAcap;
099300160928         D02loc   = IPAloc;
099400160928         D02prv   = IPAprv;
099500160928         D02cf    = IPAcf;
099600160928         D02vcf   = IPAvcf;
099700160928         D02uff   = IPAuff;
099800160928         D02uoDes = IPAuoDes;
099900160928         D02uoInd = IPAuoInd;
100000160928         D02uoCap = IPAuoCap;
100100160928         D02uoLoc = IPAuoLoc;
100200160928         D02uoPrv = IPAuoPrv;
100300160928         D02uoEml = IPAuoEml;
100400160928
100500160928       ENDSR;
100600160928
100700160928       //--------------------------------------------------------------
100800160928       //?Preparazione stringa SQL per estrazione dati da FIPSL00F.    ?
100900160928       //--------------------------------------------------------------
101000160928       BEGSR  sr_PrepSQL;
101100160928
101200160928         wSQL = 'select * +
101300160928                   from AZIPA00F +
101400160928                  where';
101500160928
101600160928         if  D01cod   <> *blank;
101700160928           wSQL +=    ' IPAcod like ''' + D01cod + '%''';
101800160928         endif;
101900160928
102000160928         if  D01loc   <> *blank;
102100160928           if  D01cod   <> *blank;
102200160928             wSQL +=  ' and';
102300160928           endif;
102400160928           wSQL +=    ' IPAloc like '''   + D01loc   + '%''';
102500160928         endif;
102600160928
102700160928         if  D01cap   <> *blank;
102800160928           if  D01cod   <> *blank  or
102900160928               D01loc   <> *blank;
103000160928             wSQL +=  ' and';
103100160928           endif;
103200160928           wSQL +=    ' IPAcap = '''      + D01cap   + '''';
103300160928         endif;
103400160928
103500160928         if  D01cf    <> *blank;
103600160928           if  D01cod   <> *blank  or
103700160928               D01loc   <> *blank  or
103800160928               D01cap   <> *blank;
103900160928             wSQL +=  ' and';
104000160928           endif;
104100160928           wSQL +=    ' IPAcf  = '''      + D01cf    + '''';
104200160928         endif;
104300160928
104400160928         if  D01uff   <> *blank;
104500160928           if  D01cod   <> *blank  or
104600160928               D01loc   <> *blank  or
104700160928               D01cap   <> *blank  or
104800160928               D01cf    <> *blank;
104900160928             wSQL +=  ' and';
105000160928           endif;
105100160928           wSQL +=    ' IPAuff = '''      + D01uff   + '''';
105200160928         endif;
105300160928
105400160928         if  D01uoLoc <> *blank;
105500160928           if  D01cod   <> *blank  or
105600160928               D01loc   <> *blank  or
105700160928               D01cap   <> *blank  or
105800160928               D01cf    <> *blank  or
105900160928               D01uff   <> *blank;
106000160928             wSQL +=  ' and';
106100160928           endif;
106200160928           wSQL +=    ' IPAuoLoc like ''' + D01uoLoc + '%''';
106300160928         endif;
106400160928
106500160928         if  D01uoCap <> *blank;
106600160928           if  D01cod   <> *blank  or
106700160928               D01loc   <> *blank  or
106800160928               D01cap   <> *blank  or
106900160928               D01cf    <> *blank  or
107000160928               D01uff   <> *blank  or
107100160928               D01uoLoc <> *blank;
107200160928             wSQL +=  ' and';
107300160928           endif;
107400160928           wSQL +=    ' IPAuoCap = '''    + D01uoCap + '''';
107500160928         endif;
107600160928
107700160928         wSQL +=    ' +
107800160928                  order by ';
107900160928         select;
108000160928           when  $Ord_x_Des;
108100160928             wSQL +=   'IPAdes, IPAuff';
108200160928           when  $Ord_x_CF;
108300160928             wSQL +=   'IPAcf, IPAuff';
108400160928           when  $Ord_x_UODes;
108500160928             wSQL +=   'IPAuoDes, IPAuff';
108600160928           when  $Ord_x_Uff;
108700160928             wSQL +=   'IPAuff';
108800160928           when  $Ord_x_Loc;
108900160928             wSQL +=   'IPAloc, IPAuff';
109000160928           when  $Ord_x_Prv;
109100160928             wSQL +=   'IPAprv, IPAuff';
109200160928           when  $Ord_x_UOLoc;
109300160928             wSQL +=   'IPAuoLoc, IPAuff';
109400160928           when  $Ord_x_UOPrv;
109500160928             wSQL +=   'IPAuoPrv, IPAuff';
109600160928         endsl;
109700160928
109800160928         wSQL +=  ' for fetch only';
109900160928
110000160928       ENDSR;
110100160928
110200160928       //--------------------------------------------------------------
110300160928       //?Apertura cursore.                                            ?
110400160928       //--------------------------------------------------------------
110500160928       BEGSR  sr_OpenCursor;
110600160928
110700160928         // -?Dichiarazione cursore?
110800160928         exec sql   prepare S1   from :wSQL;
110900160928         exec sql   declare C1   cursor for S1;
111000160928
111100160928         if  SQLcode < *zero;
111200160928           $EoF  = *on;
111300160928           exsr  sr_PrintERR;
111400160928         endif;
111500160928
111600160928         // -?Apertura del cursore?
111700160928         exec sql   open C1;
111800160928
111900160928         if  SQLcode < *zero;
112000160928           $EoF  = *on;
112100160928           exsr  sr_PrintERR;
112200160928         endif;
112300160928
112400160928       ENDSR;
112500160928
112600160928       //--------------------------------------------------------------
112700160928       //?Chiusura cursore.                                            ?
112800160928       //--------------------------------------------------------------
112900160928       BEGSR  sr_CloseCursor;
113000160928
113100160928         // -?Chiusura del cursore?
113200160928         exec sql   close C1;
113300160928
113400160928       ENDSR;
113500160928
113600160928       //--------------------------------------------------------------
113700160928       //?Lettura cursore.                                             ?
113800160928       //--------------------------------------------------------------
113900160928       BEGSR  sr_ReadCursor;
114000160928
114100160928         $EoF  = *off;
114200160928         clear  AZIPAds;
114300160928
114400160928         exec sql   fetch next   from C1   into :AZIPAds;
114500160928
114600160928         select;
114700160928           when  SQLcode = 100;
114800160928             $EoF  = *on;
114900160928           when  SQLcode < *zero;
115000160928             $EoF  = *on;
115100160928             exsr  sr_PrintERR;
115200160928         endsl;
115300160928
115400160928       ENDSR;
115500160928
115600160928       //--------------------------------------------------------------
115700160928       //?Operazioni finali.                                           ?
115800160928       //--------------------------------------------------------------
115900160928       BEGSR  sr_RoutEnd;
116000160928
116100160928         // -?Restituzione parametri al chiamante?
116200160928         if  $Called;
116300160928           KPJBU = TNTAPAds;
116400160928         endif;
116500160928
116600160928         // -?Uscita dal *pgm?
116700160928         return;
116800160928
116900160928       ENDSR;
117000160928
117100160928      /end-free
117200160928
117300160928       //--------------------------------------------------------------
117400160928       //?Definizione schiere a tempo di compilazione                  ?
117500160928       //--------------------------------------------------------------
117600160928
117700160928** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
117800160928Raggiunto il n° massimo degli uffici di P.A. interrogabili a video             1
117900160928Opzione errata                                                                 2
