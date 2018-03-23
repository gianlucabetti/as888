000100120917       //==============================================================
000200120917       //?Gestione tabella "FFR" (Forzatura Filiale di Arrivo)         ?
000300120917       //==============================================================
000400120917
000500120917       //--------------------------------------------------------------
000600120917       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120917       //--------------------------------------------------------------
000800120917
000900120917     /*PRM dbgview(*source)
001000120917     /*END
001100120917
001200120917       //--------------------------------------------------------------
001300120917       //?Specifiche di controllo.                                     ?
001400120917       //--------------------------------------------------------------
001500120917
001600120917     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001700120917
001800120917       //--------------------------------------------------------------
001900120917       //?Dichiarazione file.                                          ?
002000120917       //--------------------------------------------------------------
002100120917
002200120917       // -?Organigramma?
002300120917     fAZORG01L  if   e           k disk
002400120917
002500120917       // -?Tabella "FFR"?
002600120917     fTNTBE01L  Uf A e           k disk
002700120917
002800120917       // -?Video?
002900120917     fTNTBFFRD  cf   e             workstn
003000120917     f                                     sfile(TBFFRS1 : S01nrr)
003100120917     f                                     sfile(TBFFRS2 : S02nrr)
003200120917     f                                     indds(IndDspF)
003300120917     f                                     infds(InfDspF)
003400120917
003500120917       //--------------------------------------------------------------
003600120917       //?Definizione costanti.                                        ?
003700120917       //--------------------------------------------------------------
003800120917
003900120917       // -?Codice tabella in gestione?
004000120917     d c_Tab           c                   const('FFR')
004100120918
004200120918       // -?Costante per controllo "caratteri solo numerici"?
004300120918     d c_Digits        c                   const('0123456789')
004400120917
004500120917       // -?Tasti funzionali a video?
004600120917     d c_F01           c                   const(x'31')
004700120917     d c_F02           c                   const(x'32')
004800120917     d c_F03           c                   const(x'33')
004900120917     d c_F04           c                   const(x'34')
005000120917     d c_F05           c                   const(x'35')
005100120917     d c_F06           c                   const(x'36')
005200120917     d c_F07           c                   const(x'37')
005300120917     d c_F08           c                   const(x'38')
005400120917     d c_F09           c                   const(x'39')
005500120917     d c_F10           c                   const(x'3A')
005600120917     d c_F11           c                   const(x'3B')
005700120917     d c_F12           c                   const(x'3C')
005800120917     d c_F13           c                   const(x'B1')
005900120917     d c_F14           c                   const(x'B2')
006000120917     d c_F15           c                   const(x'B3')
006100120917     d c_F16           c                   const(x'B4')
006200120917     d c_F17           c                   const(x'B5')
006300120917     d c_F18           c                   const(x'B6')
006400120917     d c_F19           c                   const(x'B7')
006500120917     d c_F20           c                   const(x'B8')
006600120917     d c_F21           c                   const(x'B9')
006700120917     d c_F22           c                   const(x'BA')
006800120917     d c_F23           c                   const(x'BB')
006900120917     d c_F24           c                   const(x'BC')
007000120917     d c_Enter         c                   const(x'F1')
007100120917     d c_RollDown      c                   const(x'F4')
007200120917     d c_RollUp        c                   const(x'F5')
007300120917
007400120917       //--------------------------------------------------------------
007500120917       //?Definizione schiere.                                         ?
007600120917       //--------------------------------------------------------------
007700120917
007800120917       // -?Messaggi di errore?
007900120919     d $Msg            s             78    dim( 8)  ctdata  perrcd( 1)
008000120917
008100120917       //--------------------------------------------------------------
008200120917       //?Definizione aree dati.                                       ?
008300120917       //--------------------------------------------------------------
008400120917
008500120917       // -?Dati utente?
008600120917     d §AzUte        e ds                  extname(AZUTE00F)
008700120917     d                                     dtaara
008800120917     d §DatiUte      e ds                  extname(dDatiUte)
008900120917     d                                     dtaara
009000120917
009100120917       //--------------------------------------------------------------
009200120917       //?Definizione strutture dati.                                  ?
009300120917       //--------------------------------------------------------------
009400120917
009500120917       // -?Status ds?
009600120917     d Status         sds
009700120917     d   PgmName         *proc
009800120917
009900120917       // -?InfDS?
010000120917     d InfDspF         ds
010100120917     d   dsp_aid             369    369a
010200120918     d   sfl_rrn             376    377i 0
010300120918     d   min_nrr             378    379i 0
010400120918     d   num_rcds            380    381i 0
010500120917
010600120917       // -?Indicatori su DspF?
010700120917     d IndDspF         ds                  inz
010800120917         // -?Abilitazione tasti funzionali?
010900120917     d   F6Attivo                      n   overlay(IndDspF : 06)
011000120919     d   F14Attivo                     n   overlay(IndDspF : 14)
011100120917         // -?Emissione messaggio di errore?
011200120917     d   ErrMessage                    n   overlay(IndDspF : 28)
011300120917         // -?Indicatori di gestione del subfile?
011400120917     d   SflDsp_N                      n   overlay(IndDspF : 30)
011500120917     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011600120917     d   SflNxtChg                     n   overlay(IndDspF : 32)
011700120917     d   SflEnd                        n   overlay(IndDspF : 33)
011800120917     d   Sfl2Dsp_N                     n   overlay(IndDspF : 34)
011900120917     d   Sfl2DspCtl_N                  n   overlay(IndDspF : 35)
012000120917     d   Sfl2NxtChg                    n   overlay(IndDspF : 36)
012100120917     d   Sfl2End                       n   overlay(IndDspF : 37)
012200120917         // -?Indicatori per Attibuti di visualizzazione?
012300120917     d   RecAnnull                     n   overlay(IndDspF : 40)
012400120917     d   ProtectPOE                    n   overlay(IndDspF : 41)
012500120918     d   ProtectKEY                    n   overlay(IndDspF : 42)
012600120917         // -?Posizionamento cursore & segnalazione errore?
012700120917     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012800120917     d   PosCurPOE                     n   overlay(IndDspF : 51)
012900120917     d   PosCurPOR                     n   overlay(IndDspF : 52)
013000120917     d   PosCurFRZ                     n   overlay(IndDspF : 53)
013100120917         //   -?Riemissione videata?
013200120917     d   ErrGenerico                   n   overlay(IndDspF : 99)
013300120918
013400120918
013500120918       // -?Numero MAX di Filiali di Emissione gestibili?
013600120918     d c_MaxFE         c                   const(85)
013700120918       // -?Filiali di Emissione in tab. "FFR"?
013800120918     d $FE_ds          ds           255    inz
013900120918     d   $FE                   1    255    inz  dim(c_MaxFE)
014000120918
014100120918
014200120918       // -?Filiali di Emissione a video nel subfile S01 (14)?
014300120918     d S01FEx_ds       ds            42    inz
014400120918     d   S01FE1                            inz
014500120918     d   S01FE2                            inz
014600120918     d   S01FE3                            inz
014700120918     d   S01FE4                            inz
014800120918     d   S01FE5                            inz
014900120918     d   S01FE6                            inz
015000120918     d   S01FE7                            inz
015100120918     d   S01FE8                            inz
015200120918     d   S01FE9                            inz
015300120918     d   S01FEA                            inz
015400120918     d   S01FEB                            inz
015500120918     d   S01FEC                            inz
015600120918     d   S01FED                            inz
015700120918     d   S01FEE                            inz
015800120918
015900120918       // -?Parametri ricevuti?
016000120918     d KPJBA         e ds
016100120918
016200120918       // -?Dati record principale della tabella?
016300120918     d TNTBEds       e ds                  inz  extname(TNTBE00F)
016400120918     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
016500120918     d                                          prefix(TBX:3)
016600120918
016700120918       //--------------------------------------------------------------
016800120918       //?Definizione variabili globali.                               ?
016900120918       //--------------------------------------------------------------
017000120918
017100120918       // -?Flags booleani?
017200120918     d $Fine           s               n   inz
017300120918     d $EoF            s               n   inz
017400120918     d $Immissione     s               n   inz
017500120918     d $InzS01         s               n   inz(*on)
017600120918     d $InzS02         s               n   inz(*on)
017700120918     d $InzW01         s               n   inz(*on)
017800120919     d $InzW02         s               n   inz(*on)
017900120919
018000120919       // -?Indici di schiera?
018100120919     d xx              s              3  0 inz
018200120918
018300120918       // -?Variabili per la gestione del video?
018400120918     d $Video          s              2    inz('S1')
018500120918     d*// W_SflPag        s              3  0 inz
018600120918     d*// wPag            s              4  0 inz
018700120918     d*// wRig            s              3  0 inz
018800120918     d S01nrr          s                   like(C1RcdNbr) inz
018900120918     d S02nrr          s                   like(C2RcdNbr) inz
019000120918     d SavS01csr       s                   like(C1RcdNbr) inz
019100120918     d SavS02csr       s                   like(C2RcdNbr) inz
019200120918
019300120918       // -?Variabili di comodo?
019400120919     d wData_EUR       s               d   datfmt(*eur)  inz(*loval)
019500120918
019600120918       //--------------------------------------------------------------
019700120918       //?Definizione prototipi procedure.                             ?
019800120918       //--------------------------------------------------------------
019900120918
020000120918       // -?Reperimento dati utente?
020100120918     d TIBS34ds      e ds                  inz
020200120918      /copy gaitrasrc/srcProtoPR,TIBS34R
020300120918
020400120918       // -?Caricamento schiera filiali?
020500120918     d TRUL06ds      e ds                  inz
020600120919     d   D06cod      e                     inz('£1')
020700120918     d   D06tla      e                     inz('L')
020800120918     d   D06lin                1     90  0 inz  dim(30)
020900120918      /copy gaitrasrc/srcProtoPr,TRUL06R
021000120918
021100120918       // -?Ricerca codici Organigramma?
021200120918     d SD19cod         s              3    inz
021300120918     d SD19tip         s              1    inz
021400120918     d SD19des         s             25    inz
021500120919     d tnsd19r         pr                  extpgm('TNSD19R')
021600120918     d   D19cod                            like(SD19cod)
021700120918     d   D19tip                            like(SD19tip)
021800120918     d   D19des                            like(SD19des)
021900120918
022000120918       // -?API QCAPCMD (Process Commands)?
022100120918     d Qcmd            s           2048    inz  varying
022200120918      /copy qSysInc/qRpgleSrc,QCAPCMD
022300120918      /copy gaitrasrc/srcProtoPR,QCAPCMD
022400120918
022500120918       // -?Parametri gestione errori API.?
022600120918      /copy qsysinc/qrpglesrc,QUSEC
022700120918
022800120918       //--------------------------------------------------------------
022900120918       //?Definizione key-list.                                        ?
023000120918       //--------------------------------------------------------------
023100120918
023200120918       // -?File TNTBE01L?
023300120918     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
023400120918     d                                     prefix(k_)   inz
023500120918
023600120918       // -?File AZORG01L?
023700120918     d k01azorg01    e ds                  extname(AZORG01L : *key)
023800120918     d                                     prefix(k_)   inz
023900120917
024000120918       //--------------------------------------------------------------
024100120918       //?Riepilogo indicatori utilizzati.                             ?
024200120918       //--------------------------------------------------------------
024300120918
024400120918
024500120918       //--------------------------------------------------------------
024600120918       //?M A I N - L I N E                                            ?
024700120918       //--------------------------------------------------------------
024800120918
024900120918     c     *Entry        plist
025000120918     c                   parm                    KPJBA
025100120917
025200120918      /free
025300120918
025400120918       // -?Operazioni iniziali?
025500120918       exsr sr_RoutInz;
025600120918
025700120918       // -?Ciclo di gestione del file video?
025800120918       DoW  $Fine = *off;
025900120918
026000120918         select;
026100120918
026200120918           // -?Gestione videata S1 (subfile)?
026300120918           when $Video = 'S1';
026400120918             exsr sr_GesS01;
026500120918
026600120918           // -?Gestione videata S2 (subfile)?
026700120918           //  ?(NON da qua)?
026800120918           //when $Video = 'S2';
026900120918           //  exsr sr_GesS02;
027000120918
027100120918           // -?Richiesta trasmissione dati?
027200120918           //  ?(NON da qua)?
027300120918           //when $Video = 'W1';
027400120918           //  exsr  sr_GesW01;
027500120918
027600120918           // -?? ? ??
027700120918           other;
027800120918             $Fine = *on;
027900120918
028000120918         endsl;
028100120918
028200120918       EndDo;
028300120918
028400120918       // -?Operazioni finali?
028500120918       exsr sr_RoutEnd;
028600120918
028700120918       //--------------------------------------------------------------
028800120918       //?Operazioni iniziali.                                         ?
028900120918       //--------------------------------------------------------------
029000120918       BEGSR  sr_RoutInz;
029100120918
029200120918         // -?Impostazione chiusura?
029300120918         *inLR = *on;
029400120918
029500120918         // -?Reperimento dati job?
029600120918         exsr  sr_DatiJob;
029700120918
029800120918         // -?Aggancio dati generali della tabella in esame?
029900120918         clear  k_TBEcod;
030000120918         k_TBEke1 = *zero;
030100120918         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
030200120918                = c_Tab;
030300120918         clear  k_TBEke2;
030400120918         clear  k_TBElin;
030500120918         k_TBEsif = KNSIF;
030600120918         chain  %kds(k05tntbe01)  TNTBE000;
030700120918         if  not %found(TNTBE01L);
030800120918           clear  k_TBEsif;
030900120918           chain  %kds(k05tntbe01)  TNTBE000;
031000120918         endif;
031100120918         if  %found(TNTBE01L);
031200120918           xTNTBEds = TNTBEds;
031300120918         else;
031400120918           clear  xTNTBEds;
031500120918         endif;
031600120918
031700120918         // -?Pulizia / impostazione iniziale dei campi chiave?
031800120918         clear  k05tntbe01;
031900120918         k_TBEcod = c_Tab;
032000120918
032100120918       ENDSR;
032200120918
032300120918       //--------------------------------------------------------------
032400120918       //?Reperimento Dati del job (Utente/Operativi).                 ?
032500120918       //--------------------------------------------------------------
032600120918       BEGSR  sr_DatiJob;
032700120918
032800120918         in(e) §AzUte;
032900120918         if NOT %error;
033000120918           in(e) §DatiUte;
033100120918         endif;
033200120918         if %error or RSut = *blank;
033300120918           tibs34r ( tibs34ds );
033400120918           in §AzUte;
033500120918           in §DatiUte;
033600120918         endif;
033700120918
033800120918       ENDSR;
033900120918
034000120918       //--------------------------------------------------------------
034100120918       //?Gestione subfile S01.                                        ?
034200120918       //--------------------------------------------------------------
034300120918       BEGSR  sr_GesS01;
034400120918
034500120918         // -?Inizializzazione videata?
034600120918         if  $InzS01 = *on;
034700120918           exsr  sr_InzS01;
034800120918           $InzS01 = *off;
034900120918         endif;
035000120918
035100120918         // -?Emissione Testata & Piede?
035200120919         write  tbFFRt1;
035300120918         write  tbFFRp1;
035400120918
035500120918         // -?Posizionamento cursore?
035600120918         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
035700120918         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
035800120918         //  ?si visualizza la pagina che vedeva l'utente quando ha?
035900120918         //  ?premuto l'ultimo tasto.?
036000120918         if  C1CsrRrn > *zero;
036100120918           C1RcdNbr = C1CsrRrn;
036200120918         else;
036300120918           C1RcdNbr = 1;
036400120919           write  tbFFRs0;             //?(no rec.)?
036500120918         endif;
036600120918
036700120918         // -?Emissione videata?
036800120918         exfmt  tbFFRc1;
036900120918
037000120918         reset  ErrGenerico;
037100120918         reset  ErrMessage;
037200120918         clear  V1Dmsg;
037300120918
037400120918         Select;
037500120918
037600120918           // -?F3=Fine?
037700120918           When  dsp_aid = c_F03;
037800120918             $Fine = *on;
037900120918
038000120918           // -?F5=Rivisualizza?
038100120918           When  dsp_aid = c_F05;
038200120918             $InzS01 = *on;
038300120918
038400120918           // -?F10=Immissione?
038500120918           When  dsp_aid = c_F10;
038600120918             $Video  = 'S2';
038700120918             $InzS02 = *on;
038800120918             DoW  $Video = 'S2'  and  Not $Fine;
038900120918               exsr  sr_GesS02;
039000120918             EndDo;
039100120918
039200120918           // -?SubFile vuoto?
039300120918           When  S01nrr = *zero;
039400120918
039500120918           // -?Invio?
039600120918           Other;
039700120918             // -?Gestione opzioni?
039800120918             exsr  sr_OpzS01;
039900120918             if  ErrGenerico;
040000120918               leavesr;
040100120918             endif;
040200120918
040300120918         endsl;
040400120918
040500120918       ENDSR;
040600120918
040700120918       //--------------------------------------------------------------
040800120918       //?Inizializzazione subfile S01.                                ?
040900120918       //--------------------------------------------------------------
041000120918       BEGSR  sr_InzS01;
041100120918
041200120918         // -?Spegnimento degli indicatori di errore?
041300120918         %subst(IndDspF : 50) = *off;
041400120918
041500120918         // -?Pulizia subfile?
041600120918         SflDsp_N    = *on;
041700120918         SflDspCtl_N = *on;
041800120919         write  tbFFRc1;
041900120918         SflDspCtl_N = *off;
042000120918         SflEnd      = *off;
042100120918
042200120918         //clear  W_SflPag;
042300120918         clear  C1RcdNbr;
042400120918         clear  C1CsrRrn;
042500120918         clear  S01nrr;
042600120918         clear  V1Dmsg;
042700120918         ErrMessage  = *off;
042800120918         ErrGenerico = *off;
042900120918
043000120918         // -?Caricamento completo dei dati nel subfile?
043100120918         setll  %kds(k05tntbe01)  TNTBE000;
043200120918         reade  %kds(k05tntbe01 : 1)  TNTBE000;
043300120918         $EoF = (%eof(TNTBE01L));
043400120918         exsr  sr_CaricaS01;
043500120918
043600120918         // -?Impaginazione subfile?
043700120918         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
043800120918         if S01nrr > *zero;
043900120918           C1RcdNbr  = 1;
044000120918           C1CsrRrn  = 1;
044100120918         else;
044200120918           clear C1RcdNbr;
044300120918         endif;
044400120918
044500120918       ENDSR;
044600120918
044700120918       //--------------------------------------------------------------
044800120918       //?Caricamento completo del subfile S01                         ?
044900120918       //--------------------------------------------------------------
045000120918       BEGSR  sr_CaricaS01;
045100120918
045200120918         clear  S01nrr;
045300120918         SflNxtChg = *off;
045400120918
045500120918         // -?Ciclo di caricamento dati da tab. "3EW"?
045600120918         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
045700120918         //  ?l'eventuale ordinamento)?
045800120918         DoW  Not $EoF;
045900120918
046000120918           // ·?Impostazione campi?
046100120919           clear  tbFFRs1;
046200120918           S1TBEke1 = TBEke1;
046300120918           S1TBEke2 = TBEke2;
046400120918           if  TBEatb  = 'A';
046500120919              S1TBEann = 'Ann';
046600120918           endif;
046700120918
046800120918           // ·?Filiali di Emissione?
046900120918           S01FEx_ds = TBEuni;
047000120918           if  %subst( TBEuni : 43 : 3 ) <> *blank;
047100120918             S01piu = '+';
047200120918           endif;
047300120918
047400120918           // ·?Decodifica Filiale di Ritiro?
047500120918           if  %check( c_Digits : S1TBEke1 ) = *zero;
047600120918             k_ORGfil = %int( S1TBEke1 );
047700120918             chain  %kds(k01azorg01)  AZORG;
047800120918             if  %found(AZORG01L);
047900120918               S1Dpor = ORGdes;
048000120918             endif;
048100120918           endif;
048200120918
048300120918           // ·?Decodifica Filiale di Ritiro Forzata?
048400120918           if  %check( c_Digits : S1TBEke2 ) = *zero;
048500120918             k_ORGfil = %int( S1TBEke2 );
048600120918             chain  %kds(k01azorg01)  AZORG;
048700120918             if  %found(AZORG01L);
048800120918               S1Dprf = ORGdes;
048900120918             endif;
049000120918           endif;
049100120918
049200120918           // ·?Scrittura record nel subfile?
049300120918           S01nrr += 1;
049400120919           write  tbFFRs1;
049500120918
049600120918           // -?Lettura record successivo?
049700120918           reade  %kds(k05tntbe01 : 1)  TNTBE000;
049800120918           $EoF = (%eof(TNTBE01L));
049900120918
050000120918         EndDo;
050100120918
050200120918         // -?Visualizzazione del SFL (se ci sono dati)?
050300120918         SflDsp_N = (S01nrr <= *zero);
050400120918
050500120918         // -?Attivazione del SFLEND?
050600120918         SflEnd = ( $EoF );
050700120918
050800120918         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
050900120918         if  S01nrr > *zero;
051000120918           C1RcdNbr = 1;
051100120918         else;
051200120918           clear  C1RcdNbr;
051300120918         endif;
051400120918
051500120918         C1CsrRrn = C1RcdNbr;
051600120918
051700120918       ENDSR;
051800120918
051900120918       //--------------------------------------------------------------
052000120918       //?Caricamento completo del subfile S01                         ?
052100120918       //--------------------------------------------------------------
052200120918       BEGSR  sr_OpzS01;
052300120918
052400120918         clear  SavS01csr;
052500120918
052600120918         // -?Ciclo di lettura subfile?
052700120918         readc  tbFFRs1;
052800120918
052900120918         DoW  Not %eof(TNTBFFRD);
053000120918
053100120918           SflNxtChg = *off;
053200120918           %subst(IndDspF : 50) = *off;
053300120918           C1RcdNbr = S01nrr;
053400120918
053500120918           select;
053600120918
053700120918             // -?Opz. 2=Modifica?
053800120918             //  ?Opz. 4=Annullamento/Ripristino?
053900120918             //  ?Opz. 5=Visualizzazione?
054000120918             when  S1Copz = 2  or  S1Copz = 4  or  S1Copz = 5;
054100120918               If  S1Copz = 2  and  S1TBEann <> *blank;
054200120918                 ErrGenerico = *on;
054300120918                 ErrMessage  = *on;
054400120918                 PosCurOPZ   = *on;
054500120918                 V1Dmsg = $Msg(02);
054600120918                 $Fine  = *off;
054700120918               Else;
054800120918                 $Video  = 'S2';
054900120918                 $InzS02 = *on;
055000120918                 DoW  $Video = 'S2'  and  Not $Fine;
055100120918                   exsr  sr_GesS02;
055200120918                 EndDo;
055300120918               EndIf;
055400120918
055500120918             // -?Nessuna opzione?
055600120918             when  S1Copz = *zero;
055700120918
055800120918             // -?Opzione errata?
055900120918             other;
056000120918               ErrGenerico = *on;
056100120918               ErrMessage  = *on;
056200120918               PosCurOPZ   = *on;
056300120918               V1Dmsg = $Msg(01);
056400120918               $Fine  = *off;
056500120918
056600120918           endsl;
056700120918
056800120918           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
056900120918           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
057000120919           if  S1Copz  <> *zero    and
057100120919              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
057200120918             SavS01csr   = C1RcdNbr;
057300120918           endif;
057400120918
057500120918           // -?Aggiornamento sfl?
057600120918           select;
057700120918             when  ErrMessage;
057800120918               SflNxtChg = *on;
057900120918               C1CsrRrn  = C1RcdNbr;
058000120918             when  ErrGenerico;
058100120918               C1CsrRrn  = C1RcdNbr;
058200120918               clear  S1Copz;
058300120918             other;
058400120918               clear  S1Copz;
058500120918           endsl;
058600120918
058700120918           UPDATE  tbFFRs1;
058800120918
058900120918           if  ErrGenerico   or   ErrMessage;
059000120918             leave;
059100120918           endif;
059200120918
059300120919           readc  tbFFRs1;
059400120918
059500120918         ENDDO;
059600120918
059700120918         // -?Riposizionam. cursore sul record contenente la prima opz.?
059800120918         //  ?(se non sono stati rilevati errori)?
059900120918         if  NOT ErrMessage   and   NOT ErrGenerico   and
060000120918             SavS01csr > *zero;
060100120918           C1CsrRrn = SavS01csr;
060200120918         endif;
060300120918
060400120918       ENDSR;
060500120918
060600120918       //--------------------------------------------------------------
060700120918       //?Gestione subfile S02.                                        ?
060800120918       //--------------------------------------------------------------
060900120918       BEGSR  sr_GesS02;
061000120918
061100120918         // -?Inizializzazione videata?
061200120918         if  $InzS02 = *on;
061300120918           exsr  sr_InzS02;
061400120918           $InzS02 = *off;
061500120918         endif;
061600120918
061700120918         // -?Emissione Testata & Piede?
061800120919         write  tbFFRt1;
061900120918         write  tbFFRp2;
062000120918
062100120918         // -?Posizionamento cursore?
062200120918         //  ?C2CsrRrn contiene il numero di riga del subfile su cui?
062300120918         //  ?era posizionato il cursore; impostandolo in C2RcdNbr?
062400120918         //  ?si visualizza la pagina che vedeva l'utente quando ha?
062500120918         //  ?premuto l'ultimo tasto.?
062600120918         if  C2CsrRrn > *zero;
062700120918           C2RcdNbr = C2CsrRrn;
062800120918         endif;
062900120918
063000120918         // -?Emissione videata?
063100120918         if  Not $Immissione  AND
063200120918            (S1Copz = 5       or
063300120918            (S1Copz = 4  and  TBEatb = *blank));
063400120918           write  tbFFRc2;
063500120918           exfmt  Protect;
063600120918         else;
063700120918           exfmt  tbFFRc2;
063800120918         endif;
063900120918
064000120918         reset  ErrGenerico;
064100120918         reset  ErrMessage;
064200120918         clear  V1Dmsg;
064300120918
064400120918         Select;
064500120918
064600120918           // -?F3=Fine?
064700120918           When  dsp_aid = c_F03;
064800120918             unlock  TNTBE01L;
064900120918             //$Fine  = *on;
065000120918             $Video = 'S1';
065100120918             ErrGenerico = *on;
065200120918
065300120918           // -?F12=Ritorno?
065400120918           When  dsp_aid = c_F12;
065500120918             unlock  TNTBE01L;
065600120918             $Video = 'S1';
065700120918
065800120918           // -?F14=Inserimento per terminal di partenza?
065900120918           when  dsp_aid = c_F14;
066000120919              exsr  sr_CtrC02;
066100120918              if  not ErrGenerico;
066200120919                $Video  = 'W2';
066300120919                $InzW02 = *on;
066400120919              endif;
066500120919              DoW  $Video = 'W2';
066600120919                exsr sr_GesW02;
066700120919              EndDo;
066800120918
066900120918           // -?Invio; F5=Ripristino; F6=Conferma; F16=Annullamento?
067000120918           Other;
067100120918             // -?Controlli eseguiti NON se F16=Annullamento?
067200120918             if  dsp_aid <> c_F16;
067300120919               exsr  sr_CtrC02;
067400120918             // -?Controlli eseguiti SOLO se F16=Annullamento?
067500120918             //else;
067600120918             //  exsr  sr_CtrANN;
067700120918             endif;
067800120918             if  ErrGenerico;
067900120918               leavesr;
068000120918             endif;
068100120918             // -?Aggiornamento?
068200120918             if  dsp_aid = c_F06;
068300120918               $Video  = 'W1';
068400120918               $InzW01 = *on;
068500120918               DoW  $Video = 'W1';
068600120918                 exsr  sr_GesW01;
068700120918               EndDo;
068800120918             endif;
068900120918
069000120918         EndSl;
069100120918
069200120918       ENDSR;
069300120918
069400120918       //--------------------------------------------------------------
069500120918       //?Inizializzazione subfile S02.                                ?
069600120918       //--------------------------------------------------------------
069700120918       BEGSR  sr_InzS02;
069800120918
069900120918         // -?Spegnimento degli indicatori di errore?
070000120918         %subst(IndDspF : 50) = *off;
070100120918
070200120918         // -?Pulizia subfile-control?
070300120918         clear  tbFFRc2;
070400120918
070500120918         // -?Pulizia subfile?
070600120918         Sfl2Dsp_N    = *on;
070700120918         Sfl2DspCtl_N = *on;
070800120919         write  tbFFRc2;
070900120918         Sfl2DspCtl_N = *off;
071000120918         Sfl2End      = *off;
071100120918
071200120918         //clear  W_SflPag;
071300120918         clear  C2RcdNbr;
071400120918         clear  C2CsrRrn;
071500120918         clear  S02nrr;
071600120918         clear  V1Dmsg;
071700120918         ErrMessage  = *off;
071800120918         ErrGenerico = *off;
071900120918
072000120918         // -?Caricamento dei dati nel subfile-control?
072100120918         select;
072200120918            when  dsp_aid = c_F10;
072300120918              D2DesOpz = '  IMMISSIONE   ';
072400120918            when  S1Copz  = 2;
072500120918              D2DesOpz = '   MODIFICA    ';
072600120919            when  S1Copz  = 4  and  S1TBEann = *blank;
072700120918              D2DesOpz = ' ANNULLAMENTO  ';
072800120919            when  S1Copz  = 4  and  S1TBEann <> *blank;
072900120918              D2DesOpz = '  RIPRISTINO   ';
073000120918            when  S1Copz  = 5;
073100120918              D2DesOpz = 'VISUALIZZAZIONE';
073200120918         endsl;
073300120919
073400120919         // ·?Protezione campi chiave SE NON inserimento?
073500120919         $Immissione = (dsp_aid = c_F10);
073600120919         ProtectKEY  = (dsp_aid <> c_F10);
073700120918
073800120918         // -?Reperimento dati dal file?
073900120920         clear  $FE;
074000120918         if  dsp_aid <> c_F10;
074100120918           clear  k05tntbe01;
074200120918           k_TBEcod = c_Tab;
074300120918           k_TBEke1 = S1TBEke1;
074400120918           k_TBEke2 = S1TBEke2;
074500120918           chain  %kds( k05tntbe01 : 3 )  TNTBE000;
074600120918           if  %found(TNTBE01L);
074700120918             $FE_ds = TBEuni;
074800120918             RecAnnull  = (TBEatb <> *blank);
074900120918           endif;
075000120918         endif;
075100120918
075200120920         // -?Impostazione campi chiave  e?
075300120918         //  ?Caricamento delle filiali di emissione?
075400120918         //  ?(SE NON immissione)?
075500120918         If  dsp_aid <> c_F10;
075600120919
075700120918           // ·?Filiale di Ritiro?
075800120919           C02por = %int( S1TBEke1 );
075900120918           k_ORGfil = C02por;
076000120918           chain  %kds(k01azorg01)  AZORG;
076100120918           if  %found(AZORG01L);
076200120918             C02porD = ORGdes;
076300120918           endif;
076400120919
076500120918           // ·?Filiale di Ritiro Forzata?
076600120919           C02frz = %int( S1TBEke2 );
076700120918           k_ORGfil = C02frz;
076800120918           chain  %kds(k01azorg01)  AZORG;
076900120918           if  %found(AZORG01L);
077000120918             C02frzD = ORGdes;
077100120918           endif;
077200120920
077300120920         EndIf;
077400120919
077500120920         // ·?Filiali di Emissione?
077600120920         For  xx = 1  To  c_MaxFE;
077700120920
077800120920           clear  tbFFRs2;
077900120920
078000120920           if  $FE(xx) <> *blank;
078100120920
078200120920             Sfl2NxtChg = *on;
078300120920             S02poe = $FE(xx);
078400120920             k_ORGfil = %int( S02poe );
078500120920             chain  %kds(k01azorg01)  AZORG;
078600120920
078700120920             if  %found(AZORG01L);
078800120920               if  ORGde5 <> *blank;
078900120920                 S02poeD = ORGde5;
079000120920               else;
079100120920                 S02poeD = ORGdes;
079200120920               endif;
079300120920             endif;
079400120920
079500120920           endif;
079600120920
079700120920           // ·?Scrittura record nel subfile?
079800120920           S02nrr += 1;
079900120920           write  tbFFRs2;
080000120920
080100120920         EndFor;
080200120920
080300120920         // -?Posizionamento cursore sulla Filiale di Ritiro?
080400120920         //  ?SE immissione?
080500120920         PosCurPOR = (dsp_aid = c_F10);
080600120918
080700120918         // -?Visualizzazione del SFL (se ci sono dati)?
080800120918         Sfl2Dsp_N = (S02nrr <= *zero);
080900120918
081000120918         // -?Attivazione del SFLEND?
081100120918         Sfl2End = *on;
081200120918
081300120918         // -?Impaginazione subfile?
081400120918         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
081500120918         if S02nrr > *zero;
081600120918           C2RcdNbr  = 1;
081700120918           C2CsrRrn  = 1;
081800120918         else;
081900120918           clear C2RcdNbr;
082000120918         endif;
082100120918
082200120918         // -?Abilitazione tasti funzionali condizionati?
082300120919         F6Attivo  = ($Immissione  or  S1Copz <> 5);
082400120919         F14Attivo = ($Immissione  or (S1Copz <> 4  and  S1Copz <> 5));
082500120918
082600120918       ENDSR;
082700120918
082800120918       //--------------------------------------------------------------
082900120918       //?Controllo dati nella videata C02/S02.                        ?
083000120918       //--------------------------------------------------------------
083100120919       BEGSR  sr_CtrC02;
083200120917
083300120919         // -?Spegnimento degli indicatori di errore?
083400120919         %subst(IndDspF : 50) = *off;
083500120919
083600120919         // -?Controllo Filiale di Ritiro?
083700120919         // ·?Obbligatoria?
083800120919         if  C02por = *zero;
083900120919           ErrGenerico = *on;
084000120919           ErrMessage  = *on;
084100120919           PosCurPOR   = *on;
084200120919           V1Dmsg = $Msg(03);
084300120919           leavesr;
084400120919         endif;
084500120919         // ·?Errata?
084600120919         k_ORGfil = C02por;
084700120919         chain  %kds(k01azorg01)  AZORG;
084800120919         if  Not %found(AZORG01L)   or  (ORGfag <> 'F'  and
084900120919                                         ORGfag <> 'A');
085000120919           ErrGenerico = *on;
085100120919           ErrMessage  = *on;
085200120919           PosCurPOR   = *on;
085300120919           V1Dmsg = $Msg(03);
085400120919           leavesr;
085500120919         endif;
085600120919         C02porD = ORGdes;
085700120919
085800120919         // -?Controllo Filiale di Ritiro Forzata?
085900120919         // ·?Obbligatoria?
086000120919         if  C02frz = *zero;
086100120919           ErrGenerico = *on;
086200120919           ErrMessage  = *on;
086300120919           PosCurFRZ   = *on;
086400120919           V1Dmsg = $Msg(03);
086500120919           leavesr;
086600120919         endif;
086700120919         // ·?Errata?
086800120919         k_ORGfil = C02frz;
086900120919         chain  %kds(k01azorg01)  AZORG;
087000120919         if  Not %found(AZORG01L)   or  (ORGfag <> 'F'  and
087100120919                                         ORGfag <> 'A');
087200120919           ErrGenerico = *on;
087300120919           ErrMessage  = *on;
087400120919           PosCurFRZ   = *on;
087500120919           V1Dmsg = $Msg(03);
087600120919           leavesr;
087700120919         endif;
087800120919         C02frzD = ORGdes;
087900120919
088000120919         // -?Controllo SE chiave duplicata (SE immissione)?
088100120919         If  $Immissione;
088200120919           k_TBEcod = c_Tab;
088300120919           k_TBEke1 = %editc( C02por : 'X' );
088400120919           k_TBEke2 = %editc( C02frz : 'X' );
088500120919           setll  %kds(k05tntbe01)  TNTBE000;
088600120919           if  %equal(TNTBE01L);
088700120919             ErrGenerico = *on;
088800120919             ErrMessage  = *on;
088900120919             PosCurFRZ   = *on;
089000120919             V1Dmsg = $Msg(04);
089100120919             leavesr;
089200120919           endif;
089300120919         EndIf;
089400120919
089500120919         // -?Controllo Filiali di Emissione nel subfile S02?
089600120919         clear  $FE;
089700120919         readc  tbFFRs2;
089800120919
089900120919         DoW  Not %eof(TNTBFFRD);
090000120919
090100120919           exsr  sr_CtrS02;
090200120919
090300120919           // -?Aggiornamento sfl?
090400120919           select;
090500120919             when  ErrMessage;
090600120919               Sfl2NxtChg = *on;
090700120919               C2CsrRrn  = C2RcdNbr;
090800120919             when  ErrGenerico;
090900120919               C2CsrRrn  = C2RcdNbr;
091000120919           endsl;
091100120919           Sfl2NxtChg = *on;
091200120919           UPDATE  tbFFRs2;
091300120919
091400120919           if  ErrGenerico   or   ErrMessage;
091500120919             leave;
091600120919           endif;
091700120919
091800120919           readc  tbFFRs2;
091900120919
092000120919         EndDo;
092100120919
092200120919       ENDSR;
092300120919
092400120919       //--------------------------------------------------------------
092500120919       //?Controllo dati nel subfile S02.                              ?
092600120919       //--------------------------------------------------------------
092700120919       BEGSR  sr_CtrS02;
092800120919
092900120919         clear  S02poeD;
093000120919
093100120919         If  S02poe <> *blank  and  S02poe <> *zero  and
093200120919             S02poe <> '999';
093300120919
093400120919           // ·?Filiale di Emissione NON numerica?
093500120919           if  %check( c_Digits : S02poe ) > *zero;
093600120919             ErrGenerico = *on;
093700120919             ErrMessage  = *on;
093800120919             PosCurPOE   = *on;
093900120919             V1Dmsg = $Msg(05);
094000120919             leavesr;
094100120919           endif;
094200120919
094300120919           k_ORGfil = %int(S02poe);
094400120919           chain  %kds(k01azorg01)  AZORG;
094500120919           // ·?Filiale di Emissione errata?
094600120919           if  Not %found(AZORG01L)   or  (ORGfag <> 'F'  and
094700120919                                           ORGfag <> 'A');
094800120919             ErrGenerico = *on;
094900120919             ErrMessage  = *on;
095000120919             PosCurPOE   = *on;
095100120919             V1Dmsg = $Msg(05);
095200120919             leavesr;
095300120919           endif;
095400120919
095500120919           if  ORGde5 <> *blank;
095600120919             S02poeD = ORGde5;
095700120919           else;
095800120919             S02poeD = ORGdes;
095900120919           endif;
096000120919
096100120919         EndIf;
096200120919
096300120919         if  S02poe = *zero;
096400120919           clear  S02poe;
096500120919         endif;
096600120919
096700120919         If  S02poe <> *blank;
096800120919
096900120919           // ·?Filiale di Emissione ripetuta nel subfile?
097000120919           if  %lookup( S02poe : $FE ) > *zero;
097100120919             ErrGenerico = *on;
097200120919             ErrMessage  = *on;
097300120919             PosCurPOE   = *on;
097400120919             V1Dmsg = $Msg(06);
097500120919             leavesr;
097600120919           endif;
097700120919
097800120919           // ·?Memorizzazione Filiale di Emissione in chiera?
097900120919           xx = %lookup( *blank : $FE);
098000120919           if  xx > *zero;
098100120919             $FE(xx) = S02poe;
098200120919           endif;
098300120919
098400120919         EndIf;
098500120919
098600120918
098700120918       ENDSR;
098800120919
098900120919       //--------------------------------------------------------------
099000120919       //?Gestione window W02.                                         ?
099100120919       //--------------------------------------------------------------
099200120919       BEGSR  sr_GesW02;
099300120919
099400120919         // -?Inizializzazione Window?
099500120919         if  $InzW02 = *on;
099600120919           clear  W02tfp;
099700120919           clear  W02tfpD;
099800120919           $InzW02 = *off;
099900120919         endif;
100000120919
100100120919         // -?Emissione Window?
100200120919         exfmt  tbFFRw2;
100300120919
100400120919         reset  ErrGenerico;
100500120919         reset  ErrMessage;
100600120919         clear  V1Dmsg;
100700120919
100800120919         Select;
100900120919
101000120919           // -?F12=Ritorno?
101100120919           When  dsp_aid = c_F12;
101200120919             $Video = 'S2';
101300120919
101400120919           // -?Invio; F6=Conferma?
101500120919           Other;
101600120919             exsr  sr_CtrW02;
101700120919             if  ErrGenerico;
101800120919               leavesr;
101900120919             endif;
102000120919             if  dsp_aid <> c_F06;
102100120919               leavesr;
102200120919             endif;
102300120919
102400120919         EndSl;
102500120919
102600120919         // -?Impostazione filiali?
102700120919         exsr  sr_AggiorS02;
102800120919
102900120919         // -?Ritorno?
103000120919         $Video = 'S2';
103100120919
103200120919       ENDSR;
103300120919
103400120919       //--------------------------------------------------------------
103500120919       //?Controllo dati nella videata W02.                            ?
103600120919       //--------------------------------------------------------------
103700120919       BEGSR  sr_CtrW02;
103800120919
103900120919         clear  W02tfpD;
104000120919
104100120919         // -?Ricerca?
104200120919         if  %scan( '?' : W02tfp ) > *zero;
104300120919           clear  SD19cod;
104400120919           clear  SD19tip;
104500120919           clear  SD19des;
104600120919           tnsd19r( SD19cod : SD19tip : SD19des );
104700120919           W02tfp  = SD19cod;
104800120919           W02tfpD = SD19des;
104900120919           ErrGenerico = *on;
105000120919           leavesr;
105100120919         endif;
105200120919
105300120919         // -?Obbligatorio?
105400120919         if  W02tfp = *blank  or  W02tfp = *zero;
105500120919           ErrGenerico = *on;
105600120919           ErrMessage  = *on;
105700120919           V1Dmsg = $Msg(07);
105800120919           leavesr;
105900120919         endif;
106000120919
106100120919         // -?Errato?
106200120919         if  %check( c_Digits : W02tfp ) > *zero;
106300120919           ErrGenerico = *on;
106400120919           ErrMessage  = *on;
106500120919           V1Dmsg = $Msg(07);
106600120919           leavesr;
106700120919         endif;
106800120919
106900120919         k_ORGfil = %int( W02tfp );
107000120919         chain  %kds(k01azorg01)  AZORG;
107100120919         if  Not %found(AZORG01L)  or  ORGfva <> *blank  or
107200120919             (ORGfag <> 'A'       and  ORGfag <> 'F');
107300120919           ErrGenerico = *on;
107400120919           ErrMessage  = *on;
107500120919           V1Dmsg = $Msg(07);
107600120919           leavesr;
107700120919         endif;
107800120919
107900120919         W02tfpD = ORGdes;
108000120919
108100120919         // -?Reperimento elenco delle filiali?
108200120919         //  ?(deve essere un Terminal di Partenza)?
108300120919         reset  TRUL06ds;
108400120919         //D06cod = '£1';     ?(già così)?
108500120919         //D06tla = 'L';      ?(già così)?
108600120919         D06key = W02tfp;
108700120919         kpjbu  = TRUL06ds;
108800120919         TRUL06R( kpjba );
108900120919         TRUL06ds = kpjbu;
109000120919         if  D06err <> *blank;
109100120919           ErrGenerico = *on;
109200120919           ErrMessage  = *on;
109300120919           V1Dmsg = $Msg(07);
109400120919           leavesr;
109500120919         endif;
109600120919
109700120919       ENDSR;
109800120919
109900120919       //--------------------------------------------------------------
110000120919       //?Aggiunta delle filiali dalla schiera D06LIN nel subfile S02. ?
110100120919       //--------------------------------------------------------------
110200120919       BEGSR  sr_AggiorS02;
110300120919
110400120919         S02nrr = 1;
110500120919         xx     = 1;
110600120919
110700120919         chain  S02nrr  tbFFRs2;
110800120919
110900120919         DoW  D06lin(xx) > *zero  and  S02nrr <= c_MaxFE;
111000120919
111100120919           // -?Elemento da caricare già presente nel sfl?
111200120919           if  %lookup( %editc( D06lin(xx) : 'X' ) : $FE ) > *zero;
111300120919              xx += 1;
111400120919              iter;
111500120919           endif;
111600120919
111700120919           // -?Aggiunta della filiale nel subfile?
111800120919           if  S02poe = *blank  or  S02poe = *zero;
111900120919             S02poe = %editc( D06lin(xx) : 'X' );
112000120919             xx += 1;
112100120919           endif;
112200120919
112300120919           // -?Aggiornamento subfile?
112400120919           Sfl2NxtChg = *on;
112500120919           UPDATE  tbFFRs2;
112600120919
112700120919           S02nrr += 1;
112800120919           chain  S02nrr  tbFFRs2;
112900120919
113000120919         EndDo;
113100120919
113200120919         // -?NON caricate tutte le Filiali di Emissione per mancanza?
113300120919         //  ?di spazio?
113400120919         if  D06lin(xx) > *zero;
113500120919           ErrGenerico = *on;
113600120919           ErrMessage  = *on;
113700120919           V1Dmsg = $Msg(08);
113800120919           leavesr;
113900120919         endif;
114000120919
114100120919       ENDSR;
114200120918
114300120918       //--------------------------------------------------------------
114400120918       //?Gestione videata W01 (dati relativi alla trasmissione).      ?
114500120918       //--------------------------------------------------------------
114600120918       BEGSR  sr_GesW01;
114700120918
114800120918         // -?Inizializzazione videata?
114900120918         if  $InzW01 = *on;
115000120918           exsr  sr_InzW01;
115100120918           $InzW01 = *off;
115200120918         endif;
115300120918
115400120918         // -?Emissione window?
115500120918         IF  TBXftt = 'S';
115600120918
115700120918           exfmt  tbFFRw1;
115800120918
115900120918           reset  ErrGenerico;
116000120918           reset  ErrMessage;
116100120918           clear  V1Dmsg;
116200120918
116300120918           Select;
116400120918
116500120918             // -?F12=Ritorno?
116600120918             When  dsp_aid = c_F12;
116700120918               $Video = 'S2';
116800120918               leavesr;
116900120918
117000120918             // -?Invio?
117100120918             When  dsp_aid <> c_F06;
117200120918               leavesr;
117300120918
117400120918           EndSl;
117500120918
117600120918         ENDIF;
117700120918
117800120918         // -?F6=Aggiornamento TNTBE00F?
117900120918         exsr  sr_UpdTNTBE;
118000120918
118100120918         If  ErrGenerico = *on;
118200120918           // -?Rilevato errore in fase di aggiornamento?
118300120918           leavesr;
118400120918         Else;
118500120918           // -?Videata iniziale altrimenti?
118600120918           $Video  = 'S1';
118700120918           $InzS01 = $Immissione;
118800120918           if  not  $Immissione;
118900120918             if  TBEatb  = 'A';
119000120919               S1TBEann = 'Ann';
119100120919             else;
119200120919               clear  S1TBEann;
119300120918             endif;
119400120918             S01FEx_ds = TBEuni;
119500120918             if  %subst( TBEuni : 43 : 3 ) <> *blank;
119600120918               S01piu = '+';
119700120918             endif;
119800120918           endif;
119900120918         EndIf;
120000120918
120100120918       ENDSR;
120200120918
120300120918       //--------------------------------------------------------------
120400120918       //?Inizializzazione videata W01                                 ?
120500120918       //--------------------------------------------------------------
120600120918       BEGSR  sr_InzW01;
120700120918
120800120918         clear tbFFRW1;
120900120918
121000120918         select;
121100120918           // -?Opz.4=Ripristino?
121200120918           when  S1Copz = 4  and  %found(TNTBE01L)  and  TBEatb = 'A';
121300120918             W1ftt = TBEftt;
121400120918           // -?Opz.4=Annullamento?
121500120918           when  S1Copz = 4  and  %found(TNTBE01L)  and  TBEatb = *blank;
121600120918             W1ftt = TBEftt;
121700120918           // -?Opz.2=Modifica?
121800120918           when  S1Copz = 2  and  %found(TNTBE01L);
121900120918             W1ftt = TBEftt;
122000120918           // -?F10=Immissione?
122100120918           when  $Immissione  and  NOT %found(TNTBE01L);
122200120918             W1ftt = TBXftt;
122300120918         endsl;
122400120918
122500120918         // -?Se NON immissione:?
122600120918         //  ?visualizza i dati relativi all'ultima trasmissione?
122700120918         if  %found(TNTBE01L);
122800120918           W1flt  = TBEflt;
122900120918           W1ftr  = TBEftr;
123000120918           if  TBEdtr <> *zero;
123100120919             wData_Eur = %date( TBEdtr : *iso );
123200120919             W1dtr     = %dec( wData_Eur );
123300120918           endif;
123400120918         endif;
123500120918
123600120918       ENDSR;
123700120918
123800120918       //--------------------------------------------------------------
123900120918       //?Aggiornamento record TNTBE00F (tab. "FFR")                   ?
124000120918       //--------------------------------------------------------------
124100120918       BEGSR  sr_UpdTNTBE;
124200120918
124300120918         // -?Impostazione dei campi chiave SE immissione?
124400120918         if  $Immissione;
124500120918           k_TBEcod = c_Tab;
124600120918           k_TBEke1 = %editc( C02por : 'X' );
124700120918           k_TBEke2 = %editc( C02frz : 'X' );
124800120918           //k_TBElin = TBXlin;
124900120918           //k_TBEsif = TBXsif;
125000120918           clear  k_TBElin;
125100120918           clear  k_TBEsif;
125200120918           chain  %kds(k05tntbe01)  TNTBE000;
125300120918         endif;
125400120918
125500120918         // -?Impostazione dati (se NON annullamento)?
125600120918         if  S1Copz <> 4;
125700120918           TBEapl = TBXapl;
125800120918           TBEuni = $FE_ds;
125900120918         endif;
126000120918
126100120918         TBEftt = W1ftt;
126200120918         TBEflt = W1flt;
126300120918         clear TBEftr;
126400120918         //clear TBEdtr;
126500120918
126600120918         select;
126700120918           // -?Opz.4=Ripristino?
126800120918           when  S1Copz = 4  and  TBEatb <> *blank;
126900120918             clear  TBEatb;
127000120918           // -?Opz.4=Annullamento?
127100120918           when  S1Copz = 4  and  TBEatb = *blank;
127200120918             TBEatb = 'A';
127300120918         endsl;
127400120918
127500120918         IF  NOT  %found(TNTBE01L);
127600120918           clear  TBEsif;
127700120918           clear  TBElin;
127800120918           TBEcod = c_Tab;
127900120918           TBEke1 = %editc( C02por : 'X' );
128000120918           TBEke2 = %editc( C02frz : 'X' );
128100120918           clear  TBEdtr;
128200120918           //_________________
128300120918           WRITE(e)  TNTBE000;
128400120918           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
128500120918           if  %error;
128600120918             ErrGenerico = *on;
128700120918             ErrMessage  = *on;
128800120918             PosCurPOR   = *on;
128900120918             V1Dmsg = $Msg(06);
129000120918             leavesr;
129100120918           endif;
129200120918         ELSE;
129300120918           //_______________
129400120918           UPDATE  TNTBE000;
129500120918           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
129600120918         ENDIF;
129700120918
129800120918       ENDSR;
129900120917
130000120918       //--------------------------------------------------------------
130100120918       //?Operazioni finali.                                           ?
130200120918       //--------------------------------------------------------------
130300120918       BEGSR  sr_RoutEnd;
130400120918
130500120918         //kpjbu = TNTBFFRds;
130600120918
130700120918         return;
130800120918
130900120918       ENDSR;
131000120917
131100120917      /end-free
131200120917
131300120918** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
131400120918Opzione errata                                                                  1
131500120918Record annullato: usare l'opzione 4 per ripristinarlo e modificarlo             2
131600120919Filiale di Ritiro mancante o errata                                             3
131700120919Chiave già presente in tabella                                                  4
131800120919Filiale di Emissione mancante o errata                                          5
131900120919Filiale di Emissione già indicata                                               6
132000120919Terminal di Partenza mancante o errato                                          7
132100120919Attenzione: NON inserite tutte le linee per mancanza di spazio                  8
