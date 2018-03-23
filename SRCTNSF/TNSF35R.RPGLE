000100170913       //==============================================================
000200170914       // ?e-Mail per la Trasmissione in DOCFLOW dei Flussi di         ?
000300170914       // ?   Fatturazione (e Trasmissione...).                        ?
000400170911       //==============================================================
000500170911
000600170911       //--------------------------------------------------------------
000700170913       // ?Parametri di compilazione (vedi *cmd UBCRTOBJ).             ?
000800170911       //--------------------------------------------------------------
000900170911
001000170911     /*END
001100170911
001200170911       //--------------------------------------------------------------
001300170913       // ?Specifiche di controllo.                                    ?
001400170911       //--------------------------------------------------------------
001500170911
001600170911     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170911     h dftactgrp(*no)
001800170911
001900170911       //--------------------------------------------------------------
002000170913       // ?Dichiarazione file.                                         ?
002100170911       //--------------------------------------------------------------
002200170911
002300170913       // - ?Video?
002400170911     fTNSF35D   cf   e             workstn
002500170911     f                                     indds( IndDspF )
002600170920     f                                     infds( InfDspF )
002700170920
002800170920       // - ?Stampa da inviare come e-mail?
002900170920     fPrtEMAIL  o    f  198        printer  oflind(*inOF)  usropn
003000170911
003100170911       //--------------------------------------------------------------
003200170913       // ?Definizione costanti.                                       ?
003300170920       //--------------------------------------------------------------
003400170920
003500170920       // - ?Dati di default?
003600170920     d c_Oggetto       c                   const('TRASMISSIONE FLUSSI +
003700170920     d                                            FATTURAZIONE')
003800170920     d c_Sede          c                   const('046')
003900170920
004000170920       // - ?Comando di override al PrtF?
004100170920     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004200170920     d                                           file(PRTEMAIL) +
004300170920     d                                           pagesize(66 198) +
004400170920     d                                           lpi(6) cpi(12) +
004500170920     d                                           ovrscope(*actgrpdfn) +
004600170920     d                                           ')
004700170920     d c_CmdDltOvr     c                   const('DLTOVR +
004800170920     d                                            file(PRTEMAIL) +
004900170920     d                                            lvl(*actgrpdfn)')
005000170920
005100170913       // - ?Tasti funzionali a video?
005200170911     d c_F01           c                   const(x'31')
005300170911     d c_F02           c                   const(x'32')
005400170911     d c_F03           c                   const(x'33')
005500170911     d c_F04           c                   const(x'34')
005600170911     d c_F05           c                   const(x'35')
005700170911     d c_F06           c                   const(x'36')
005800170911     d c_F07           c                   const(x'37')
005900170911     d c_F08           c                   const(x'38')
006000170911     d c_F09           c                   const(x'39')
006100170911     d c_F10           c                   const(x'3A')
006200170911     d c_F11           c                   const(x'3B')
006300170911     d c_F12           c                   const(x'3C')
006400170911     d c_F13           c                   const(x'B1')
006500170911     d c_F14           c                   const(x'B2')
006600170911     d c_F15           c                   const(x'B3')
006700170911     d c_F16           c                   const(x'B4')
006800170911     d c_F17           c                   const(x'B5')
006900170911     d c_F18           c                   const(x'B6')
007000170911     d c_F19           c                   const(x'B7')
007100170911     d c_F20           c                   const(x'B8')
007200170911     d c_F21           c                   const(x'B9')
007300170911     d c_F22           c                   const(x'BA')
007400170911     d c_F23           c                   const(x'BB')
007500170911     d c_F24           c                   const(x'BC')
007600170911     d c_Enter         c                   const(x'F1')
007700170911     d c_RollDown      c                   const(x'F4')
007800170911     d c_RollUp        c                   const(x'F5')
007900170911
008000170911       //--------------------------------------------------------------
008100170913       // ?Definizione schiere.                                        ?
008200170911       //--------------------------------------------------------------
008300170911
008400170913       // - ?Messaggi di errore?
008500170913     d sk_Msg          s             78    dim( 8)  ctdata  perrcd( 1)
008600170911
008700170911       //--------------------------------------------------------------
008800170913       // ?Definizione aree dati.                                      ?
008900170911       //--------------------------------------------------------------
009000170911
009100170913       // - ?Dati utente?
009200170911     d §AzUte        e ds                  extname(AZUTE00F)
009300170911     d                                     dtaara
009400170911     d §DatiUte      e ds                  extname(dDatiUte)
009500170911     d                                     dtaara
009600170911
009700170911       //--------------------------------------------------------------
009800170913       // ?Definizione strutture dati.                                 ?
009900170911       //--------------------------------------------------------------
010000170911
010100170913       // - ?Status ds?
010200170911     d Status         sds
010300170911     d   SDSpgmName      *proc
010400170913     d   SDSparms        *parms
010500170913     d   SDSdata             191    198
010600170913     d   SDSjobName          244    253
010700170913     d   SDSuser             254    263
010800170913     d   SDSjobNumber        264    269s 0
010900170911
011000170913       // - ?InfDS?
011100170911     d InfDspF         ds
011200170911     d   dsp_aid             369    369a
011300170911     d*//sfl_rrn             376    377i 0
011400170911     d*//min_nrr             378    379i 0
011500170911     d*//num_rcds            380    381i 0
011600170911
011700170913       // - ?IndDS?
011800170913     d IndDspF         ds                  inz
011900170913         // ·?Emissione messaggio di errore?
012000170913     d   $AbilitF6                     n   overlay( IndDspF : 06 )
012100170913         // ·?Emissione messaggio di errore?
012200170911     d   $ErrMessage                   n   overlay( IndDspF : 28 )
012300170913         // ·?Posizionamento cursore?
012400170911     d   $PosCurFATA                   n   overlay( IndDspF : 51 )
012500170911     d   $PosCurFATD                   n   overlay( IndDspF : 52 )
012600170911     d   $PosCurFAT9                   n   overlay( IndDspF : 53 )
012700170911     d   $PosCurDTFT                   n   overlay( IndDspF : 54 )
012800170911     d   $PosCurHMPF                   n   overlay( IndDspF : 55 )
012900170911     d   $PosCurEMIT                   n   overlay( IndDspF : 56 )
013000170911     d   $PosCurEDES                   n   overlay( IndDspF : 57 )
013100170918     d*//$PosCurEDCC                   n   overlay( IndDspF : 58 )
013200170913         // ·?Riemissione videata?
013300170911     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
013400170911
013500170911       // - Orari a video
013600170912     d wHhMm_ds        ds             4    inz   qualified
013700170911     d   wHH                          2s 0 inz
013800170911     d   wMM                          2s 0 inz
013900170911
014000170913       // - ?Parametri ricevuti?
014100170911     d KPJBA         e ds
014200170918
014300170918       // - ?Tabella "MRA" = Bart-Mailing - Danni?
014400170918     d dMRAdan       e ds                  inz   qualified
014500170920
014600170920       // - ?Parametri x Ridefinizione dati utente estesi per mailing PDF?
014700170920     d TRTCM1ds      e ds                  inz
014800170920       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
014900170920     d   §CM1mitt    e                     inz('ced')
015000170920       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
015100170920     d   §CM1dst     e                     inz('ced@brt.it')
015200170920       //  ?·§CM1tips = Tipo lettera via e-mail:?
015300170920       //   ?"LET" = testo allegato in corpo con logo?
015400170920       //           ?(richiede righe libere iniziali per il logo)?
015500170920       //   ?"COR" = testo integrato senza logo?
015600170920       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
015700170920     d   §CM1tips    e                     inz('COR')
015800170920       //  ?·§CM1po   = Filiale?
015900170920     d   §CM1po      e                     inz('046')
016000170920       //  ?·§CM1var  = Oggetto e-mail?
016100170920     d   §CM1var     e                     inz('*OBJM*+
016200170920     d                                     TRASMISSIONE FLUSSI FATTURA+
016300170920     d                                     ZIONE')
016400170920       //  ?·§CM1sts  = Stato?
016500170920     d   §CM1sts     e                     inz(*off)
016600170920       //  ?·§CM1sts  = Id processo?
016700170920     d   §CM1idp     e                     inz('*')
016800170920
016900170911       //--------------------------------------------------------------
017000170913       // ?Definizione variabili globali.                              ?
017100170911       //--------------------------------------------------------------
017200170911
017300170913       // - ?Flags booleani?
017400170911     d $Fine           s               n   inz
017500170911     d $InzD01         s               n   inz(*on)
017600170911     d $Avviso         s               n   inz
017700170911     d $Avviso2        s               n   inz
017800170920     d*//· $Invio          s               n   inz
017900170920     d $First          s               n   inz
018000170911
018100170913       // - ?Indici di schiera?
018200170911     d xx              s              3  0 inz
018300170911
018400170913       // - ?Variabili per la gestione del video?
018500170911     d $Video          s              2    inz('D1')
018600170911
018700170920       // - ?Campi di stampa?
018800170920     d wEMLtxt         s            198    inz
018900170920
019000170920       // - ?Variabili di comodo?
019100170912     d wTime           s               t   inz   timfmt(*hms)
019200170913     d wMinutes        s              2  0 inz
019300170912     d wEMLindO        s                   inz   like(§EMLindO)
019400170911
019500170911       //--------------------------------------------------------------
019600170918       // ?Definizione prototipi.                                      ?
019700170911       //--------------------------------------------------------------
019800170911
019900170913       // - ?Controllo/Inversione Date?
020000170911     d WLBdat          ds                  inz
020100170911     d   G08dat                       8  0 inz
020200170911     d   G08inv                       8  0 inz
020300170911     d   G08err                       1    inz('3')
020400170911     d   G08tgi                       5  0 inz
020500170911      /copy gaitrasrc/srcProtoPR,XSRDA8
020600170918
020700170918       // - ?Gestione tabelle: controllo e ricerca?
020800170918     d TIBS02ds      e ds                  inz
020900170918     d   T02tla      e                     inz('L')
021000170918     d   T02mod      e                     inz('C')
021100170918     d   T02cod      e                     inz('MRA')
021200170918     d   T02ke2      e                     inz('DAN')
021300170918      /copy gaitrasrc/srcProtoPr,TIBS02R
021400170911
021500170913       // - ?Reperimento dati utente?
021600170911     d TIBS34ds      e ds                  inz
021700170911      /copy gaitrasrc/srcProtoPR,TIBS34R
021800170913
021900170913       // - ?Controllo indirizzo e-mail?
022000170913     d dsEMAIL       e ds                  inz
022100170913      /copy gaitrasrc/srcProtoPR,XEMAIL
022200170920
022300170920       // - ?API QCAPCMD (Process Commands)?
022400170920     d Qcmd            s           2048    inz  varying
022500170920      /copy qSysInc/qRpgleSrc,QCAPCMD
022600170920      /copy gaitrasrc/srcProtoPR,QCAPCMD
022700170920       // - ?Parametri gestione errori API.?
022800170920      /copy qsysinc/qRpgleSrc,QUSEC
022900170920
023000170911       //--------------------------------------------------------------
023100170913       // ?Definizione key-list.                                       ?
023200170911       //--------------------------------------------------------------
023300170911
023400170911
023500170911       //--------------------------------------------------------------
023600170913       // ?Riepilogo indicatori utilizzati.                            ?
023700170911       //--------------------------------------------------------------
023800170911
023900170911
024000170911       //--------------------------------------------------------------
024100170913       // ?M A I N - L I N E                                           ?
024200170911       //--------------------------------------------------------------
024300170911
024400170911     c     *Entry        plist
024500170911     c                   parm                    KPJBA
024600170911
024700170911      /free
024800170911
024900170913       // - ?Operazioni iniziali?
025000170911       exsr  sr_RoutInz;
025100170911
025200170913       // - ?Gestione videate?
025300170911       DoW  $Fine = *off;
025400170911
025500170911         Select;
025600170911
025700170913           // - ?Gestione videata D1?
025800170911           When $Video = 'D1';
025900170911             exsr  sr_GesD01;
026000170911
026100170913           // - ?? ? ? ? ??
026200170911           Other;
026300170911             $Fine = *on;
026400170911
026500170911         EndSl;
026600170911
026700170911       EndDo;
026800170911
026900170913       // - ?Operazioni finali?
027000170911       exsr sr_RoutEnd;
027100170911
027200170911       //--------------------------------------------------------------
027300170913       // ?Operazioni iniziali.                                        ?
027400170911       //--------------------------------------------------------------
027500170911       BEGSR  sr_RoutInz;
027600170911
027700170913         // - ?Impostazione chiusura?
027800170911         *inLR = *on;
027900170911
028000170913         // - ?Reperimento dati job?
028100170911         exsr  sr_DatiJob;
028200170911
028300170913         // - ?Impostazione nome programma a video?
028400170918         VT1pgm = SDSpgmName;
028500170918
028600170918         // -?Reperimento tab. "MRA"?
028700170918         clear  dMRAdan;
028800170918         clear  kPJbU;
028900170918         reset  TIBS02ds;
029000170920         T02ke1 = SDSpgmName;
029100170918         TNTBE_RicercaControllo ( kpjba : tibs02ds );
029200170918         if  T02err = *blank;
029300170918           dMRAdan = T02uni;
029400170918         endif;
029500170920
029600170911       ENDSR;
029700170911
029800170911       //--------------------------------------------------------------
029900170913       // ?Reperimento Dati del job (Utente/Operativi).                ?
030000170911       //--------------------------------------------------------------
030100170911       BEGSR  sr_DatiJob;
030200170911
030300170911         in(e) §AzUte;
030400170911         if NOT %error;
030500170911           in(e) §DatiUte;
030600170911         endif;
030700170911         if %error or RSut = *blank;
030800170911           tibs34r ( tibs34ds );
030900170911           in §AzUte;
031000170911           in §DatiUte;
031100170911         endif;
031200170911
031300170911       ENDSR;
031400170911
031500170911       //--------------------------------------------------------------
031600170913       // ?Gestione videata D01.                                       ?
031700170911       //--------------------------------------------------------------
031800170911       BEGSR  sr_GesD01;
031900170911
032000170913         // - ?Inizializzazione videata?
032100170911         if  $InzD01 = *on  and  Not $errGenerico;
032200170911           exsr  sr_InzD01;
032300170911           $InzD01 = *off;
032400170911         endif;
032500170913
032600170913         // - ?(Dis)Abilitazione tasti funzionali?
032700170913         $AbilitF6   = *on;
032800170911
032900170913         // - ?Emissione Testata e Piede?
033000170911         write  SF35T01;
033100170911         write  SF35P01;
033200170911
033300170913         // - ?Emissione videata di richiesta parametri?
033400170911         exfmt  SF35D01;
033500170911
033600170911         clear  VIDmsg;
033700170911         reset  $ErrMessage;
033800170911         reset  $ErrGenerico;
033900170911
034000170911         Select;
034100170911
034200170913           // - ?Sistema Informativo di Filiale => Uscita forzata?
034300170911           When  DUTlpo <> 'S';
034400170911             $Fine = *on;
034500170911
034600170913           // - ?F3=Fine?
034700170911           When  dsp_aid = c_F03;
034800170911             $Fine = *on;
034900170911
035000170913           // - ?Invio / F6 = Conferma?
035100170911           Other;
035200170911             exsr  sr_CtrD01;
035300170911             if  Not $ErrGenerico  and  dsp_aid = c_F06;
035400170911               exsr  sr_F06D01;
035500170911             endif;
035600170911
035700170911         EndSl;
035800170911
035900170911       ENDSR;
036000170911
036100170911       //--------------------------------------------------------------
036200170913       // ?Inizializzazione videata D01.                               ?
036300170911       //--------------------------------------------------------------
036400170911       BEGSR  sr_InzD01;
036500170911
036600170913         // - ?Pulizia videata?
036700170911         clear  SF35D01;
036800170911
036900170913         // - ?Impostazione campi a video?
037000170913         reset  WLBdat;
037100170920         G08inv = %dec( %date() );
037200170913         xsrDA8( WLBdat );
037300170913         V1DtFt = G08dat;
037400170912
037500170914         // · ?Orario proposto a 15 minuti dopo il *Time,?
037600170914         //   ?arrotondato ai 15 minuti successivi?
037700170918         exsr  sr_SetTime;
037800170913         V1hmPF   = ( %int( %subdt( wTime : *h ) ) * 100 ) +
037900170913                      %int( %subdt( wTime : *mn ) );
038000170914
038100170918         //*·V1emMit  = 'ced@brt.it';
038200170918         V1emMit  = %trim( dMRAdan.§MRADmitt );
038300170918
038400170918         //*·V1emDes  = 'service.admin@service.docflow.it';
038500170918         //*·V1emDCC  = 'Roberto.Dragone@docflow.it;+
038600170918         //*·            Cedreti@brt.it;+
038700170918         //*·            Giovanni.Pesce@brt.it;+
038800170918         //*·            ced@brt.it';
038900170918         //*·V1emDes  = 'service.admin@service.docflow.it;+
039000170918         //*·            Roberto.Dragone@docflow.it;+
039100170918         //*·            Cedreti@brt.it;+
039200170918         //*·            Giovanni.Pesce@brt.it;+
039300170918         //*·            ced@brt.it';
039400170918         V1emDes  = dMRAdan.§MRADdest;
039500170911
039600170911         reset  $Avviso;
039700170913         reset  $Avviso2;
039800170911
039900170913         // - ?Controllo del sistema informativo in cui si sta lavorando?
040000170913         //   ?(DEVE essere di SEDE)?
040100170911         if  DUTlpo <> 'S';
040200170911           $ErrGenerico = *on;
040300170911           $ErrMessage  = *on;
040400170911           VIDmsg = sk_Msg(01);
040500170911           leavesr;
040600170911         endif;
040700170911
040800170911       ENDSR;
040900170918
041000170918       //--------------------------------------------------------------
041100170918       // ?Impostazione Orario di Presenza Flussi.                     ?
041200170918       //--------------------------------------------------------------
041300170918       BEGSR  sr_SetTime;
041400170918
041500170918         wTime    = %time();
041600170918         wMinutes = %int( %subdt( wTime : *mn ) );
041700170918         wTime   -= %minutes( wMinutes );
041800170918         wTime   += %minutes( %div( wMinutes : 15 ) * 15 );
041900170918         if  %rem( wMinutes : 15 ) > *zero;
042000170918           wTime += %minutes(30);
042100170918         else;
042200170918           wTime += %minutes(15);
042300170918         endif;
042400170918         wMinutes = %int( %subdt( wTime : *mn ) );
042500170918
042600170918       ENDSR;
042700170911
042800170911       //--------------------------------------------------------------
042900170913       // ?Controllo dati nella videata D01.                           ?
043000170911       //--------------------------------------------------------------
043100170911       BEGSR  sr_CtrD01;
043200170911
043300170913         // - ?Spegnimento degli indicatori di errore?
043400170911         %subst( IndDspF : 28 ) = *off;
043500170911
043600170913         // - ?Controllo selezione di almeno uno spool?
043700170911         if  V1fatA = *blank  and
043800170911             V1fatD = *blank  and
043900170913             V1fat9 = *blank;
044000170911           $ErrGenerico = *on;
044100170911           $ErrMessage  = *on;
044200170913           $PosCurFATA  = *on;
044300170911           VIDmsg = sk_Msg(02);
044400170911           leavesr;
044500170911         endif;
044600170911
044700170911         select;
044800170913           // - ?Controllo selezione spool "FATA"?
044900170911           when  V1fatA <> *blank  and
045000170911                 V1fatA <> 'X';
045100170911             $ErrGenerico = *on;
045200170911             $ErrMessage  = *on;
045300170913             $PosCurFATA  = *on;
045400170911             VIDmsg = sk_Msg(03);
045500170911             leavesr;
045600170913           // - ?Controllo selezione spool "FATD"?
045700170911           when  V1fatD <> *blank  and
045800170911                 V1fatD <> 'X';
045900170911             $ErrGenerico = *on;
046000170911             $ErrMessage  = *on;
046100170913             $PosCurFATD  = *on;
046200170911             VIDmsg = sk_Msg(03);
046300170911             leavesr;
046400170913           // - ?Controllo selezione spool "FATX"?
046500170911           when  V1fat9 <> *blank  and
046600170911                 V1fat9 <> 'X';
046700170911             $ErrGenerico = *on;
046800170911             $ErrMessage  = *on;
046900170913             $PosCurFAT9  = *on;
047000170911             VIDmsg = sk_Msg(03);
047100170911             leavesr;
047200170911         endsl;
047300170911
047400170913         // - ?Controllo Data Fatturazione?
047500170918         if  V1dtFt = *zero;
047600170918           reset  WLBdat;
047700170920           G08inv = %dec( %date() );
047800170918           xsrDA8( WLBdat );
047900170918           V1dtFt = G08dat;
048000170918           $ErrGenerico = *on;
048100170918         endif;
048200170911         clear  WLBdat;
048300170918         G08dat = V1dtFt;
048400170911         xsrDA8( WLBdat );
048500170911         if  G08err = *on;
048600170911           $ErrGenerico = *on;
048700170911           $ErrMessage  = *on;
048800170911           $PosCurDTFT  = *on;
048900170911           VIDmsg = sk_Msg(04);
049000170911           leavesr;
049100170911         endif;
049200170918         V1dtFt = G08dat;
049300170911
049400170913         // - ?Controllo ora presenza flussi?
049500170918         // · ?Orario proposto a 15 minuti dopo il *Time,?
049600170918         //   ?arrotondato ai 15 minuti successivi?
049700170918         if  V1hmPF = *zero;
049800170918           exsr  sr_SetTime;
049900170918           V1hmPF   = ( %int( %subdt( wTime : *h ) ) * 100 ) +
050000170918                        %int( %subdt( wTime : *mn ) );
050100170918           $ErrGenerico = *on;
050200170918         endif;
050300170912         wHhMm_ds = %editc( V1HmPf : 'X' );
050400170912         if  wHhMm_ds.wHH < 00  or  wHhMm_ds.wHH > 23  or
050500170912             wHhMm_ds.wMM < 00  or  wHhMm_ds.wMM > 59;
050600170913           $ErrGenerico = *on;
050700170913           $ErrMessage  = *on;
050800170913           $PosCurHMPF  = *on;
050900170912           VIDmsg = sk_Msg(05);
051000170912           leavesr;
051100170912         endif;
051200170911
051300170913         // - ?Controllo indirizzo e-mail Mittente?
051400170911         if  V1emMit = *blank;
051500170911           $ErrGenerico = *on;
051600170911           $ErrMessage  = *on;
051700170911           $PosCurEMIT  = *on;
051800170911           VIDmsg = sk_Msg(06);
051900170911           leavesr;
052000170911         endif;
052100170911
052200170911         clear  xx;
052300170911         clear  dsEMAIL;
052400170911         clear  wEMLindO;
052500170918         §EMLindI = %trim( V1emMit ) + '@brt.it';
052600170911         xemail ( dsEMAIL );
052700170911         if  §EMLerrO <> *off;
052800170913           $ErrGenerico = *on;
052900170913           $ErrMessage  = *on;
053000170913           $PosCurEMIT  = *on;
053100170911           VIDmsg = §EMLmsgO;
053200170911           leavesr;
053300170911         endif;
053400170918         //*·V1emMit  = §EMLindO;
053500170911
053600170913         // - ?Controllo indirizzo e-mail Destinatari?
053700170911         if  V1emDes = *blank;
053800170911           $ErrGenerico = *on;
053900170911           $ErrMessage  = *on;
054000170911           $PosCurEDES  = *on;
054100170911           VIDmsg = sk_Msg(06);
054200170911           leavesr;
054300170911         endif;
054400170911
054500170911         clear  xx;
054600170911         clear  dsEMAIL;
054700170913         // - ?Se unico indirizzo e-mail più lungo di 100 bytes:?
054800170913         //   ?NON viene controllato!!!?
054900170911         clear  wEMLindO;
055000170912
055100170912         IF  %subst( V1emDes : %len( §EMLindI ) + 1 ) = *blank;
055200170912
055300170911           §EMLindI = V1emDes;
055400170912
055500170912         ELSE;
055600170912
055700170918           For  xx = %len( §EMLindI )  DownTo 1;
055800170912
055900170911             if  %subst( V1emDes : xx : 1 ) = *blank  or
056000170911                 %subst( V1emDes : xx : 1 ) = ';';
056100170911               §EMLindI = %subst( V1emDes : 1 : xx - 1 );
056200170911               leave;
056300170911             endif;
056400170912
056500170911           EndFor;
056600170912
056700170911           if  §EMLindI = *blank  and  Not $Avviso;
056800170913             $Avviso      = *on;
056900170913             $ErrGenerico = *on;
057000170913             $ErrMessage  = *on;
057100170913             $PosCurEDES  = *on;
057200170913             VIDmsg = sk_Msg(07);
057300170911             leavesr;
057400170911           endif;
057500170912
057600170912         ENDIF;
057700170911
057800170911         If  §EMLindI <> *blank;
057900170912
058000170911           xemail ( dsEMAIL );
058100170911           if  §EMLerrO <> *off;
058200170913             $ErrGenerico = *on;
058300170913             $ErrMessage  = *on;
058400170913             $PosCurEDES  = *on;
058500170911             VIDmsg = §EMLmsgO;
058600170911             leavesr;
058700170911           endif;
058800170912
058900170913           // - ?Più indirizzi e-mail, per una lunghezza totale di?
059000170913           //   ?oltre 100 bytes?
059100170911           if  %subst( V1emDes : %len( §EMLindI ) + 1 ) <> *blank  and
059200170911               Not  $Avviso        and
059300170911               xx     > *zero      and
059400170911               %subst( V1emDes : xx + 1 ) <> *blank;
059500170912
059600170911             wEMLindO = §EMLindO;
059700170911             clear  dsEMAIL;
059800170911             §EMLindI = %subst( V1emDes : xx + 1 );
059900170911             xemail ( dsEMAIL );
060000170912
060100170911             if  §EMLerrO <> *off;
060200170913               $ErrGenerico = *on;
060300170913               $ErrMessage  = *on;
060400170913               $PosCurEDES  = *on;
060500170911               VIDmsg = §EMLmsgO;
060600170911               leavesr;
060700170911             endif;
060800170912
060900170911           endif;
061000170912
061100170913           // - ?REimpostazione a video dell'indirizzo e-mail corretto?
061200170911           if  wEMLindO = *blank;
061300170911             V1emDes = §EMLindO;
061400170911           else;
061500170911             V1emDes = %trim( wEMLindO ) + ';' + %trim( §EMLindO );
061600170911           endif;
061700170912
061800170911         EndIf;
061900170911
062000170911       ENDSR;
062100170911
062200170911       //--------------------------------------------------------------
062300170913       // ?Gestione F6 = Conferma - Copia spool in DOCFLOW.            ?
062400170911       //--------------------------------------------------------------
062500170911       BEGSR  sr_F06D01;
062600170912
062700170920         // - ?Override al file di stampa ed apertura dello stesso?
062800170920         if  NOT  %open(PrtEMAIL);
062900170920           exsr  sr_Override;
063000170920           open  PrtEMAIL;
063100170920         endif;
063200170920
063300170920
063400170920         // - ?Preparazione del corpo della singola e-mail?
063500170920         wEMLtxt = 'Fatturazione del ' + %editc( V1dtFt : 'Y' );
063600170920         except  PrtDet;
063700170920
063800170920         clear  wEMLtxt;
063900170920         except  PrtDet;
064000170920
064100170920         wEMLtxt = 'Ora indicativa di presenza dei file di spool +
064200170920                   sull';
064300170920         select;
064400170920           when  V1fatA <> *blank   and
064500170920                 V1fatD =  *blank   and
064600170920                 V1fat9 =  *blank;
064700170920             wEMLtxt = %trimR( wEMLtxt ) + 'a cartella FATA';
064800170920           when  V1fatA =  *blank   and
064900170920                 V1fatD <> *blank   and
065000170920                 V1fat9 =  *blank;
065100170920             wEMLtxt = %trimR( wEMLtxt ) + 'a cartella FATD';
065200170920           when  V1fatA =  *blank   and
065300170920                 V1fatD =  *blank   and
065400170920                 V1fat9 <> *blank;
065500170920             wEMLtxt = %trimR( wEMLtxt ) + 'a cartella FAT9';
065600170920           other;
065700170920             wEMLtxt = %trimR( wEMLtxt ) + 'e cartelle';
065800170920             $First  = *on;
065900170920             if  V1fatA <> *blank;
066000170920               if  $First = *off;
066100170920                 wEMLtxt = %trimR( wEMLtxt ) + ',';
066200170920               endif;
066300170920               wEMLtxt = %trimR( wEMLtxt ) + ' FATA';
066400170920               $First  = *off;
066500170920             endif;
066600170920             if  V1fatD <> *blank;
066700170920               if  $First = *off;
066800170920                 wEMLtxt = %trimR( wEMLtxt ) + ',';
066900170920               endif;
067000170920               wEMLtxt = %trimR( wEMLtxt ) + ' FATD';
067100170920               $First  = *off;
067200170920             endif;
067300170920             if  V1fat9 <> *blank;
067400170920               if  $First = *off;
067500170920                 wEMLtxt = %trimR( wEMLtxt ) + ',';
067600170920               endif;
067700170920               wEMLtxt = %trimR( wEMLtxt ) + ' FAT9';
067800170920               $First  = *off;
067900170920             endif;
068000170920         endsl;
068100170920         wEMLtxt = %trimR( wEMLtxt ) + ': ' +
068200170920                   %editw( V1hmPF : '0 :  ' );
068300170920         except  PrtDet;
068400170920
068500170920         clear  wEMLtxt;
068600170920         except  PrtDet;
068700170920         except  PrtDet;
068800170920
068900170920         wEMLtxt = 'BRT S.p.A.';
069000170920         except  PrtDet;
069100170920
069200170920         // - ?Chiusura dello spool?
069300170920         close  PrtEMAIL;
069400170920
069500170920         // - ?Eliminazione override?
069600170920         Qcmd = c_CmdDltOvr;
069700170920         exsr  sr_ExecCmd;
069800170920
069900170920         // - ?Uscira dal *pgm?
070000170920         $Fine = *on;
070100170920
070200170913       ENDSR;
070300170920
070400170920       //--------------------------------------------------------------
070500170920       //?Override al file di stampa per impostarvi i dati per         ?
070600170920       //?  l'invio via e-mail   +   stampa inizio e-mail              ?
070700170920       //--------------------------------------------------------------
070800170920       BEGSR  sr_Override;
070900170920
071000170920         //*·reset  $Invio;
071100170920         reset  TRTCM1ds;
071200170920
071300170920         If  dMRAdan.§MRAdreg <> *blank;
071400170920           §CM1mitt = V1emMit;
071500170920           §CM1dst  = V1emDes;
071600170920           §CM1tips = dMRAdan.§MRAdreg;
071700170920           §CM1po   = c_Sede;
071800170920           //*·§CM1var  = '*OBJM*' + dMRAdan.§MRAddes;  // ?(un po' corto...)?
071900170920           §CM1var  = '*OBJM*' + c_Oggetto;
072000170920           §CM1idp  = dMRAdan.§MRAdidpro;
072100170920           Qcmd = c_CmdOvrPrtF
072200170920                + ' outq(' + %trim(dMRAdan.§MRAdoutqi) + ')'
072300170920                + ' usrdfndta(''' + TRTCM1ds + ''')';
072400170920         Else;
072500170920           Qcmd = c_CmdOvrPrtF;
072600170920         EndIf;
072700170920
072800170920         exsr  sr_ExecCmd;
072900170920         //*·If  dMRAdan.§MRAdreg <> *blanks  and  Qusei = *blank;
073000170920         //*·  $Invio = *on;
073100170920         //*·EndIf;
073200170920
073300170920       ENDSR;
073400170920
073500170920       //--------------------------------------------------------------
073600170920       //?Esecuzione del comando (già impostato).                      ?
073700170920       //--------------------------------------------------------------
073800170920       BEGSR  sr_ExecCmd;
073900170920
074000170920         clear Qcap0100;
074100170920         Qcabcsdh = *off;
074200170920         Qcapa    = *off;
074300170920         Qcacmdss = *off;
074400170920         Qcaerved = *allX'00';
074500170920
074600170920         clear Qusec;
074700170920         Qusbprv  = %size(Qusec);
074800170920
074900170920         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
075000170920                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
075100170920                           0 : 0 : Qusec);
075200170920
075300170920         // -?Invio *msg per segnalazione errore?
075400170920         //if  Qusei <> *blank;
075500170920         //  ...;
075600170920         //endif;
075700170920
075800170920       ENDSR;
075900170911
076000170911       //--------------------------------------------------------------
076100170913       //??Operazioni finali.                                          ?
076200170911       //--------------------------------------------------------------
076300170911       BEGSR  sr_RoutEnd;
076400170911
076500170913         // - ?Uscita dal *pgm?
076600170911         return;
076700170911
076800170911       ENDSR;
076900170911
077000170911      /end-free
077100170920
077200170920      //---------------------------------------------------------------
077300170920      //?Spool di stampa (per e-mail).                                 ?
077400170920      //---------------------------------------------------------------
077500170920
077600170920     oPrtEMAIL  e            PRTdet      1
077700170920     o                       wEMLtxt
077800170920
077900170920       //--------------------------------------------------------------
078000170913       // ?Schiere a tempo di compilazione.                            ?
078100170911       //--------------------------------------------------------------
078200170911
078300170913** --? sk_Msg:? Messaggi di Errore ?-----------------------------------------*
078400170911Funzione eseguibile solo in ambiente di SEDE: premere "Invio" per uscire       1
078500170913Selezionare ALMENO un file di spool                                            2
078600170913Valore errato per la selezione                                                 3
078700170913Data formalmente errata                                                        4
078800170913Ora formalmente errata                                                         5
078900170913Indirizzo e-mail obbligatorio                                                  6
079000170913AVVISO: indirizzo e-mail troppo lungo per il controllo formale. Premere Invio. 7
079100170913Rilevato errore nella trasmissione degli spool di fatturazione.                8
