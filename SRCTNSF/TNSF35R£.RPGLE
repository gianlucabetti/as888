000100170913       //==============================================================
000200170913       // ?Trasferimento in DOCFLOW delle Mail di Trasmissione dei     ?
000300170913       // ?   Flussi di Fatturazione.                                  ?
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
002600170911     f                                     infds( InfDspF )
002700170911
002800170911       //--------------------------------------------------------------
002900170913       // ?Definizione costanti.                                       ?
003000170911       //--------------------------------------------------------------
003100170911
003200170913       // - ?Tasti funzionali a video?
003300170911     d c_F01           c                   const(x'31')
003400170911     d c_F02           c                   const(x'32')
003500170911     d c_F03           c                   const(x'33')
003600170911     d c_F04           c                   const(x'34')
003700170911     d c_F05           c                   const(x'35')
003800170911     d c_F06           c                   const(x'36')
003900170911     d c_F07           c                   const(x'37')
004000170911     d c_F08           c                   const(x'38')
004100170911     d c_F09           c                   const(x'39')
004200170911     d c_F10           c                   const(x'3A')
004300170911     d c_F11           c                   const(x'3B')
004400170911     d c_F12           c                   const(x'3C')
004500170911     d c_F13           c                   const(x'B1')
004600170911     d c_F14           c                   const(x'B2')
004700170911     d c_F15           c                   const(x'B3')
004800170911     d c_F16           c                   const(x'B4')
004900170911     d c_F17           c                   const(x'B5')
005000170911     d c_F18           c                   const(x'B6')
005100170911     d c_F19           c                   const(x'B7')
005200170911     d c_F20           c                   const(x'B8')
005300170911     d c_F21           c                   const(x'B9')
005400170911     d c_F22           c                   const(x'BA')
005500170911     d c_F23           c                   const(x'BB')
005600170911     d c_F24           c                   const(x'BC')
005700170911     d c_Enter         c                   const(x'F1')
005800170911     d c_RollDown      c                   const(x'F4')
005900170911     d c_RollUp        c                   const(x'F5')
006000170911
006100170911       //--------------------------------------------------------------
006200170913       // ?Definizione schiere.                                        ?
006300170911       //--------------------------------------------------------------
006400170911
006500170913       // - ?Messaggi di errore?
006600170913     d sk_Msg          s             78    dim( 8)  ctdata  perrcd( 1)
006700170911
006800170911       //--------------------------------------------------------------
006900170913       // ?Definizione aree dati.                                      ?
007000170911       //--------------------------------------------------------------
007100170911
007200170913       // - ?Dati utente?
007300170911     d §AzUte        e ds                  extname(AZUTE00F)
007400170911     d                                     dtaara
007500170911     d §DatiUte      e ds                  extname(dDatiUte)
007600170911     d                                     dtaara
007700170911
007800170911       //--------------------------------------------------------------
007900170913       // ?Definizione strutture dati.                                 ?
008000170911       //--------------------------------------------------------------
008100170911
008200170913       // - ?Status ds?
008300170911     d Status         sds
008400170911     d   SDSpgmName      *proc
008500170913     d   SDSparms        *parms
008600170913     d   SDSdata             191    198
008700170913     d   SDSjobName          244    253
008800170913     d   SDSuser             254    263
008900170913     d   SDSjobNumber        264    269s 0
009000170911
009100170913       // - ?InfDS?
009200170911     d InfDspF         ds
009300170911     d   dsp_aid             369    369a
009400170911     d*//sfl_rrn             376    377i 0
009500170911     d*//min_nrr             378    379i 0
009600170911     d*//num_rcds            380    381i 0
009700170911
009800170913       // - ?IndDS?
009900170913     d IndDspF         ds                  inz
010000170913         // ·?Emissione messaggio di errore?
010100170913     d   $AbilitF6                     n   overlay( IndDspF : 06 )
010200170913     d   $AbilitRlUp                   n   overlay( IndDspF : 25 )
010300170913     d   $AbilitRlDwn                  n   overlay( IndDspF : 26 )
010400170913         // ·?Emissione messaggio di errore?
010500170911     d   $ErrMessage                   n   overlay( IndDspF : 28 )
010600170913         // ·?Posizionamento cursore?
010700170911     d   $PosCurFATA                   n   overlay( IndDspF : 51 )
010800170911     d   $PosCurFATD                   n   overlay( IndDspF : 52 )
010900170911     d   $PosCurFAT9                   n   overlay( IndDspF : 53 )
011000170911     d   $PosCurDTFT                   n   overlay( IndDspF : 54 )
011100170911     d   $PosCurHMPF                   n   overlay( IndDspF : 55 )
011200170911     d   $PosCurEMIT                   n   overlay( IndDspF : 56 )
011300170911     d   $PosCurEDES                   n   overlay( IndDspF : 57 )
011400170911     d   $PosCurEDCC                   n   overlay( IndDspF : 58 )
011500170913         // ·?Riemissione videata?
011600170911     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
011700170911
011800170911       // - Orari a video
011900170912     d wHhMm_ds        ds             4    inz   qualified
012000170911     d   wHH                          2s 0 inz
012100170911     d   wMM                          2s 0 inz
012200170911
012300170913       // - ?Parametri ricevuti?
012400170911     d KPJBA         e ds
012500170913
012600170913     d TNSF35ds      e ds                  inz   qualified
012700170911
012800170911       //--------------------------------------------------------------
012900170913       // ?Definizione variabili globali.                              ?
013000170911       //--------------------------------------------------------------
013100170911
013200170913       // - ?Flags booleani?
013300170911     d $Fine           s               n   inz
013400170911     d $InzD01         s               n   inz(*on)
013500170911     d $Avviso         s               n   inz
013600170911     d $Avviso2        s               n   inz
013700170911
013800170913       // - ?Indici di schiera?
013900170911     d xx              s              3  0 inz
014000170911
014100170913       // - ?Variabili per la gestione del video?
014200170911     d $Video          s              2    inz('D1')
014300170911
014400170913       // - ?Variabili di comodo?
014500170912     d wTime           s               t   inz   timfmt(*hms)
014600170913     d wMinutes        s              2  0 inz
014700170913     d wDataFatt       s              8    inz
014800170912     d wEMLindO        s                   inz   like(§EMLindO)
014900170913     d wSplFile        s                   inz   like(prm_SplFile)
015000170913     d wSubFldr        s                   inz   like(prm_SubFldr)
015100170911
015200170911       //--------------------------------------------------------------
015300170913       // ?Definizione prototipi procedure.                            ?
015400170911       //--------------------------------------------------------------
015500170911
015600170913       // - ?Controllo/Inversione Date?
015700170911     d WLBdat          ds                  inz
015800170911     d   G08dat                       8  0 inz
015900170911     d   G08inv                       8  0 inz
016000170911     d   G08err                       1    inz('3')
016100170911     d   G08tgi                       5  0 inz
016200170911      /copy gaitrasrc/srcProtoPR,XSRDA8
016300170911
016400170913       // - ?Reperimento dati utente?
016500170911     d TIBS34ds      e ds                  inz
016600170911      /copy gaitrasrc/srcProtoPR,TIBS34R
016700170913
016800170913       // - ?Controllo indirizzo e-mail?
016900170913     d dsEMAIL       e ds                  inz
017000170913      /copy gaitrasrc/srcProtoPR,XEMAIL
017100170911
017200170913       // - ?Copia spool in DOCFLOW?
017300170913      /copy gaitrasrc/srcProtoPI,TNSF35C
017400170913      /copy gaitrasrc/srcProtoPR,TNSF35C
017500170913
017600170913       // - ?API QCAPCMD (Process Commands)?
017700170913     d*//· Qcmd            s           2048    inz   varying
017800170913      *//·/copy qSysInc/qRpgleSrc,QCAPCMD
017900170913      *//·/copy gaitrasrc/srcProtoPR,QCAPCMD
018000170913       // - ?Parametri gestione errori API.?
018100170913      *//·/copy qsysinc/qRpgleSrc,QUSEC
018200170911
018300170911       //--------------------------------------------------------------
018400170913       // ?Definizione key-list.                                       ?
018500170911       //--------------------------------------------------------------
018600170911
018700170911
018800170911       //--------------------------------------------------------------
018900170913       // ?Riepilogo indicatori utilizzati.                            ?
019000170911       //--------------------------------------------------------------
019100170911
019200170911
019300170911       //--------------------------------------------------------------
019400170913       // ?M A I N - L I N E                                           ?
019500170911       //--------------------------------------------------------------
019600170911
019700170911     c     *Entry        plist
019800170911     c                   parm                    KPJBA
019900170911
020000170911      /free
020100170911
020200170913       // - ?Operazioni iniziali?
020300170911       exsr  sr_RoutInz;
020400170911
020500170913       // - ?Gestione videate?
020600170911       DoW  $Fine = *off;
020700170911
020800170911         Select;
020900170911
021000170913           // - ?Gestione videata D1?
021100170911           When $Video = 'D1';
021200170911             exsr  sr_GesD01;
021300170913
021400170913           // - ?Gestione videata D2?
021500170913           When $Video = 'D2';
021600170913             exsr  sr_GesD02;
021700170911
021800170913           // - ?? ? ? ? ??
021900170911           Other;
022000170911             $Fine = *on;
022100170911
022200170911         EndSl;
022300170911
022400170911       EndDo;
022500170911
022600170913       // - ?Operazioni finali?
022700170911       exsr sr_RoutEnd;
022800170911
022900170911       //--------------------------------------------------------------
023000170913       // ?Operazioni iniziali.                                        ?
023100170911       //--------------------------------------------------------------
023200170911       BEGSR  sr_RoutInz;
023300170911
023400170913         // - ?Impostazione chiusura?
023500170911         *inLR = *on;
023600170911
023700170913         // - ?Reperimento dati job?
023800170911         exsr  sr_DatiJob;
023900170911
024000170913         // - ?Impostazione nome programma a video?
024100170913         VT1pgm = SDSpgmName;
024200170911
024300170911       ENDSR;
024400170911
024500170911       //--------------------------------------------------------------
024600170913       // ?Reperimento Dati del job (Utente/Operativi).                ?
024700170911       //--------------------------------------------------------------
024800170911       BEGSR  sr_DatiJob;
024900170911
025000170911         in(e) §AzUte;
025100170911         if NOT %error;
025200170911           in(e) §DatiUte;
025300170911         endif;
025400170911         if %error or RSut = *blank;
025500170911           tibs34r ( tibs34ds );
025600170911           in §AzUte;
025700170911           in §DatiUte;
025800170911         endif;
025900170911
026000170911       ENDSR;
026100170911
026200170911       //--------------------------------------------------------------
026300170913       // ?Gestione videata D01.                                       ?
026400170911       //--------------------------------------------------------------
026500170911       BEGSR  sr_GesD01;
026600170911
026700170913         // - ?Inizializzazione videata?
026800170911         if  $InzD01 = *on  and  Not $errGenerico;
026900170911           exsr  sr_InzD01;
027000170911           $InzD01 = *off;
027100170911         endif;
027200170913
027300170913         // - ?(Dis)Abilitazione tasti funzionali?
027400170913         $AbilitF6   = *on;
027500170913         $AbilitRlUp = ( %subst( kNmUs : 1 : 5 ) = 'EDPSM' );
027600170911
027700170913         // - ?Emissione Testata e Piede?
027800170911         write  SF35T01;
027900170911         write  SF35P01;
028000170911
028100170913         // - ?Emissione videata di richiesta parametri?
028200170911         exfmt  SF35D01;
028300170911
028400170911         clear  VIDmsg;
028500170911         reset  $ErrMessage;
028600170911         reset  $ErrGenerico;
028700170911
028800170911         Select;
028900170911
029000170913           // - ?Sistema Informativo di Filiale => Uscita forzata?
029100170911           When  DUTlpo <> 'S';
029200170911             $Fine = *on;
029300170911
029400170913           // - ?F3=Fine?
029500170911           When  dsp_aid = c_F03;
029600170911             $Fine = *on;
029700170913
029800170913           // - ?RollUp = Videata successiva?
029900170913           When  dsp_aid = c_RollUp;
030000170913             $Video = 'D2';
030100170911
030200170913           // - ?Invio / F6 = Conferma?
030300170911           Other;
030400170911             exsr  sr_CtrD01;
030500170911             if  Not $ErrGenerico  and  dsp_aid = c_F06;
030600170911               exsr  sr_F06D01;
030700170911             endif;
030800170911
030900170911         EndSl;
031000170911
031100170911       ENDSR;
031200170911
031300170911       //--------------------------------------------------------------
031400170913       // ?Inizializzazione videata D01.                               ?
031500170911       //--------------------------------------------------------------
031600170911       BEGSR  sr_InzD01;
031700170911
031800170913         // - ?Pulizia videata?
031900170911         clear  SF35D01;
032000170913
032100170913         // - ?Impostazione parametri eventualmente ricevuti?
032200170913         clear  TNSF35ds;
032300170913         if  KPJBU <> *blank;
032400170913           TNSF35ds = KPJBU;
032500170913         else;
032600170913           TNSF35ds.i35aplK = 'TNSF35C   ';
032700170913           TNSF35ds.i35coda = 'FATTMESE  ';
032800170913           TNSF35ds.i35job  = SDSjobName;
032900170913           TNSF35ds.i35user = SDSuser;
033000170913           TNSF35ds.i35num  = SDSjobNumber;
033100170913           TNSF35ds.i35dtFt = %dec( %date() );
033200170913         endif;
033300170911
033400170913         // - ?Impostazione campi a video?
033500170913         reset  WLBdat;
033600170913         G08inv = TNSF35ds.i35dtFt;
033700170913         xsrDA8( WLBdat );
033800170913         V1DtFt = G08dat;
033900170912
034000170912         wTime    = %time();
034100170912         wMinutes = %int( %subdt( wTime : *mn ) );
034200170913         wTime   -= %minutes( wMinutes );
034300170913         wTime   += %minutes( %div( wMinutes : 15 ) * 15 );
034400170912         if  %rem( wMinutes : 15 ) > *zero;
034500170913           wTime += %minutes(30);
034600170913         else;
034700170913           wTime += %minutes(15);
034800170912         endif;
034900170913         wMinutes = %int( %subdt( wTime : *mn ) );
035000170913         V1hmPF   = ( %int( %subdt( wTime : *h ) ) * 100 ) +
035100170913                      %int( %subdt( wTime : *mn ) );
035200170913
035300170913         V1job    = TNSF35ds.i35job;
035400170913         V1user   = TNSF35ds.i35user;
035500170913         V1num    = TNSF35ds.i35num;
035600170913         V1coda   = TNSF35ds.i35coda;
035700170913         V1aplK   = TNSF35ds.i35aplK;
035800170911
035900170911         reset  $Avviso;
036000170913         reset  $Avviso2;
036100170911
036200170913         // - ?Controllo del sistema informativo in cui si sta lavorando?
036300170913         //   ?(DEVE essere di SEDE)?
036400170911         if  DUTlpo <> 'S';
036500170911           $ErrGenerico = *on;
036600170911           $ErrMessage  = *on;
036700170911           VIDmsg = sk_Msg(01);
036800170911           leavesr;
036900170911         endif;
037000170911
037100170911       ENDSR;
037200170911
037300170911       //--------------------------------------------------------------
037400170913       // ?Controllo dati nella videata D01.                           ?
037500170911       //--------------------------------------------------------------
037600170911       BEGSR  sr_CtrD01;
037700170911
037800170913         // - ?Spegnimento degli indicatori di errore?
037900170911         %subst( IndDspF : 28 ) = *off;
038000170911
038100170913         // - ?Controllo selezione di almeno uno spool?
038200170911         if  V1fatA = *blank  and
038300170911             V1fatD = *blank  and
038400170913             V1fat9 = *blank;
038500170911           $ErrGenerico = *on;
038600170911           $ErrMessage  = *on;
038700170913           $PosCurFATA  = *on;
038800170911           VIDmsg = sk_Msg(02);
038900170911           leavesr;
039000170911         endif;
039100170911
039200170911         select;
039300170913           // - ?Controllo selezione spool "FATA"?
039400170911           when  V1fatA <> *blank  and
039500170911                 V1fatA <> 'X';
039600170911             $ErrGenerico = *on;
039700170911             $ErrMessage  = *on;
039800170913             $PosCurFATA  = *on;
039900170911             VIDmsg = sk_Msg(03);
040000170911             leavesr;
040100170913           // - ?Controllo selezione spool "FATD"?
040200170911           when  V1fatD <> *blank  and
040300170911                 V1fatD <> 'X';
040400170911             $ErrGenerico = *on;
040500170911             $ErrMessage  = *on;
040600170913             $PosCurFATD  = *on;
040700170911             VIDmsg = sk_Msg(03);
040800170911             leavesr;
040900170913           // - ?Controllo selezione spool "FATX"?
041000170911           when  V1fat9 <> *blank  and
041100170911                 V1fat9 <> 'X';
041200170911             $ErrGenerico = *on;
041300170911             $ErrMessage  = *on;
041400170913             $PosCurFAT9  = *on;
041500170911             VIDmsg = sk_Msg(03);
041600170911             leavesr;
041700170911         endsl;
041800170911
041900170913         // - ?Controllo Data Fatturazione?
042000170911         clear  WLBdat;
042100170911         G08dat = V1DtFt;
042200170911         xsrDA8( WLBdat );
042300170911         if  G08err = *on;
042400170911           $ErrGenerico = *on;
042500170911           $ErrMessage  = *on;
042600170911           $PosCurDTFT  = *on;
042700170911           VIDmsg = sk_Msg(04);
042800170911           leavesr;
042900170911         endif;
043000170911         V1DtFt = G08dat;
043100170913         wDataFatt = %editc( G08inv : 'X' );
043200170911
043300170913         // - ?Controllo ora presenza flussi?
043400170912         wHhMm_ds = %editc( V1HmPf : 'X' );
043500170912         if  wHhMm_ds.wHH < 00  or  wHhMm_ds.wHH > 23  or
043600170912             wHhMm_ds.wMM < 00  or  wHhMm_ds.wMM > 59;
043700170913           $ErrGenerico = *on;
043800170913           $ErrMessage  = *on;
043900170913           $PosCurHMPF  = *on;
044000170912           VIDmsg = sk_Msg(05);
044100170912           leavesr;
044200170912         endif;
044300170911
044400170913         // - ?Controllo indirizzo e-mail Mittente?
044500170911         if  V1emMit = *blank;
044600170911           $ErrGenerico = *on;
044700170911           $ErrMessage  = *on;
044800170911           $PosCurEMIT  = *on;
044900170911           VIDmsg = sk_Msg(06);
045000170911           leavesr;
045100170911         endif;
045200170911
045300170911         clear  xx;
045400170911         clear  dsEMAIL;
045500170911         clear  wEMLindO;
045600170911         §EMLindI = V1emMit;
045700170911         xemail ( dsEMAIL );
045800170911         if  §EMLerrO <> *off;
045900170913           $ErrGenerico = *on;
046000170913           $ErrMessage  = *on;
046100170913           $PosCurEMIT  = *on;
046200170911           VIDmsg = §EMLmsgO;
046300170911           leavesr;
046400170911         endif;
046500170911         V1emMit  = §EMLindO;
046600170911
046700170913         // - ?Controllo indirizzo e-mail Destinatari?
046800170911         if  V1emDes = *blank;
046900170911           $ErrGenerico = *on;
047000170911           $ErrMessage  = *on;
047100170911           $PosCurEDES  = *on;
047200170911           VIDmsg = sk_Msg(06);
047300170911           leavesr;
047400170911         endif;
047500170911
047600170911         clear  xx;
047700170911         clear  dsEMAIL;
047800170913         // - ?Se unico indirizzo e-mail più lungo di 100 bytes:?
047900170913         //   ?NON viene controllato!!!?
048000170911         clear  wEMLindO;
048100170912
048200170912         IF  %subst( V1emDes : %len( §EMLindI ) + 1 ) = *blank;
048300170912
048400170911           §EMLindI = V1emDes;
048500170912
048600170912         ELSE;
048700170912
048800170911           For  xx = %len( §EMLindI )  By -1  DownTo 1;
048900170912
049000170911             if  %subst( V1emDes : xx : 1 ) = *blank  or
049100170911                 %subst( V1emDes : xx : 1 ) = ';';
049200170911               §EMLindI = %subst( V1emDes : 1 : xx - 1 );
049300170911               leave;
049400170911             endif;
049500170912
049600170911           EndFor;
049700170912
049800170911           if  §EMLindI = *blank  and  Not $Avviso;
049900170913             $Avviso      = *on;
050000170913             $ErrGenerico = *on;
050100170913             $ErrMessage  = *on;
050200170913             $PosCurEDES  = *on;
050300170913             VIDmsg = sk_Msg(07);
050400170911             leavesr;
050500170911           endif;
050600170912
050700170912         ENDIF;
050800170911
050900170911         If  §EMLindI <> *blank;
051000170912
051100170911           xemail ( dsEMAIL );
051200170911           if  §EMLerrO <> *off;
051300170913             $ErrGenerico = *on;
051400170913             $ErrMessage  = *on;
051500170913             $PosCurEDES  = *on;
051600170911             VIDmsg = §EMLmsgO;
051700170911             leavesr;
051800170911           endif;
051900170912
052000170913           // - ?Più indirizzi e-mail, per una lunghezza totale di?
052100170913           //   ?oltre 100 bytes?
052200170911           if  %subst( V1emDes : %len( §EMLindI ) + 1 ) <> *blank  and
052300170911               Not  $Avviso        and
052400170911               xx     > *zero      and
052500170911               %subst( V1emDes : xx + 1 ) <> *blank;
052600170912
052700170911             wEMLindO = §EMLindO;
052800170911             clear  dsEMAIL;
052900170911             §EMLindI = %subst( V1emDes : xx + 1 );
053000170911             xemail ( dsEMAIL );
053100170912
053200170911             if  §EMLerrO <> *off;
053300170913               $ErrGenerico = *on;
053400170913               $ErrMessage  = *on;
053500170913               $PosCurEDES  = *on;
053600170911               VIDmsg = §EMLmsgO;
053700170911               leavesr;
053800170911             endif;
053900170912
054000170911           endif;
054100170912
054200170913           // - ?REimpostazione a video dell'indirizzo e-mail corretto?
054300170911           if  wEMLindO = *blank;
054400170911             V1emDes = §EMLindO;
054500170911           else;
054600170911             V1emDes = %trim( wEMLindO ) + ';' + %trim( §EMLindO );
054700170911           endif;
054800170912
054900170911         EndIf;
055000170911
055100170913         // - ?Controllo indirizzo e-mail Dest. x Conoscenza?
055200170911         if  V1emDcc = *blank;
055300170911           $ErrGenerico = *on;
055400170911           $ErrMessage  = *on;
055500170911           $PosCurEDCC  = *on;
055600170911           VIDmsg = sk_Msg(06);
055700170911           leavesr;
055800170911         endif;
055900170911
056000170911         clear  xx;
056100170911         clear  dsEMAIL;
056200170913         // - ?Se unico indirizzo e-mail più lungo di 100 bytes:?
056300170913         //   ?NON viene controllato!!!?
056400170911         clear  wEMLindO;
056500170912
056600170912         IF  %subst( V1emDcc : %len( §EMLindI ) + 1 ) = *blank;
056700170912
056800170911           §EMLindI = V1emDcc;
056900170912
057000170912         ELSE;
057100170912
057200170911           For  xx = %len( §EMLindI )  By -1  DownTo 1;
057300170912
057400170911             if  %subst( V1emDcc : xx : 1 ) = *blank  or
057500170911                 %subst( V1emDcc : xx : 1 ) = ';';
057600170911               §EMLindI = %subst( V1emDcc : 1 : xx - 1 );
057700170911               leave;
057800170911             endif;
057900170912
058000170911           EndFor;
058100170912
058200170911           if  §EMLindI = *blank  and  Not $Avviso2;
058300170913             $Avviso2     = *on;
058400170913             $ErrGenerico = *on;
058500170913             $ErrMessage  = *on;
058600170913             $PosCurEDCC  = *on;
058700170913             VIDmsg = sk_Msg(07);
058800170911             leavesr;
058900170911           endif;
059000170912
059100170912         ENDIF;
059200170911
059300170911         If  §EMLindI <> *blank;
059400170912
059500170911           xemail ( dsEMAIL );
059600170911           if  §EMLerrO <> *off;
059700170913             $ErrGenerico = *on;
059800170913             $ErrMessage  = *on;
059900170913             $PosCurEDCC  = *on;
060000170911             VIDmsg = §EMLmsgO;
060100170911             leavesr;
060200170911           endif;
060300170912
060400170913           // - ?Più indirizzi e-mail, per una lunghezza totale di?
060500170913           //   ?oltre 100 bytes?
060600170911           if  %subst( V1emDcc : %len( §EMLindI ) + 1 ) <> *blank  and
060700170913               Not  $Avviso2       and
060800170911               xx     > *zero      and
060900170911               %subst( V1emDcc : xx + 1 ) <> *blank;
061000170912
061100170911             wEMLindO = §EMLindO;
061200170911             clear  dsEMAIL;
061300170911             §EMLindI = %subst( V1emDcc : xx + 1 );
061400170911             xemail ( dsEMAIL );
061500170912
061600170911             if  §EMLerrO <> *off;
061700170913               $ErrGenerico = *on;
061800170913               $ErrMessage  = *on;
061900170913               $PosCurEDCC  = *on;
062000170911               VIDmsg = §EMLmsgO;
062100170911               leavesr;
062200170911             endif;
062300170912
062400170911           endif;
062500170912
062600170913           // - ?REimpostazione a video dell'indirizzo e-mail corretto?
062700170911           if  wEMLindO = *blank;
062800170911             V1emDcc = §EMLindO;
062900170911           else;
063000170911             V1emDcc = %trim( wEMLindO ) + ';' + %trim( §EMLindO );
063100170911           endif;
063200170912
063300170911         EndIf;
063400170911
063500170911       ENDSR;
063600170911
063700170911       //--------------------------------------------------------------
063800170913       // ?Gestione F6 = Conferma - Copia spool in DOCFLOW.            ?
063900170911       //--------------------------------------------------------------
064000170911       BEGSR  sr_F06D01;
064100170912
064200170913         // - ?Copia spool in cartelle FATA?
064300170912         If  V1fata = 'X';
064400170913           wSubFldr = 'FATA';
064500170913           wSplFile = 'FTWCODIM  ';
064600170913           exsr  sr_Copia_1_Spool;
064700170913           if  $ErrGenerico;
064800170913             $PosCurFATA = *on;
064900170913             leavesr;
065000170913           endif;
065100170913         EndIf;
065200170913
065300170913         // - ?Copia spool in cartelle FATD?
065400170913         If  V1fata = 'X';
065500170913           wSubFldr = 'FATD';
065600170913           wSplFile = 'FTWCODDS  ';
065700170913           exsr  sr_Copia_1_Spool;
065800170913           if  $ErrGenerico;
065900170913             $PosCurFATD = *on;
066000170913             leavesr;
066100170913           endif;
066200170913         EndIf;
066300170913
066400170913         // - ?Copia spool in cartelle FAT9?
066500170913         If  V1fata = 'X';
066600170913           wSubFldr = 'FAT9';
066700170913           wSplFile = 'FTWFAT89  ';
066800170913           exsr  sr_Copia_1_Spool;
066900170913           if  $ErrGenerico;
067000170913             $PosCurFAT9 = *on;
067100170913             leavesr;
067200170913           endif;
067300170913         EndIf;
067400170913
067500170913       ENDSR;
067600170913
067700170913       //--------------------------------------------------------------
067800170913       // ?Gestione videata D02.                                       ?
067900170913       //--------------------------------------------------------------
068000170913       BEGSR  sr_GesD02;
068100170913
068200170913         // - ?Inizializzazione videata già eseguita nella subr.?
068300170913         //   ?"sr_RoutInz"?
068400170913
068500170913         // - ?(Dis)Abilitazione tasti funzionali?
068600170913         $AbilitF6    = *off;
068700170913         $AbilitRlDwn = *on;
068800170913
068900170913         // - ?Emissione Testata e Piede?
069000170913         write  SF35T01;
069100170913         write  SF35P01;
069200170913
069300170913         // - ?Emissione videata di visualizzazione altri parametri?
069400170913         exfmt  SF35D02;
069500170913
069600170913         clear  VIDmsg;
069700170913         reset  $ErrMessage;
069800170913         reset  $ErrGenerico;
069900170913
070000170913         Select;
070100170913
070200170913           // - ?F3=Fine?
070300170913           When  dsp_aid = c_F03;
070400170913             $Fine = *on;
070500170913
070600170913           // - ?RollDown = Videata precedente?
070700170913           When  dsp_aid = c_RollDown;
070800170913             $Video = 'D1';
070900170913
071000170913         EndSl;
071100170913
071200170913       ENDSR;
071300170913
071400170913       //--------------------------------------------------------------
071500170913       //??Copia singolo gruppo di spool in DOCFLOW.                   ?
071600170913       //--------------------------------------------------------------
071700170913       BEGSR  sr_Copia_1_Spool;
071800170913
071900170913           prm_APLkey   = V1aplK;
072000170913           prm_SubFldr  = wSubFldr;
072100170913           prm_Coda     = V1coda;
072200170913           prm_SplFile  = wSplFile;
072300170913           prm_Job      = V1job;
072400170913           prm_User     = V1user;
072500170913           prm_Number   = %editc( V1num : 'X' );
072600170913           prm_DataFatt = wDataFatt;
072700170913           prm_File     = %trim( wSubFldr ) +
072800170913                          %subst( %editc( %subdt( %date() : *years ) : 'X' )
072900170913                                  : 3 : 2 ) +
073000170913                          %editc( %subdt( %date() : *months ) : 'X' );
073100170912           clear  prm_Error;
073200170912
073300170913           tnsf35c ( prm_APLkey :
073400170913                     prm_SubFldr :
073500170913                     prm_Coda :
073600170913                     prm_SplFile :
073700170912                     prm_Job :
073800170912                     prm_User :
073900170912                     prm_Number :
074000170913                     prm_DataFatt :
074100170913                     prm_File :
074200170912                     prm_Error );
074300170912
074400170912           if  prm_Error = *on;
074500170913             $ErrGenerico = *on;
074600170913             $ErrMessage  = *on;
074700170913             VIDmsg = sk_Msg(07);
074800170912             leavesr;
074900170912           endif;
075000170911
075100170911       ENDSR;
075200170911
075300170911       //--------------------------------------------------------------
075400170913       //??Stampa segnalazione dell'errore rilevato via SQL.           ?
075500170911       //--------------------------------------------------------------
075600170913       //*·BEGSR  sr_PrintERR;
075700170913       //*·
075800170913       //*·  // -?Stampa del Dump?
075900170913       //*·  Dump(A);
076000170913       //*·
076100170913       //*·  // -?Stampa del Job-Log?
076200170913       //*·  Qcmd = 'DSPJOBLOG job(*) output(*print)';
076300170913       //*·  exsr  sr_ExecCmd;
076400170913       //*·
076500170913       //*·  // -?Segnalazione errore a video?
076600170913       //*·  $ErrGenerico = *on;
076700170913       //*·  $ErrMessage  = *on;
076800170913       //*·  VIDmsg = sk_Msg(08);
076900170913       //*·
077000170913       //*·ENDSR;
077100170911
077200170911       //--------------------------------------------------------------
077300170913       //??Esecuzione del comando (già impostato).                     ?
077400170911       //--------------------------------------------------------------
077500170913       //*·BEGSR  sr_ExecCmd;
077600170913       //*·
077700170913       //*·  clear Qcap0100;
077800170913       //*·  Qcabcsdh = *off;
077900170913       //*·  Qcapa    = *off;
078000170913       //*·  Qcacmdss = *off;
078100170913       //*·  Qcaerved = *allX'00';
078200170913       //*·
078300170913       //*·  clear Qusec;
078400170913       //*·  Qusbprv  = %size(Qusec);
078500170913       //*·
078600170913       //*·  ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
078700170913       //*·                    %size(Qcap0100) : 'CPOP0100' : *omit :
078800170913       //*·                    0 : 0 : Qusec);
078900170913       //*·
079000170913       //*·  //if  Qusei <> *blank;
079100170913       //*·  //  ...;
079200170913       //*·  //endif;
079300170913       //*·
079400170913       //*·ENDSR;
079500170911
079600170911       //--------------------------------------------------------------
079700170913       //??Operazioni finali.                                          ?
079800170911       //--------------------------------------------------------------
079900170911       BEGSR  sr_RoutEnd;
080000170911
080100170913         // - ?Uscita dal *pgm?
080200170911         return;
080300170911
080400170911       ENDSR;
080500170911
080600170911      /end-free
080700170911
080800170911       //--------------------------------------------------------------
080900170913       // ?Schiere a tempo di compilazione.                            ?
081000170911       //--------------------------------------------------------------
081100170911
081200170913** --? sk_Msg:? Messaggi di Errore ?-----------------------------------------*
081300170911Funzione eseguibile solo in ambiente di SEDE: premere "Invio" per uscire       1
081400170913Selezionare ALMENO un file di spool                                            2
081500170913Valore errato per la selezione                                                 3
081600170913Data formalmente errata                                                        4
081700170913Ora formalmente errata                                                         5
081800170913Indirizzo e-mail obbligatorio                                                  6
081900170913AVVISO: indirizzo e-mail troppo lungo per il controllo formale. Premere Invio. 7
082000170913Rilevato errore nella trasmissione degli spool di fatturazione.                8
