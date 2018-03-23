000100080807     H DECEDIT('0,') DATEDIT(*ymd/)
000200080807     H* FNLSV1R *-----------------------------------------------------*
000300080909     H*   Inventario documenti da restituire al mittente
000400930126     H*---------------------------------------------------------------*
000500080807     FFNLsv1d   CF   E             WORKSTN indds(IndDspF)
000600080807     f                                     infds(InfDspF)
000700080319     Ftabel00f  IF   E           K DISK
000800120330     Ftntbe01l  IF   E           K DISK
000900080807     Fcnaco00f  IF   E           K DISK
001000080902     Ffnlbl01l  IF   E           K DISK
001100080826     Ftitas30c  IF   E           K DISK    extfile(WTAS30) usropn
001200080826     Ftitas32c  IF   E           K DISK    extfile(WTAS32) usropn
001300080826     f                                     rename(titas000:titas002)
001400080826     f                                     rename(titas010:titas012)
001500080826     f                                     rename(titasp00:titasp02)
001600080826     Ftita435c  IF   E           K DISK    extfile(WTA435) usropn
001700080826     Ffiar531c  uF a E           K DISK    extfile(WFIAR5) usropn
001800080319     d*-----------------
001900080807      // - Tasti funzionali a video
002000080807     d c_F01           c                   const(x'31')
002100080807     d c_F02           c                   const(x'32')
002200080807     d c_F03           c                   const(x'33')
002300080807     d c_F05           c                   const(x'35')
002400080807     d c_F06           c                   const(x'36')
002500080807     d c_F07           c                   const(x'37')
002600080807     d c_F08           c                   const(x'38')
002700080807     d c_F09           c                   const(x'39')
002800080807     d c_F10           c                   const(x'3A')
002900080807     d c_F12           c                   const(x'3C')
003000080807     d c_F13           c                   const(x'B1')
003100080807     d c_F14           c                   const(x'B2')
003200080807     d c_F15           c                   const(x'B3')
003300080807     d c_F16           c                   const(x'B4')
003400080807     d c_F17           c                   const(x'B5')
003500080807     d c_F18           c                   const(x'B6')
003600080807     d c_F19           c                   const(x'B7')
003700080807     d c_F20           c                   const(x'B8')
003800080807     d c_F21           c                   const(x'B9')
003900080807     d c_F22           c                   const(x'BA')
004000080807     d c_F23           c                   const(x'BB')
004100080807     d c_F24           c                   const(x'BC')
004200080807     d InfDspF         ds
004300080807     d  dsp_aid              369    369a
004400080807     d  sfl_rrn              376    377i 0
004500080807     d  min_nrr              378    379i 0
004600080807     d  num_rcds             380    381i 0
004700080807
004800080807      // - Indicatori su DspF
004900080807     d IndDspF         ds
005000080807        // - Indicatori di errore
005100080807     d  errMessage                    1n   overlay(IndDspF : 28)
005200120329     d  errGenerico                   1n   overlay(IndDspF : 99)
005300080807        // - Indicatori vari
005400080807     d  Protvid1                      1n   overlay(IndDspF : 01)
005500080807     d  vistotbo                      1n   overlay(IndDspF : 02)
005600120320     d  Protvid2                      1n   overlay(IndDspF : 03)
005700080807     d  VisNumer                      1n   overlay(IndDspF : 05)
005800080807     d  Visalfa                       1n   overlay(IndDspF : 06)
005900080807     d  Vissped                       1n   overlay(IndDspF : 07)
006000110520     d  poscurs                       1n   overlay(IndDspF : 08)
006100120320     d  IndManut                      1n   overlay(IndDspF : 10)
006200110524     d  ErrBar1                       1n   overlay(IndDspF : 40)
006300110524     d  ErrBar2                       1n   overlay(IndDspF : 41)
006400110524     d  ErrBar3                       1n   overlay(IndDspF : 42)
006500110524     d  ErrBar4                       1n   overlay(IndDspF : 43)
006600110524     d  ErrBar5                       1n   overlay(IndDspF : 44)
006700110524     d  ErrBar6                       1n   overlay(IndDspF : 45)
006800110524     d  ErrBar7                       1n   overlay(IndDspF : 46)
006900120330     d  ErrBar8                       1n   overlay(IndDspF : 47)
007000110524     d  ErrKSC                        1n   overlay(IndDspF : 50)
007100080826     d  ErrSPE                        1n   overlay(IndDspF : 51)
007200110520     d  ErrBar                        1n   overlay(IndDspF : 52)
007300120328     d  ErrDRDT                       1n   overlay(IndDspF : 53)
007400120330     d  ErrMTV                        1n   overlay(IndDspF : 54)
007500080807
007600080807     d*-----------------
007700120330     D MSG             S             78    DIM(19) CTDATA PERRCD(1)
007800080807     D SKI7R           S              2    DIM(50)
007900080807     d*-----------------
008000911010     D WLBDAT          DS
008100941221     D  G02DAT                 1      8  0
008200941221     D  G02INV                 9     16  0
008300941221     D  G02ERR                17     17
008400941221     D  G02TGI                18     22  0
008500110520     d
008600110520     d* ds per sottocampare il barcode della spedizione
008700110520     D ds_pis          DS
008800110520     D  ds_pisaas                     2  0
008900110520     D  ds_pislnp                     3  0
009000110520     D  ds_pisnrs                     2  0
009100110520     D  ds_pisnsp                     7  0
009200110520     D  ds_chkdgt                     1  0
009300080319     d
009400040604     D KPJBA         E DS
009500080807     D tibs34ds      E DS
009600080319     D ds7N          E DS
009700080807     D ds7PQRS       E DS
009800080807     d kscabi                         2    DIM(30)  overlay(ds7cdp)
009900080319     d
010000080807     d DS7R          e ds
010100080826     D Dar5GEN       E DS
010200110520     D trul28ds      E DS
010300120329     D tntb92ds      E DS
010400040614     D*
010500110520     d Datasys         s               d   datfmt(*iso) inz(*sys)
010600110520     d Dataiso         s               d   datfmt(*iso)
010700080826     d timeiso         s               t   timfmt(*iso)
010800080319     d Dataeur         s               d   datfmt(*eur)
010900080807     D W007A           S              7
011000080807     D $video          S              2    inz('D1')
011100080807     D $inzd01         S              1
011200080807     D $inzd02         S              1
011300080807     d $Fine           s               n   inz(*off)
011400080807     d Primavolta      s              1    inz
011500080807     D XX              S              3  0 inz
011600080807     D Indx            S              3  0 inz
011700110520     d barcode         s             14
011800110520     d secolo          s              2  0
011900110520     d Kanno           s              4  0
012000080807     d
012100080807     d  xpardut        s             30
012200080807     d  xparkut        s              1  0
012300080807     d  xparrag        s             48
012400080807     d  xparkcc        s              4  0
012500080807     d  xparsta        s              1  0
012600080807     d  xparflr        s             90
012700080807     d  xpardit        s              3
012800080807     d  xparnum        s              2  0
012900080807     d  xparkcm        s             80
013000080807     d  xparksm        s            140
013100080807     d  xparkdm        s             60
013200080807     d  xparesci       s              1
013300080807     d  xparerr        s              2
013400080807     d  xpariva        s             16
013500080807     d  xparcdf        s             16
013600080826
013700080826     d  contaBolle     s              2  0
013800080826     d  Wtitasp        s              1
013900080826     d  s_tasaas       s                   like(tasaas)
014000080826     d  s_taslnp       s                   like(taslnp)
014100080826     d  s_tasnrs       s                   like(tasnrs)
014200080826     d  s_tasnsp       s                   like(tasnsp)
014300080826     d  w5aas          s                   like(tasaas)
014400080826     d  w5lnp          s                   like(taslnp)
014500080826     d  w5nrs          s                   like(tasnrs)
014600080826     d  w5nsp          s                   like(tasnsp)
014700120330     d  b_w5aas        s                   like(tasaas)
014800120330     d  b_w5lnp        s                   like(taslnp)
014900120330     d  b_w5nrs        s                   like(tasnrs)
015000120330     d  b_w5nsp        s                   like(tasnsp)
015100080826     d
015200080826     d WTAS30          s             21    inz('GAITRAGRU /TITAS30C')
015300080826     d WTAS32          s             21    inz('GAITRAGRU /TITAS32C')
015400080826     d WTA435          s             21    inz('GAITRAGRU /TITA435C')
015500080826     d WFIAR5          s             21    inz('GAITRAGRU /FIAR531C')
015600080807      //---------------------------------------------------------------
015700080807      //?Definizione aree dati.
015800080807      //---------------------------------------------------------------
015900080807
016000080807      // - Dati utente
016100080807     d §AzUte        e ds                  extname(AZUTE00F)
016200080807     d                                     dtaara
016300080807     d §DatiUte      e ds                  extname(dDatiUte)
016400080807     d                                     dtaara
016500080807      //---------------------------------------------------------------
016600080807      //?Definizione procedure usate.
016700080807      //---------------------------------------------------------------
016800080807      /copy gaitrasrc/srcprotopr,XALFA3BR
016900080807      /copy gaitrasrc/srcprotopr,tibs34r
017000080807      /copy gaitrasrc/srcprotopr,trtb70r
017100110520      /copy gaitrasrc/srcprotopr,trul28r1
017200120329      /copy gaitrasrc/srcprotopr,tntb92r
017300080826
017400080826     ITITAS000      81
017500080826     ITITAS010      82
017600080826     ITITASP00      83
017700080826     ITITAS002      84
017800080826     ITITAS012      85
017900080826     ITITASP02      86
018000000000     C*---------------------------------------------------------------*
018100911010     C* INDICATORI
018200911010     C*---------------------------------------------------------------*
018300951115     C*  28    - EMETTE CAMPO MESSAGGI
018400040607     C*  40    - ERRORE
018500911010     C*  90    - ERRORE GENERICO
018600911010     C*---------------------------------------------------------------*
018700000000     C     *ENTRY        PLIST
018800000000     C                   PARM                    KPJBA
018900120320     c                   movel     kpjbu         parmanut          1
019000080807
019100080807      /free
019200080807
019300080807       // Operazioni iniziali
019400080807       exsr RoutInz;
019500080807
019600080807       // Gestione video
019700080807       DOW $Fine = *off;
019800080807         select;
019900080807
020000080807         // Videata di selezioni
020100080807           when $Video = 'D1';
020200080807             exsr GesD01;
020300080807
020400080807           when $Video = 'D2';
020500080807             exsr GesD02;
020600120320
020700120320           when $Video = 'D3';
020800120320             exsr GesD03;
020900080807
021000080807           other   ;
021100080807             $Fine = *on;
021200080807         endsl;
021300080807       ENDDO;
021400080807
021500080807       // Operazioni finali
021600080807       exsr RoutEnd;
021700080807
021800080807       //--------------------------------------------------------------
021900080807       //?Operazioni iniziali.
022000080807       //--------------------------------------------------------------
022100080807       BEGSR RoutInz;
022200080807       $inzD01=*on;
022300080807
022400080807       // Solo la prima volta
022500080807 1     if primavolta=' '   ;
022600080807
022700080807         // Reperimento dati job
022800080807         exsr DatiJob;
022900110520
023000110520        // data corrente  ;
023100110520        eval secolo=%int(%subst(%editc(%dec(datasys):'X'):1:2))  ;
023200080807
023300080807         // Carico tutte le particolarità consegne che prevedono
023400080807         //  la restituzione della BAM
023500080807
023600080807         clear XX   ;
023700080807         setll (1:'7R')  tabel00f    ;
023800080807         reade (1:'7R')  tabel00f    ;
023900080807
024000080807         dow not %eof(tabel00f)  ;
024100120320
024200080807         if tblflg=' '   ;
024300080807           ds7r=tbluni   ;
024400080807           if §7r1bf='B' ;
024500080807            xx=xx+1   ;
024600080807            SKI7r(xx)=tblkey   ;
024700080807           endif     ;
024800080807         endif           ;
024900080807
025000080807         reade (1:'7R')  tabel00f    ;
025100080807         enddo     ;
025200080826
025300080826         // apertura FIAR5 di sede   ;
025400080826           open(e)   FIAR531C           ;
025500080826           if      not %open(FIAR531C)  ;
025600080826             %subst(WFIAR5:7:4)='GRPS'    ;
025700080826             open fiar531c                ;
025800080826           endif                        ;
025900080807
026000080807 1       endif    ;
026100120320         //  in base al flag imposto la testata pgm
026200120320         if parmanut='M'   ;
026300120320         v1ctes='*  M A N U T E N Z I O N E  *'  ;
026400120320         IndManut=*on  ;
026500120320         ELSE   ;
026600120320         v1ctes='**   I M M I S S I O N E   **'  ;
026700120320         IndManut=*off ;
026800120320         ENDIF  ;
026900080807
027000080807         ENDSR;
027100080807
027200080807       //--------------------------------------------------------------
027300080807       //?Reperimento Dati del job (Utente/Operativi).
027400080807       //--------------------------------------------------------------
027500080807       BEGSR DatiJob;
027600080807
027700080807         in(E) §AzUte;
027800080807         if NOT %error;
027900080807           in(E) §DatiUte;
028000080807         endif;
028100080807         if %error or RSut = *blanks;
028200080807           clear TIBS34ds;
028300080807           tibs34r(tibs34ds);
028400080807           in §AzUte;
028500080807           in §DatiUte;
028600080807         endif;
028700080807
028800080807       ENDSR;
028900080807       //--------------------------------------------------------------
029000080807       //?Gestione video 1
029100080807       //--------------------------------------------------------------
029200080807       BEGSR GesD01;
029300080807
029400080807         // Inizializzazione videata
029500080807         if  $Inzd01 = *on;
029600080807            exsr InzD01;
029700080807            $Inzd01  = *off;
029800080807         endif;
029900080807
030000080807
030100080807         // Emissione Testata e Piede con tasti funzionali abilitati
030200080807         if  errGenerico = *off;
030300080807           write  lsv1z03;
030400080807         endif;
030500080807
030600080807         // Emissione videata
030700080807         exfmt  lsv1d01;
030800080807
030900080807         reset errMessage;
031000080807         reset errGenerico;
031100080807         clear V1cmsg;
031200080807         errksc=*off     ;
031300080807
031400080807 1       SELECT;
031500080807
031600080807         // - F3=Fine
031700080807 1         WHEN  dsp_aid = c_F03;
031800080807            $Fine = *on;
031900080807
032000120329 1         WHEN  dsp_aid = c_F08;
032100120329            clear tntb92ds  ;
032200120402            b92ke1=%editc(dutpou:'X')  ;
032300120329            kpjbu=tntb92ds ;
032400120329            callp tntb92r (kpjba ) ;
032500120329
032600080807 x1      // Invio
032700080807           OTHER;
032800080807             exsr ContrD01 ;
032900080807 2           if  errGenerico = *on;
033000080807               leavesr;
033100080807 2           endif;
033200080807
033300080807           $Video = 'D2';
033400080807           $Inzd02 = *on;
033500080807
033600080807 1       ENDSL;
033700080807
033800080807       ENDSR;
033900080807       //--------------------------------------------------------------
034000080807       //?controlli video1
034100080807       //--------------------------------------------------------------
034200080807       BEGSR ContrD01 ;
034300080807       select    ;
034400080807       when v1cksc=0 and v1dksc=*blanks               ;
034500080807           errksc=*on;
034600080807           v1cmsg = Msg(01);
034700080807           errMessage  = *on;
034800080807           errGenerico = *on;
034900080807           leavesr;
035000080807
035100080807       when v1cksc=0 and v1dksc<>*blanks   ;
035200080807       xparsta=9   ;
035300080807       xpardut=rsut   ;
035400080807       clear xparrag   ;
035500080807       xparrag  =v1dksc  ;
035600080807       xparkcc=dutkci    ;
035700080807       clear xparflr  ;
035800080807       clear xpardit  ;
035900080807       xparnum=1     ;
036000080807       clear xparkcm ;
036100080807       clear xparksm ;
036200080807       clear xparkdm ;
036300080807       clear xparesci;
036400080807       clear xparerr ;
036500080807       clear xpariva ;
036600080807       clear xparcdf ;
036700080807       xalfa3br (xpardut:xparkut:xparrag:xparkcc:xparsta:xparflr:xpardit
036800080807                 :xparnum:xparkcm:xparksm:xparkdm:xparesci:xparerr:xpariva
036900080807                 :xparcdf)             ;
037000080807       if xparsta<>-1  ;
037100080807       w007a=XPARKSM  ;
037200080807       V1CKSC=%int(W007A)  ;
037300080807       v1dksc=xparrag   ;
037400080807       else   ;
037500080807       clear v1dksc    ;
037600080807       endif    ;
037700080807       ErrGenerico=*on   ;
037800080807       write lsv1z03   ;
037900080807       leavesr   ;
038000080807
038100080807       other     ;
038200080807       chain   (1:dutkci:v1cksc)   cnaco000   ;
038300080807       if   not %found(cnaco00f) or acoflg<>' '    ;
038400080807           errksc=*on;
038500080807           v1cmsg = Msg(02);
038600080807           errMessage  = *on;
038700080807           errGenerico = *on;
038800080807           clear v1dksc    ;
038900080807           leavesr;
039000080807       endif   ;
039100080807
039200080807       v1dksc=acorag   ;
039300080807       ENDSL   ;
039400080807
039500080807       // Verifico se il cliente prevede la particolarità giusta
039600080807       clear ds7pqrs   ;
039700080807       ds7ksc=v1cksc   ;
039800080826       ds7ges='O'      ;
039900080807       ds7ric='S'      ;
040000080807       kpjbu=ds7pqrs   ;
040100080807       TRTB70R (KPJBA)   ;
040200080807       ds7pqrs=kpjbu     ;
040300080807
040400080807       xx=1   ;
040500080807       clear Indx    ;
040600080807       dow kscabi(xx)<>*blanks  and Indx=0;
040700080807       Indx=%lookup(kscabi(xx):SKI7R)  ;
040800080807       xx=xx+1  ;
040900080807       enddo    ;
041000080807
041100080807       if Indx=0   ;
041200080807           errksc=*on;
041300080807           v1cmsg = Msg(03);
041400080807           errMessage  = *on;
041500080807           errGenerico = *on;
041600080807           leavesr;
041700080807       endif       ;
041800080807
041900080807       ENDSR   ;
042000080807       //--------------------------------------------------------------
042100080807       //?Inizializzazione video1
042200080807       //--------------------------------------------------------------
042300080807       BEGSR InzD01;
042400080807
042500080807         Protvid1 =*off   ;
042600120320         Protvid2 =*off   ;
042700080827         vistotbo =*off   ;
042800080807         clear v1cksc;
042900080807         clear v1dksc;
043000080807         v1ctip='A'  ;
043100080807       ENDSR  ;
043200080807       //--------------------------------------------------------------
043300080807       //?Gestione video 2
043400080807       //--------------------------------------------------------------
043500080807       BEGSR GesD02;
043600080807
043700080807         // Inizializzazione videata
043800080807         if  $Inzd02 = *on;
043900080807            exsr InzD02;
044000080807            $Inzd02  = *off;
044100080807         endif;
044200080807
044300080807
044400080807         // Emissione Testata e Piede con tasti funzionali abilitati
044500080807         if  errGenerico = *off;
044600080807           write  lsv1z03;
044700080807           write  lsv1d01;
044800080807         endif;
044900080807
045000080807         // Emissione videata
045100080807         exfmt  lsv1d02;
045200080807
045300080827         errSPE=*off     ;
045400110520         errBAR=*off     ;
045500080827         reset errMessage;
045600080807         reset errGenerico;
045700080807         clear V1cmsg;
045800080807
045900080807 1       SELECT;
046000080807
046100080807         // - F3=Fine
046200080807 1         WHEN  dsp_aid = c_F03;
046300080807            $Fine = *on;
046400120329
046500120329 1         WHEN  dsp_aid = c_F08;
046600120329            clear tntb92ds  ;
046700120402            b92ke1=%editc(dutpou:'X')  ;
046800120329            kpjbu=tntb92ds ;
046900120329            callp tntb92r (kpjba ) ;
047000120329
047100080807
047200080807 1         WHEN  dsp_aid = c_F12;
047300080807            $Video= 'D1' ;
047400080826            Protvid1=*off  ;
047500080827            vistotbo=*off  ;
047600080826            close(e) titas32c   ;
047700080826            close(e) titas30c   ;
047800080826            close(e) tita435c   ;
047900080807
048000080807 x1      // Invio
048100080807           OTHER;
048200080807             exsr ContrD02 ;
048300080807 2           if  errGenerico = *on;
048400080807               leavesr;
048500080807 2           endif;
048600080807
048700080807 1       ENDSL;
048800080807
048900080807       ENDSR;
049000120320       //--------------------------------------------------------------
049100120320       //?Gestione video 2
049200120320       //--------------------------------------------------------------
049300120320       BEGSR GesD03;
049400120320
049500120320            Protvid2=*on   ;
049600120320
049700120320         // Emissione Testata e Piede con tasti funzionali abilitati
049800120320         if  errGenerico = *off;
049900120320           write  lsv1z03;
050000120320           write  lsv1d01;
050100120320           write  lsv1d02;
050200120320         endif;
050300120320
050400120320         // Emissione videata
050500120320         exfmt  lsv1d04;
050600120328
050700120328         Errdrdt=*off  ;
050800120330         ErrMTV =*off  ;
050900120320         reset errMessage;
051000120320         reset errGenerico;
051100120320         clear V1cmsg;
051200120328
051300120320
051400120320 1       SELECT;
051500120320
051600120320         // - F3=Fine
051700120320 1         WHEN  dsp_aid = c_F03;
051800120320            $Fine = *on;
051900120329
052000120329 1         WHEN  dsp_aid = c_F08;
052100120329            clear tntb92ds  ;
052200120402            b92ke1=%editc(dutpou:'X')  ;
052300120329            kpjbu=tntb92ds ;
052400120329            callp tntb92r (kpjba ) ;
052500120320
052600120320 1         WHEN  dsp_aid = c_F12;
052700120320            $Video= 'D2' ;
052800120320            Protvid2=*off  ;
052900120402            exsr Inzparz02  ;
053000120328
053100120328           // elimino data inventario documenti se presente
053200120328 1         WHEN  dsp_aid = c_F16;
053300120328           exsr Annuldata  ;
053400120320
053500120320 x1      // Invio
053600120320           OTHER;
053700120320             exsr ContrD03 ;
053800120320 2           if  errGenerico = *on;
053900120320               leavesr;
054000120320 2           endif;
054100120328             if dsp_aid = c_F06;
054200120328              exsr Aggiomotivo  ;
054300120329              $Video= 'D2' ;
054400120329              Protvid2=*off  ;
054500120328             endif  ;
054600120320
054700120320 1       ENDSL;
054800120320
054900120328         Protvid2=*off  ;
055000120320       ENDSR;
055100120328       //--------------------------------------------------------------
055200120328       //?F16 - eliminazione data inventario documenti
055300120328       //--------------------------------------------------------------
055400120328       BEGSR  Annuldata   ;
055500120329
055600120328       // se data non presente errore
055700120329 1         if v1cdrdt=0  ;
055800120328           errdrdt=*on;
055900120328           v1cmsg = Msg(09);
056000120328           errMessage  = *on;
056100120328           errGenerico = *on;
056200120328           leavesr;
056300120329 1         endif  ;
056400120328
056500120328       // Updato pulendo la data   ;
056600120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
056700120329 1     if %found(fiar531c)  ;
056800120328       dar5gen=ar5uni           ;
056900120328
057000120330       §ar5drdt='99999999'      ;
057100120328       clear §ar5fstd           ;
057200120328       ar5uni=dar5gen           ;
057300120329
057400120329 2       if wtitasp='S'  ;
057500120329          update fiar5p00           ;
057600120329           else    ;
057700120329          update fiar5000           ;
057800120329 2     endif    ;
057900120328
058000120328 1     endif    ;
058100120329
058200120329       clear v1cdrdt   ;
058300120328
058400120328       ENDSR  ;
058500120328       //--------------------------------------------------------------
058600120328       //?F06 - Aggiorno motivo ddt errati
058700120328       //--------------------------------------------------------------
058800120328       BEGSR  Aggiomotivo ;
058900120328
059000120402 1     if §ar5mtvdt  <> %editc(v1cfil:'X')+v1cmtvdt   ;
059100120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
059200120328
059300120402 2     if not %found(fiar531c)  ;
059400120328       exsr ImpostaAR5          ;
059500120402 x2    else                     ;
059600120328       dar5gen=ar5uni           ;
059700120402 2     endif                    ;
059800120328
059900120402        if v1cmtvdt= *blanks  ;
060000120329         clear §ar5dimtv  ;
060100120402         clear §ar5mtvdt  ;
060200120329        else   ;
060300120402         §ar5mtvdt=%editc(v1cfil:'X') +v1cmtvdt   ;
060400120329         dataiso=%date             ;
060500120329         §ar5dimtv=%char(%dec(dataiso))   ;
060600120329        endif    ;
060700120329
060800120329       ar5uni=dar5gen  ;
060900120328
061000120329       exsr UpdateAR5  ;
061100120329
061200120402       endif    ;
061300120328
061400120402       exsr InzParz02 ;
061500120328
061600120328       ENDSR  ;
061700080807       //--------------------------------------------------------------
061800080807       //?Inizializzazione video2
061900080807       //--------------------------------------------------------------
062000080807       BEGSR InzD02;
062100080807
062200120330         exsr Inzparz02 ;
062300080807         Protvid1=*on   ;
062400080807         clear v1crmn;
062500080807         clear v1crma;
062600080807         clear v1clnp;
062700080807         clear v1cnrs;
062800080807         clear v1cnsp;
062900110520         clear v1cpis;
063000080826         v1caas=%subdt(%date(*date):*y);
063100080807         clear v1ctot;
063200080807
063300080807         visNumer=*off    ;
063400080807         visAlfa =*off    ;
063500080807         vissped =*off    ;
063600110520         poscurs =*off    ;
063700080807         vistotBo=*off    ;
063800120328
063900120328         // pulisco campi video 3
064000120328            clear v1cdrdt  ;
064100120328            clear v1cmtvdt ;
064200120328            clear v1dmtvdt ;
064300120328            clear v1cdimtv ;
064400120402            v1cfil=dutpou  ;
064500080807
064600080807         select   ;
064700080807         when v1ctip='N'   ;
064800080826           visNumer  =*on   ;
064900110524           if  %subst(knsif:7:1)='P' ;
065000110524             %subst(WTAS32:7:4)='GRPS'    ;
065100110524           endif   ;
065200110524
065300110524           open(e)   titas32c           ;
065400080826           if      not %open(titas32c)  ;
065500080826             %subst(WTAS32:7:4)='GRPS'    ;
065600080826             open titas32c                ;
065700080826           endif                        ;
065800080807
065900080807         when v1ctip='A'  ;
066000080826           visAlfa =*on   ;
066100110524           if  %subst(knsif:7:1)='P' ;
066200110524             %subst(WTA435:7:4)='GRPS'    ;
066300110524             %subst(WTAS30:7:4)='GRPS'    ;
066400110524           endif   ;
066500110524
066600080826           open(e)   tita435c           ;
066700110524
066800080826           if      not %open(tita435c)  ;
066900080826             %subst(WTA435:7:4)='GRPS'    ;
067000080826             %subst(WTAs30:7:4)='GRPS'    ;
067100080826             open tita435c                ;
067200080826             open titas30c                ;
067300080826           else   ;
067400080826             open titas30c                ;
067500080826           endif                        ;
067600080826
067700080807         other   ;
067800080826           visSped =*on   ;
067900110520           // se barcode accendo indicatore per posizionamento cursore
068000110520           if v1ctip='B'   ;
068100110520             poscurs=*on  ;
068200110520           endif  ;
068300110520
068400110524           if  %subst(knsif:7:1)='P' ;
068500110524             %subst(WTAS30:7:4)='GRPS'    ;
068600110524           endif   ;
068700110524
068800080826           open(e)   titas30c           ;
068900080826           if      not %open(titas30c)  ;
069000080826             %subst(WTAS30:7:4)='GRPS'    ;
069100080826             open titas30c                ;
069200080826           endif                        ;
069300080807         endsl   ;
069400080807
069500080807       ENDSR  ;
069600080807       //--------------------------------------------------------------
069700080807       //?controlli   video2
069800080807       //--------------------------------------------------------------
069900080807       BEGSR Contrd02 ;
070000080826
070100080901       *in81=*off  ;
070200080901       *in82=*off  ;
070300080901       *in83=*off  ;
070400080901       *in84=*off  ;
070500080901       *in85=*off  ;
070600080901       *in86=*off  ;
070700080826       clear Wtitasp      ;
070800080826       clear contabolle   ;
070900080826       clear s_tasaas     ;
071000080826       clear s_taslnp     ;
071100080826       clear s_tasnrs     ;
071200080826       clear s_tasnsp     ;
071300080826
071400080826 1     select              ;
071500080826       // Immissione per riferimenti mitt NUMERICO
071600080826 1     when visNumer=*on     ;
071700080826 2      if v1crmn=0    ;
071800080826           errSPE=*on;
071900080826           v1cmsg = Msg(04);
072000080826           errMessage  = *on;
072100080826           errGenerico = *on;
072200080826           leavesr;
072300080826 2     endif               ;
072400080826
072500080826        setll (v1crmn:v1cksc) titas32c  ;
072600080826        reade (v1crmn:v1cksc) titas32c  ;
072700080826 2       dow   not %eof(titas32c)  ;
072800080826
072900080902         // Escludo se bolla figlia ;
073000080902         chain  (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l  ;
073100080902 2a      if not %found(fnlbl01l)    ;
073200080902
073300080826         // la tengo solo se ha particolarità giusta
073400080826         Indx=%lookup(tasgma:SKI7R)  ;
073500080826 3       if   Indx>0  and tasgma<>*blanks  ;
073600080826 4       if contabolle<99 ;
073700080826         contabolle=contabolle+1  ;
073800080826
073900080826         // Mi salvo se la bolla
074000080826 5         if s_tasaas=0    ;
074100080826            s_tasaas=tasaas  ;
074200080826            s_taslnp=taslnp  ;
074300080826            s_tasnrs=tasnrs  ;
074400080826            s_tasnsp=tasnsp  ;
074500080901 6           if *in86=*on ;
074600080826             wtitasp='S'   ;
074700080826 6           endif   ;
074800080826 5         endif   ;
074900080826 4       endif   ;
075000080826 3       endif   ;
075100080902 2a      endif   ;
075200080826
075300080901       *in84 =*off  ;
075400080901       *in85 =*off  ;
075500080901       *in86 =*off  ;
075600080826        reade (v1crmn:v1cksc) titas32c  ;
075700080826 2      enddo    ;
075800080826
075900080826 2      if contabolle<>1   ;
076000080826           errSPE=*on;
076100080826 3         if contabolle=0 ;
076200080826           v1cmsg = Msg(06);
076300080826           else      ;
076400080826           v1cmsg = Msg(05);
076500080826           %subst(v1cmsg:40:2)=%char(contabolle)  ;
076600080826 3         endif    ;
076700080826           errMessage  = *on;
076800080826           errGenerico = *on;
076900080826           leavesr;
077000080826 2      endif              ;
077100080901
077200080901        // Se la bolla è in TITASP non accetto più l'inserimento
077300080901        if wtitasp='S'   ;
077400080901           errSPE=*on;
077500080901           v1cmsg = Msg(15);
077600080901           errMessage  = *on;
077700080901           errGenerico = *on;
077800080901           leavesr;
077900080901        endif   ;
078000080826
078100080826       // Immissione per riferimenti mittente ALFANUMERICO
078200080826 1     when visAlfa =*on     ;
078300080826 2      if v1crma=*blanks ;
078400080826           errSPE=*on;
078500080826           v1cmsg = Msg(07);
078600080826           errMessage  = *on;
078700080826           errGenerico = *on;
078800080826           leavesr;
078900080826 2     endif               ;
079000080826
079100080826        setll (v1crma) tita435c  ;
079200080826        reade (v1crma) tita435c  ;
079300080826 2       dow   not %eof(tita435c)  ;
079400080901         *in81    =*off  ;
079500080901         *in82    =*off  ;
079600080901         *in83    =*off  ;
079700080826         chain  (ta4aas:ta4lnp:ta4nrs:ta4nsp) TITAS30C  ;
079800080826 3       if %found(titas30c) and tasccm=v1cksc and tasgma<>*blanks     ;
079900080902
080000080902         // Escludo se bolla figlia ;
080100080902         chain  (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l  ;
080200080902 3a      if not %found(fnlbl01l)    ;
080300080826
080400080826         // la tengo solo se ha particolarità giusta
080500080826         Indx=%lookup(tasgma:SKI7R)  ;
080600080826 4       if   Indx>0   ;
080700080826 5        if contabolle<99 ;
080800080826          contabolle=contabolle+1  ;
080900080826          // Memorizzo la prima bolla che trovo valida
081000080826 6         if s_tasaas=0    ;
081100080826            s_tasaas=tasaas  ;
081200080826            s_taslnp=taslnp  ;
081300080826            s_tasnrs=tasnrs  ;
081400080826            s_tasnsp=tasnsp  ;
081500080901 7           if *in83    =*on ;
081600080826             wtitasp='S'   ;
081700080826 7           endif   ;
081800080826 6         endif   ;
081900080826 5        endif   ;
082000080826 4       endif   ;
082100080902 3a      endif   ;
082200080902 3       endif   ;
082300080826
082400080826        reade (v1crma) tita435c  ;
082500080826 2      enddo    ;
082600080826
082700080826 2      if contabolle<>1   ;
082800080826           errSPE=*on;
082900080826 3         if contabolle=0 ;
083000080826           v1cmsg = Msg(06);
083100080826           else      ;
083200080826           v1cmsg = Msg(08);
083300080826           %subst(v1cmsg:42:2)=%char(contabolle)  ;
083400080826 3         endif    ;
083500080826           errMessage  = *on;
083600080826           errGenerico = *on;
083700080826           leavesr;
083800080826 2      endif              ;
083900080901
084000080901        // Se la bolla è in TITASP non accetto più l'inserimento
084100080901        if wtitasp='S'      ;
084200080901           errSPE=*on;
084300080901           v1cmsg = Msg(15);
084400080901           errMessage  = *on;
084500080901           errGenerico = *on;
084600080901           leavesr;
084700080901        endif   ;
084800080826
084900080826 x1    other  ;
085000080826
085100080826       // Immissione per numero di spedizione
085200110520 2      if (v1cnsp=0  and v1cpis=0 )  or
085300110520           (v1cnsp>0  and v1cpis<> 0 ) ;
085400080826           errSPE=*on;
085500080826           v1cmsg = Msg(10);
085600080826           errMessage  = *on;
085700080826           errGenerico = *on;
085800080826           leavesr;
085900080826 2     endif               ;
086000080826
086100110520       // Se pieno numero di spedizione --> uso quello altrimenti il barcode
086200110520       clear ds_pis ;
086300110520       if v1cpis<>0     ;
086400110524            ds_pis=%subst(%editc(v1cpis:'X'):4:15)  ;
086500110520       // controllo chkdigit
086600110520
086700110524       // barcode=%editc(v1cpis:'X') ;         //
086800110524         barcode=%subst(%editc(v1cpis:'X'):4:15)  ;
086900110520       clear trul28ds ;
087000110520       i28mod = 'BAR' ;
087100110520       i28cod = barcode  ;
087200110520       callp  TRUL28R1   (trul28ds) ;
087300110520
087400110520       if o28err<>*blanks or o28cod<>ds_pis    ;
087500110524           errBAR7=*on;
087600110520           errGenerico = *on;
087700110520           leavesr;
087800110520 2     endif               ;
087900110520 1     endif               ;
088000110520
088100080901         *in81    =*off  ;
088200080901         *in82    =*off  ;
088300080901         *in83    =*off  ;
088400110520         if v1cnsp>0   ;
088500080827         chain  (v1caas:v1clnp:v1cnrs:v1cnsp) TITAS30C  ;
088600110520         else  ;
088700110520         Kanno=(secolo*100)+ds_pisaas   ;
088800110520         chain  (kanno:ds_pislnp:ds_pisnrs:ds_pisnsp) TITAS30C  ;
088900110520         endif  ;
089000110520
089100080826 2       if    not %found(titas30c)  ;
089200110520         if v1cnsp>0   ;
089300110524           v1cmsg = Msg(11);
089400110524           ErrMessage  = *on;
089500080826           errSPE=*on;
089600110520           else  ;
089700110524           errBAR1=*on;
089800110520           endif  ;
089900110520
090000080826           errGenerico = *on;
090100080826           leavesr;
090200080826 x2      else         ;
090300080826 3       if tasccm<>v1cksc   ;
090400110520         if v1cnsp>0   ;
090500080826           errSPE=*on;
090600110524           v1cmsg = Msg(12);
090700110524           errMessage  = *on;
090800110520           else  ;
090900110524           errBAR2=*on;
091000110520           endif  ;
091100080826           errGenerico = *on;
091200080826           leavesr;
091300080826 3       endif    ;
091400080826
091500080826         // Se la bolla non ha una particolarità che prevede la restituz
091600080826         //  del DDT --> errore
091700080826         Indx=%lookup(tasgma:SKI7R)  ;
091800080826         if   Indx=0  or tasgma=*blanks  ;
091900110520         if v1cnsp>0   ;
092000110520           errSPE=*on;
092100110524           v1cmsg = Msg(14);
092200110524           errMessage  = *on;
092300110520           else  ;
092400110524           errBAR3=*on;
092500110520           endif  ;
092600080826           errGenerico = *on;
092700080826           leavesr;
092800080826       endif       ;
092900080902
093000120322         // Se la bolla è figlia errore
093100080902         chain  (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l  ;
093200080902         if %found(fnlbl01l)    ;
093300110520         if v1cnsp>0   ;
093400110520           errSPE=*on;
093500110524           v1cmsg = Msg(16);
093600110524           errMessage  = *on;
093700110520           else  ;
093800110524           errBAR4=*on;
093900110520           endif  ;
094000080902           errGenerico = *on;
094100080902           leavesr;
094200080902       endif       ;
094300080902
094400080901
094500080901        // Se la bolla è in TITASP non accetto più l'inserimento
094600080901        if *in83    =*on    ;
094700110520         if v1cnsp>0   ;
094800110520           errSPE=*on;
094900110524           v1cmsg = Msg(15);
095000110524           errMessage  = *on;
095100110520           else  ;
095200110524           errBAR5=*on;
095300110520           endif  ;
095400080901           errGenerico = *on;
095500080901           leavesr;
095600080901        endif   ;
095700080826
095800080826 2      endif              ;
095900080826
096000080826        // salvo il numero di spedizione
096100080826            s_tasaas=tasaas  ;
096200080826            s_taslnp=taslnp  ;
096300080826            s_tasnrs=tasnrs  ;
096400080826            s_tasnsp=tasnsp  ;
096500080901 2           if *in83    =*on ;
096600080826             wtitasp='S'   ;
096700080826 2           endif   ;
096800080826 1     ENDSL  ;
096900080826
097000120330       // Trovata la spedizione, avviso se già aggiornata in altra data
097100080826 1     if s_tasaas<>w5aas or s_taslnp<>w5lnp or s_tasnrs<>w5nrs or
097200080826        s_tasnsp<>w5nsp  ;
097300080826           w5aas=s_tasaas  ;
097400080826           w5lnp=s_taslnp  ;
097500080826           w5nrs=s_tasnrs  ;
097600080826           w5nsp=S_tasnsp  ;
097700080826
097800120329 2     if indManut=*off  ;
097900080826       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
098000120328       else   ;
098100120328       chain(n) (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
098200120329 2     endif  ;
098300120328
098400080826 2     if %found(fiar531c)   ;
098500080826       dar5gen=ar5uni        ;
098600080826       dataiso=%date             ;
098700120322
098800120322       //  Avviso se già aggiornata in altra data --> modo Immissione
098900120322 2a    if indManut=*off  ;
099000120322
099100080902 3      if §ar5drdt<>*blanks and §ar5drdt<>'99999999'   ;
099200120330 4       if visnumer=*off and visalfa=*off and v1cnsp=0 ;
099300120330           errBAR6=*on;
099400120330 x4      else  ;
099500120330           v1cmsg = Msg(13);
099600120330           DATAEUR=%DATE(%int(§ar5drdt):*iso) ;
099700120330           %subst(v1cmsg:36:10)=%editw(%dec(dataeur):'  /  /    ') ;
099800120330           errMessage  = *on;
099900120330           errSPE=*on;
100000120330 4        endif  ;
100100120330
100200080826           errGenerico = *on;
100300080826           leavesr;
100400080826 3      endif      ;
100500120330
100600120322
100700120322 x2a    else    ;
100800120328
100900120322        // In manutenzione imposto la data
101000120322 3      if §ar5drdt<>*blanks and §ar5drdt<>'99999999'   ;
101100120322
101200120322  4     monitor  ;
101300120322        dataiso=%date(%int(§ar5drdt))  ;
101400120322        on-error   ;
101500120326        clear dataiso  ;
101600120322  4     endmon  ;
101700120329
101800120329  x3    else   ;
101900120322        clear dataiso  ;
102000120329  3     endif  ;
102100120322
102200120329  3     if dataiso>*loval  ;
102300120326          dataeur=dataiso   ;
102400120328          V1CDrdt =%dec(dataeur)  ;
102500120329  3     endif  ;
102600120322
102700120328
102800120328        // Data inserimento motivo
102900120329 3      if §ar5dimtv<>*blanks   ;
103000120328
103100120328  4     monitor  ;
103200120328        dataiso=%date(%int(§ar5dimtv))  ;
103300120328        on-error   ;
103400120328        clear dataiso  ;
103500120328  4     endmon  ;
103600120329  x3    else   ;
103700120328        clear dataiso  ;
103800120329  3     endif  ;
103900120328
104000120329  3     if dataiso>*loval  ;
104100120328          dataeur=dataiso   ;
104200120328          V1CDimtv =%dec(dataeur)  ;
104300120329  3     endif  ;
104400120328
104500120328
104600120328        // Codice motivo
104700120402 3      if §ar5mtvdt<>*blanks   ;
104800120402
104900120402        v1cmtvdt=%subst(§ar5mtvdt:4:5)  ;
105000120402        v1cfil  =%int(%subst(§ar5mtvdt:1:3))  ;
105100120330        clear tbeke1 ;
105200120330        clear tbeke2 ;
105300120402        tbeke1=%editc(v1cfil:'X')  ;
105400120330        tbeke2=v1cmtvdt  ;
105500120330        chain ('MDE':tbeke1:tbeke2) tntbe01l  ;
105600120330         if %found(tntbe01l)  ;
105700120330          v1dmtvdt=tbeuni  ;
105800120330         endif  ;
1059001204023        endif  ;
106000120330
106100120329 2a     endif  ;
106200120322
106300080826 2      endif      ;
106400080826 1     endif                 ;
106500080826
106600120320       // Nessun errore --> Se manutenzione emetto 3° videata
106700120328 0     if IndManut=*on    ;
106800120320            $Video= 'D3' ;
106900120328 x0    else  ;
107000120330       // Trovata la spedizione, avviso se presente motivo che verrà tolto
107100120330 1     if §ar5mtvdt<>*blanks  and
107200120330         (s_tasaas<>b_w5aas or s_taslnp<>b_w5lnp or s_tasnrs<>b_w5nrs or
107300120330        s_tasnsp<>b_w5nsp)  ;
107400120330           b_w5aas=s_tasaas  ;
107500120330           b_w5lnp=s_taslnp  ;
107600120330           b_w5nrs=s_tasnrs  ;
107700120330           b_w5nsp=S_tasnsp  ;
107800120330 2       if visnumer=*off and visalfa=*off and v1cnsp=0 ;
107900120330           errBAR8=*on;
108000120330 x2      else  ;
108100120330           v1cmsg = Msg(19);
108200120521           %subst(v1cmsg:35:8)=§ar5mtvdt  ;
108300120330           errMessage  = *on;
108400120330           errSPE=*on;
108500120330 2        endif  ;
108600120330
108700120330           errGenerico = *on;
108800120330           leavesr;
108900120330 1      endif  ;
109000120320
109100080826       // Se nessun errore aggiorno con ENTER  fiar5 e contatore
109200080826 1     if not %found(fiar531c) or §ar5drdt<>%char(%dec(dataiso))   ;
109300080826
109400080826 2     if not %found(fiar531c)  ;
109500120328       exsr ImpostaAR5          ;
109600080826 x2    else                     ;
109700080826       dar5gen=ar5uni           ;
109800080826 2     endif                    ;
109900080826
110000080826       dataiso=%date             ;
110100080826       §ar5drdt=%char(%dec(dataiso))   ;
110200080826       clear §ar5fstd           ;
110300120330       clear §ar5mtvdt          ;
110400120330       clear §ar5dimtv          ;
110500080826       ar5uni=dar5gen           ;
110600080826
110700120329       exsr UpdateAR5  ;
110800080826
110900080826       // totale bolle aggiornate
111000080826       v1ctot=v1ctot+1    ;
111100080827       vistotbo=*on       ;
111200080827       else   ;
111300080827
111400080827       unlock fiar531c   ;
111500080826 1     endif    ;
111600080826
111700120329       exsr InzParz02  ;
111800120328  0      endif  ;
111900080826
112000080826
112100080807       ENDSR  ;
112200120320       //--------------------------------------------------------------
112300120320       //?controlli formato 03
112400120320       //--------------------------------------------------------------
112500120320       BEGSR Contrd03  ;
112600120330
112700120329         clear v1dmtvdt  ;
112800120330
112900120329       // Verifico '?' sulla causale
113000120329       Indx=%scan ('?':v1cmtvdt)  ;
1131001203301      if indx>0   ;
113200120329         clear v1cmtvdt  ;
113300120329         clear tntb92ds  ;
113400120329         b92fun ='1'  ;
113500120402         b92ke1=%editc(v1cfil:'X')  ;
113600120329         kpjbu=tntb92ds  ;
113700120329         callp tntb92r (kpjba)  ;
113800120329
113900120329       tntb92ds=kpjbu  ;
1140001203302      if b92ke2<>*blanks  ;
114100120329         v1cmtvdt=b92ke2  ;
114200120329         v1dmtvdt=b92des  ;
1143001203302      endif  ;
114400120329
1145001203301      endif   ;
114600120329
114700120330       // controllo causale se immessa
1148001204021      if v1cmtvdt<>*blanks ;
114900120330        clear tbeke1 ;
115000120330        clear tbeke2 ;
115100120402        tbeke1=%editc(v1cfil:'X')  ;
115200120330        tbeke2=v1cmtvdt  ;
115300120330
115400120330        chain ('MDE':tbeke1:tbeke2) tntbe01l  ;
1155001204022       if not %found(tntbe01l) or tbeatb<>*blanks   ;
115600120330           errMTV=*on;
115700120330           v1cmsg = Msg(18);
115800120330           errMessage  = *on;
115900120330           errGenerico = *on;
116000120330           leavesr    ;
1161001204022       endif  ;
116200120330
116300120330        v1dmtvdt=tbeuni  ;
116400120330
1165001204021      endif  ;
116600120330
116700120402       // Se tolto MOTIVO AZZERO LA DATA
1168001204021      if v1cmtvdt=*blANKS  ;
116900120402       clear v1cdimtv  ;
117000120402       v1cfil=dutpou  ;
117100120402x1     else   ;
117200120402
117300120402       // se motivo = a quello del file stessa data
1174001204022      if v1cmtvdt=%subst(§ar5mtvdt:4:5) and §ar5dimtv>*zeros  ;
117500120402        dataiso=%date(%int(§ar5dimtv))  ;
117600120402       else   ;
117700120402         dataiso=%date             ;
1178001204022      endif  ;
117900120402
118000120402          dataeur=dataiso   ;
118100120402          V1CDimtv =%dec(dataeur)  ;
1182001204021      endif  ;
118300120402
118400120320       ENDSR  ;
118500080807
118600080807       //--------------------------------------------------------------
118700080807       //?Operazioni finali.
118800080807       //--------------------------------------------------------------
118900080807       BEGSR RoutEnd;
119000080807
119100080807         *inLR = *on;
119200080826
119300080807         return;
119400080807
119500080807       ENDSR;
119600120328       //--------------------------------------------------------------
119700120328       //?Imposta campi FIar5 per la write
119800120328       //--------------------------------------------------------------
119900120328       BEGSR ImpostaAR5         ;
120000120328
120100120328       clear fiar5000           ;
120200120328       clear fiar5p00           ;
120300120328       ar5aas=s_tasaas          ;
120400120328       ar5lnp=s_taslnp          ;
120500120328       ar5nrs=s_tasnrs          ;
120600120328       ar5nsp=s_tasnsp          ;
120700120328       ar5trd='GEN'             ;
120800120328       dataiso=%date             ;
120900120328       ar5dac=%dec(dataiso)     ;
121000120328       timeiso=%time             ;
121100120328       ar5orc=%dec(timeiso)     ;
121200120328       ar5ft1='R'               ;
121300120328       ar5dt1=%dec(dataiso)     ;
121400120328       ar5ft2='R'               ;
121500120328       ar5dt2=%dec(dataiso)     ;
121600120328       ar5ft3='R'               ;
121700120328       ar5dt3=%dec(dataiso)     ;
121800120328
121900120328       clear dar5gen            ;
122000120328       ENDSR                   ;
122100120329       //--------------------------------------------------------------
122200120329       //?Aggiorno record FIAR5
122300120329       //--------------------------------------------------------------
122400120329       BEGSR  UpdateAR5  ;
122500120329
122600120329 2     if not %found(fiar531c)  ;
122700120329 3       if wtitasp='S'  ;
122800120329         write  fiar5p00           ;
122900120329          else    ;
123000120329         write  fiar5000           ;
123100120329 3       endif  ;
123200120329
123300120329 x2    else   ;
123400120329 3       if wtitasp='S'  ;
123500120329          update fiar5p00           ;
123600120329           else    ;
123700120329          update fiar5000           ;
123800120329 3     endif    ;
123900120329 2     endif    ;
124000120329
124100120329       ENDSR  ;
124200120329       //--------------------------------------------------------------
124300120329       //?Pulizia parziale campoi video 2
124400120329       //--------------------------------------------------------------
124500120329       BEGSR InzParz02 ;
124600120329
124700120329         clear v1crmn;
124800120329         clear v1crma;
124900120329         clear v1cnsp;
125000120329         clear v1cpis;
125100120329         clear w5aas ;
125200120329         clear w5lnp ;
125300120329         clear w5nrs ;
125400120329         clear w5nsp ;
125500120330         clear b_w5aas ;
125600120330         clear b_w5lnp ;
125700120330         clear b_w5nrs ;
125800120330         clear b_w5nsp ;
125900120402         clear v1cdrdt  ;
126000120402         clear v1cmtvdt ;
126100120402         clear v1dmtvdt ;
126200120402         clear v1cdimtv ;
126300120402         v1cfil=dutpou  ;
126400120329       ENDSR  ;
126500080807
126600080807      /end-free
126700080807
126800080807
126900000606**  MSG
127000080826Immettere il codice Cliente Mittente                                           1
127100080826Codice Cliente Mittente inesistente                                            2
127200080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
127300080826Immettere il riferimento mittente NUMERICO                                     4
127400080826Per il rif. Mittente NUMERICO esistono xx bolle: inserire col numero di Spediz 5
127500080909Nessuna bolla trovata per il cliente con particolarità restituzione documenti  6
127600080826Immettere il riferimento mittente ALFANUMERICO                                 7
127700080826Per il RifMittente ALFANUMERICO esistono xx bolle:inserire col numero di Sped  8
127800120328Impossibile annullare : data non presente!                                     9
127900110520Immettere il numero di SPEDIZIONE  o il Barcode, in alternativa               10
128000080826Numero di SPEDIZIONE inesistente                                               1
128100080826La SPEDIZIONE non appartiene al cliente mittente indicato                      2
128200120521Già presente data ricezione doc.al xx/xx/xxxx:ENTER per forzare e riaggiornare 3
128300080909La SPEDIZIONE non prevede particolarità consegna con restituzione documenti    4
128400080901La spedizione è ormai troppo vecchia: impossibile eseguire l'inventario        5
128500120521Spedizione con legame bolla:inserire ricezione documenti sulla bolla originale 6
128600110520Errore nel Barcode                                                             7
128700120330Causale motivo inesistente                                                     8
128800120521Presente motivo documenti errati "xxxxxxxx": verrà eliminato                   9
