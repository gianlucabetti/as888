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
006200120706     d  Abistampa                     1n   overlay(IndDspF : 12)
006300110524     d  ErrBar1                       1n   overlay(IndDspF : 40)
006400110524     d  ErrBar2                       1n   overlay(IndDspF : 41)
006500110524     d  ErrBar3                       1n   overlay(IndDspF : 42)
006600110524     d  ErrBar4                       1n   overlay(IndDspF : 43)
006700110524     d  ErrBar5                       1n   overlay(IndDspF : 44)
006800110524     d  ErrBar6                       1n   overlay(IndDspF : 45)
006900110524     d  ErrBar7                       1n   overlay(IndDspF : 46)
007000120330     d  ErrBar8                       1n   overlay(IndDspF : 47)
007100110524     d  ErrKSC                        1n   overlay(IndDspF : 50)
007200080826     d  ErrSPE                        1n   overlay(IndDspF : 51)
007300110520     d  ErrBar                        1n   overlay(IndDspF : 52)
007400120328     d  ErrDRDT                       1n   overlay(IndDspF : 53)
007500120330     d  ErrMTV                        1n   overlay(IndDspF : 54)
007600080807
007700080807     d*-----------------
007800120330     D MSG             S             78    DIM(19) CTDATA PERRCD(1)
007900080807     D SKI7R           S              2    DIM(50)
008000080807     d*-----------------
008100911010     D WLBDAT          DS
008200941221     D  G02DAT                 1      8  0
008300941221     D  G02INV                 9     16  0
008400941221     D  G02ERR                17     17
008500941221     D  G02TGI                18     22  0
008600110520     d
008700110520     d* ds per sottocampare il barcode della spedizione
008800110520     D ds_pis          DS
008900110520     D  ds_pisaas                     2  0
009000110520     D  ds_pislnp                     3  0
009100110520     D  ds_pisnrs                     2  0
009200110520     D  ds_pisnsp                     7  0
009300110520     D  ds_chkdgt                     1  0
009400080319     d
009500040604     D KPJBA         E DS
009600080807     D tibs34ds      E DS
009700080319     D ds7N          E DS
009800080807     D ds7PQRS       E DS
009900080807     d kscabi                         2    DIM(30)  overlay(ds7cdp)
010000080319     d
010100080807     d DS7R          e ds
010200080826     D Dar5GEN       E DS
010300110520     D trul28ds      E DS
010400120329     D tntb92ds      E DS
010500120625     D tnsb25ds      E DS
010600040614     D*
010700110520     d Datasys         s               d   datfmt(*iso) inz(*sys)
010800110520     d Dataiso         s               d   datfmt(*iso)
010900080826     d timeiso         s               t   timfmt(*iso)
011000080319     d Dataeur         s               d   datfmt(*eur)
011100080807     D W007A           S              7
011200080807     D $video          S              2    inz('D1')
011300080807     D $inzd01         S              1
011400080807     D $inzd02         S              1
011500080807     d $Fine           s               n   inz(*off)
011600080807     d Primavolta      s              1    inz
011700080807     D XX              S              3  0 inz
011800080807     D Indx            S              3  0 inz
011900110520     d barcode         s             14
012000110520     d secolo          s              2  0
012100110520     d Kanno           s              4  0
012200080807     d
012300080807     d  xpardut        s             30
012400080807     d  xparkut        s              1  0
012500080807     d  xparrag        s             48
012600080807     d  xparkcc        s              4  0
012700080807     d  xparsta        s              1  0
012800080807     d  xparflr        s             90
012900080807     d  xpardit        s              3
013000080807     d  xparnum        s              2  0
013100080807     d  xparkcm        s             80
013200080807     d  xparksm        s            140
013300080807     d  xparkdm        s             60
013400080807     d  xparesci       s              1
013500080807     d  xparerr        s              2
013600080807     d  xpariva        s             16
013700080807     d  xparcdf        s             16
013800080826
013900080826     d  contaBolle     s              2  0
014000120706     d  Wprof          s              3
014100080826     d  Wtitasp        s              1
014200080826     d  s_tasaas       s                   like(tasaas)
014300080826     d  s_taslnp       s                   like(taslnp)
014400080826     d  s_tasnrs       s                   like(tasnrs)
014500080826     d  s_tasnsp       s                   like(tasnsp)
014600080826     d  w5aas          s                   like(tasaas)
014700080826     d  w5lnp          s                   like(taslnp)
014800080826     d  w5nrs          s                   like(tasnrs)
014900080826     d  w5nsp          s                   like(tasnsp)
015000120330     d  b_w5aas        s                   like(tasaas)
015100120330     d  b_w5lnp        s                   like(taslnp)
015200120330     d  b_w5nrs        s                   like(tasnrs)
015300120330     d  b_w5nsp        s                   like(tasnsp)
015400080826     d
015500080826     d WTAS30          s             21    inz('GAITRAGRU /TITAS30C')
015600080826     d WTAS32          s             21    inz('GAITRAGRU /TITAS32C')
015700080826     d WTA435          s             21    inz('GAITRAGRU /TITA435C')
015800080826     d WFIAR5          s             21    inz('GAITRAGRU /FIAR531C')
015900080807      //---------------------------------------------------------------
016000080807      //?Definizione aree dati.
016100080807      //---------------------------------------------------------------
016200080807
016300080807      // - Dati utente
016400080807     d §AzUte        e ds                  extname(AZUTE00F)
016500080807     d                                     dtaara
016600080807     d §DatiUte      e ds                  extname(dDatiUte)
016700080807     d                                     dtaara
016800080807      //---------------------------------------------------------------
016900080807      //?Definizione procedure usate.
017000080807      //---------------------------------------------------------------
017100080807      /copy gaitrasrc/srcprotopr,XALFA3BR
017200080807      /copy gaitrasrc/srcprotopr,tibs34r
017300080807      /copy gaitrasrc/srcprotopr,trtb70r
017400110520      /copy gaitrasrc/srcprotopr,trul28r1
017500120329      /copy gaitrasrc/srcprotopr,tntb92r
017600120625      /copy gaitrasrc/srcprotopr,tnsb25r
017700080826
017800080826     ITITAS000      81
017900080826     ITITAS010      82
018000080826     ITITASP00      83
018100080826     ITITAS002      84
018200080826     ITITAS012      85
018300080826     ITITASP02      86
018400000000     C*---------------------------------------------------------------*
018500911010     C* INDICATORI
018600911010     C*---------------------------------------------------------------*
018700120706     C*  10    - on manutenzione inventario
018800120706     C*  28    - EMETTE CAMPO MESSAGGI
018900040607     C*  40    - ERRORE
019000911010     C*  90    - ERRORE GENERICO
019100911010     C*---------------------------------------------------------------*
019200000000     C     *ENTRY        PLIST
019300000000     C                   PARM                    KPJBA
019400120320     c                   movel     kpjbu         parmanut          1
019500120706      /free
019600120706
019700120706       // Operazioni iniziali
019800120706       exsr RoutInz;
019900120706      /end-free
020000120706
020100120706     c* Posizione 2-3-4- kpjbu --> filiale abilitata a stampare LDV
020200120706     c*          Se '  ' ---> nessuno
020300120706     c*          Se 'TUT' --> tutti
020400120706     c*          Se 'xxx' --> la singola filiale
020500120706     c
020600120706     c                   eval      Wprof =%subst(kpjbu:2:3)
020700120706     c
020800120706     c                   if        wprof='TUT'
020900120706     c                   eval      AbiStampa=*on
021000120706     c                   endif
021100120706
021200120706     c                   if        %subst(knmus:1:3)='EDP'
021300120706     c                   eval      AbiStampa=*on
021400120706     c                   endif
021500120706     c
021600120706     c                   if        wprof>*zeros
021700120706     c                   movel     wprof         w0030             3 0
021800120706     c                   if        w0030=dutpou
021900120706     c                   eval      AbiStampa=*on
022000120706     c                   endif
022100120706     c                   endif
022200120706     c
022300080807
022400080807      /free
022500080807
022600080807       // Gestione video
022700080807       DOW $Fine = *off;
022800080807         select;
022900080807
023000080807         // Videata di selezioni
023100080807           when $Video = 'D1';
023200080807             exsr GesD01;
023300080807
023400080807           when $Video = 'D2';
023500080807             exsr GesD02;
023600120320
023700120320           when $Video = 'D3';
023800120320             exsr GesD03;
023900080807
024000080807           other   ;
024100080807             $Fine = *on;
024200080807         endsl;
024300080807       ENDDO;
024400080807
024500080807       // Operazioni finali
024600080807       exsr RoutEnd;
024700080807
024800080807       //--------------------------------------------------------------
024900080807       //?Operazioni iniziali.
025000080807       //--------------------------------------------------------------
025100080807       BEGSR RoutInz;
025200080807       $inzD01=*on;
025300080807
025400080807       // Solo la prima volta
025500080807 1     if primavolta=' '   ;
025600080807
025700080807         // Reperimento dati job
025800080807         exsr DatiJob;
025900110520
026000110520        // data corrente  ;
026100110520        eval secolo=%int(%subst(%editc(%dec(datasys):'X'):1:2))  ;
026200080807
026300080807         // Carico tutte le particolarità consegne che prevedono
026400080807         //  la restituzione della BAM
026500080807
026600080807         clear XX   ;
026700080807         setll (1:'7R')  tabel00f    ;
026800080807         reade (1:'7R')  tabel00f    ;
026900080807
027000080807         dow not %eof(tabel00f)  ;
027100120320
027200080807         if tblflg=' '   ;
027300080807           ds7r=tbluni   ;
027400080807           if §7r1bf='B' ;
027500080807            xx=xx+1   ;
027600080807            SKI7r(xx)=tblkey   ;
027700080807           endif     ;
027800080807         endif           ;
027900080807
028000080807         reade (1:'7R')  tabel00f    ;
028100080807         enddo     ;
028200080826
028300080826         // apertura FIAR5 di sede   ;
028400080826           open(e)   FIAR531C           ;
028500080826           if      not %open(FIAR531C)  ;
028600080826             %subst(WFIAR5:7:4)='GRPS'    ;
028700080826             open fiar531c                ;
028800080826           endif                        ;
028900080807
029000080807 1       endif    ;
029100120320         //  in base al flag imposto la testata pgm
029200120320         if parmanut='M'   ;
029300120320         v1ctes='*  M A N U T E N Z I O N E  *'  ;
029400120320         IndManut=*on  ;
029500120320         ELSE   ;
029600120320         v1ctes='**   I M M I S S I O N E   **'  ;
029700120320         IndManut=*off ;
029800120320         ENDIF  ;
029900080807
030000080807         ENDSR;
030100080807
030200080807       //--------------------------------------------------------------
030300080807       //?Reperimento Dati del job (Utente/Operativi).
030400080807       //--------------------------------------------------------------
030500080807       BEGSR DatiJob;
030600080807
030700080807         in(E) §AzUte;
030800080807         if NOT %error;
030900080807           in(E) §DatiUte;
031000080807         endif;
031100080807         if %error or RSut = *blanks;
031200080807           clear TIBS34ds;
031300080807           tibs34r(tibs34ds);
031400080807           in §AzUte;
031500080807           in §DatiUte;
031600080807         endif;
031700080807
031800080807       ENDSR;
031900080807       //--------------------------------------------------------------
032000080807       //?Gestione video 1
032100080807       //--------------------------------------------------------------
032200080807       BEGSR GesD01;
032300080807
032400080807         // Inizializzazione videata
032500080807         if  $Inzd01 = *on;
032600080807            exsr InzD01;
032700080807            $Inzd01  = *off;
032800080807         endif;
032900080807
033000080807
033100080807         // Emissione Testata e Piede con tasti funzionali abilitati
033200080807         if  errGenerico = *off;
033300080807           write  lsv1z03;
033400080807         endif;
033500080807
033600080807         // Emissione videata
033700080807         exfmt  lsv1d01;
033800080807
033900080807         reset errMessage;
034000080807         reset errGenerico;
034100080807         clear V1cmsg;
034200080807         errksc=*off     ;
034300080807
034400080807 1       SELECT;
034500080807
034600080807         // - F3=Fine
034700080807 1         WHEN  dsp_aid = c_F03;
034800080807            $Fine = *on;
034900080807
035000120329 1         WHEN  dsp_aid = c_F08;
035100120329            clear tntb92ds  ;
035200120402            b92ke1=%editc(dutpou:'X')  ;
035300120329            kpjbu=tntb92ds ;
035400120329            callp tntb92r (kpjba ) ;
035500120329
035600080807 x1      // Invio
035700080807           OTHER;
035800080807             exsr ContrD01 ;
035900080807 2           if  errGenerico = *on;
036000080807               leavesr;
036100080807 2           endif;
036200080807
036300080807           $Video = 'D2';
036400080807           $Inzd02 = *on;
036500080807
036600080807 1       ENDSL;
036700080807
036800080807       ENDSR;
036900080807       //--------------------------------------------------------------
037000080807       //?controlli video1
037100080807       //--------------------------------------------------------------
037200080807       BEGSR ContrD01 ;
037300080807       select    ;
037400080807       when v1cksc=0 and v1dksc=*blanks               ;
037500080807           errksc=*on;
037600080807           v1cmsg = Msg(01);
037700080807           errMessage  = *on;
037800080807           errGenerico = *on;
037900080807           leavesr;
038000080807
038100080807       when v1cksc=0 and v1dksc<>*blanks   ;
038200080807       xparsta=9   ;
038300080807       xpardut=rsut   ;
038400080807       clear xparrag   ;
038500080807       xparrag  =v1dksc  ;
038600080807       xparkcc=dutkci    ;
038700080807       clear xparflr  ;
038800080807       clear xpardit  ;
038900080807       xparnum=1     ;
039000080807       clear xparkcm ;
039100080807       clear xparksm ;
039200080807       clear xparkdm ;
039300080807       clear xparesci;
039400080807       clear xparerr ;
039500080807       clear xpariva ;
039600080807       clear xparcdf ;
039700080807       xalfa3br (xpardut:xparkut:xparrag:xparkcc:xparsta:xparflr:xpardit
039800080807                 :xparnum:xparkcm:xparksm:xparkdm:xparesci:xparerr:xpariva
039900080807                 :xparcdf)             ;
040000080807       if xparsta<>-1  ;
040100080807       w007a=XPARKSM  ;
040200080807       V1CKSC=%int(W007A)  ;
040300080807       v1dksc=xparrag   ;
040400080807       else   ;
040500080807       clear v1dksc    ;
040600080807       endif    ;
040700080807       ErrGenerico=*on   ;
040800080807       write lsv1z03   ;
040900080807       leavesr   ;
041000080807
041100080807       other     ;
041200080807       chain   (1:dutkci:v1cksc)   cnaco000   ;
041300080807       if   not %found(cnaco00f) or acoflg<>' '    ;
041400080807           errksc=*on;
041500080807           v1cmsg = Msg(02);
041600080807           errMessage  = *on;
041700080807           errGenerico = *on;
041800080807           clear v1dksc    ;
041900080807           leavesr;
042000080807       endif   ;
042100080807
042200080807       v1dksc=acorag   ;
042300080807       ENDSL   ;
042400080807
042500080807       // Verifico se il cliente prevede la particolarità giusta
042600080807       clear ds7pqrs   ;
042700080807       ds7ksc=v1cksc   ;
042800080826       ds7ges='O'      ;
042900080807       ds7ric='S'      ;
043000080807       kpjbu=ds7pqrs   ;
043100080807       TRTB70R (KPJBA)   ;
043200080807       ds7pqrs=kpjbu     ;
043300080807
043400080807       xx=1   ;
043500080807       clear Indx    ;
043600080807       dow kscabi(xx)<>*blanks  and Indx=0;
043700080807       Indx=%lookup(kscabi(xx):SKI7R)  ;
043800080807       xx=xx+1  ;
043900080807       enddo    ;
044000080807
044100080807       if Indx=0   ;
044200080807           errksc=*on;
044300080807           v1cmsg = Msg(03);
044400080807           errMessage  = *on;
044500080807           errGenerico = *on;
044600080807           leavesr;
044700080807       endif       ;
044800080807
044900080807       ENDSR   ;
045000080807       //--------------------------------------------------------------
045100080807       //?Inizializzazione video1
045200080807       //--------------------------------------------------------------
045300080807       BEGSR InzD01;
045400080807
045500080807         Protvid1 =*off   ;
045600120320         Protvid2 =*off   ;
045700080827         vistotbo =*off   ;
045800080807         clear v1cksc;
045900080807         clear v1dksc;
046000080807         v1ctip='A'  ;
046100120625         v1cldv='NO'  ;
046200080807       ENDSR  ;
046300080807       //--------------------------------------------------------------
046400080807       //?Gestione video 2
046500080807       //--------------------------------------------------------------
046600080807       BEGSR GesD02;
046700080807
046800080807         // Inizializzazione videata
046900080807         if  $Inzd02 = *on;
047000080807            exsr InzD02;
047100080807            $Inzd02  = *off;
047200080807         endif;
047300080807
047400080807
047500080807         // Emissione Testata e Piede con tasti funzionali abilitati
047600080807         if  errGenerico = *off;
047700080807           write  lsv1z03;
047800080807           write  lsv1d01;
047900080807         endif;
048000080807
048100080807         // Emissione videata
048200080807         exfmt  lsv1d02;
048300080807
048400080827         errSPE=*off     ;
048500110520         errBAR=*off     ;
048600080827         reset errMessage;
048700080807         reset errGenerico;
048800080807         clear V1cmsg;
048900080807
049000080807 1       SELECT;
049100080807
049200080807         // - F3=Fine
049300080807 1         WHEN  dsp_aid = c_F03;
049400080807            $Fine = *on;
049500120329
049600120329 1         WHEN  dsp_aid = c_F08;
049700120329            clear tntb92ds  ;
049800120402            b92ke1=%editc(dutpou:'X')  ;
049900120329            kpjbu=tntb92ds ;
050000120329            callp tntb92r (kpjba ) ;
050100120329
050200080807
050300080807 1         WHEN  dsp_aid = c_F12;
050400080807            $Video= 'D1' ;
050500080826            Protvid1=*off  ;
050600080827            vistotbo=*off  ;
050700080826            close(e) titas32c   ;
050800080826            close(e) titas30c   ;
050900080826            close(e) tita435c   ;
051000080807
051100080807 x1      // Invio
051200080807           OTHER;
051300080807             exsr ContrD02 ;
051400080807 2           if  errGenerico = *on;
051500080807               leavesr;
051600080807 2           endif;
051700080807
051800080807 1       ENDSL;
051900080807
052000080807       ENDSR;
052100120320       //--------------------------------------------------------------
052200120625       //?Gestione video 3
052300120320       //--------------------------------------------------------------
052400120320       BEGSR GesD03;
052500120320
052600120320            Protvid2=*on   ;
052700120320
052800120320         // Emissione Testata e Piede con tasti funzionali abilitati
052900120320         if  errGenerico = *off;
053000120320           write  lsv1z03;
053100120320           write  lsv1d01;
053200120320           write  lsv1d02;
053300120320         endif;
053400120320
053500120320         // Emissione videata
053600120320         exfmt  lsv1d04;
053700120328
053800120328         Errdrdt=*off  ;
053900120330         ErrMTV =*off  ;
054000120320         reset errMessage;
054100120320         reset errGenerico;
054200120320         clear V1cmsg;
054300120328
054400120320
054500120320 1       SELECT;
054600120320
054700120320         // - F3=Fine
054800120320 1         WHEN  dsp_aid = c_F03;
054900120320            $Fine = *on;
055000120329
055100120329 1         WHEN  dsp_aid = c_F08;
055200120329            clear tntb92ds  ;
055300120402            b92ke1=%editc(dutpou:'X')  ;
055400120329            kpjbu=tntb92ds ;
055500120329            callp tntb92r (kpjba ) ;
055600120320
055700120320 1         WHEN  dsp_aid = c_F12;
055800120320            $Video= 'D2' ;
055900120320            Protvid2=*off  ;
056000120402            exsr Inzparz02  ;
056100120328
056200120328           // elimino data inventario documenti se presente
056300120328 1         WHEN  dsp_aid = c_F16;
056400120328           exsr Annuldata  ;
056500120320
056600120320 x1      // Invio
056700120320           OTHER;
056800120320             exsr ContrD03 ;
056900120320 2           if  errGenerico = *on;
057000120320               leavesr;
057100120320 2           endif;
057200120328             if dsp_aid = c_F06;
057300120328              exsr Aggiomotivo  ;
057400120329              $Video= 'D2' ;
057500120329              Protvid2=*off  ;
057600120328             endif  ;
057700120320
057800120320 1       ENDSL;
057900120320
058000120328         Protvid2=*off  ;
058100120320       ENDSR;
058200120328       //--------------------------------------------------------------
058300120328       //?F16 - eliminazione data inventario documenti
058400120328       //--------------------------------------------------------------
058500120328       BEGSR  Annuldata   ;
058600120329
058700120328       // se data non presente errore
058800120329 1         if v1cdrdt=0  ;
058900120328           errdrdt=*on;
059000120328           v1cmsg = Msg(09);
059100120328           errMessage  = *on;
059200120328           errGenerico = *on;
059300120328           leavesr;
059400120329 1         endif  ;
059500120328
059600120328       // Updato pulendo la data   ;
059700120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
059800120329 1     if %found(fiar531c)  ;
059900120328       dar5gen=ar5uni           ;
060000120328
060100120330       §ar5drdt='99999999'      ;
060200120328       clear §ar5fstd           ;
060300120328       ar5uni=dar5gen           ;
060400120329
060500120329 2       if wtitasp='S'  ;
060600120329          update fiar5p00           ;
060700120329           else    ;
060800120329          update fiar5000           ;
060900120329 2     endif    ;
061000120328
061100120328 1     endif    ;
061200120329
061300120329       clear v1cdrdt   ;
061400120328
061500120328       ENDSR  ;
061600120328       //--------------------------------------------------------------
061700120328       //?F06 - Aggiorno motivo ddt errati
061800120328       //--------------------------------------------------------------
061900120328       BEGSR  Aggiomotivo ;
062000120328
062100120402 1     if §ar5mtvdt  <> %editc(v1cfil:'X')+v1cmtvdt   ;
062200120328       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
062300120328
062400120402 2     if not %found(fiar531c)  ;
062500120328       exsr ImpostaAR5          ;
062600120402 x2    else                     ;
062700120328       dar5gen=ar5uni           ;
062800120402 2     endif                    ;
062900120328
063000120402        if v1cmtvdt= *blanks  ;
063100120329         clear §ar5dimtv  ;
063200120402         clear §ar5mtvdt  ;
063300120329        else   ;
063400120402         §ar5mtvdt=%editc(v1cfil:'X') +v1cmtvdt   ;
063500120329         dataiso=%date             ;
063600120329         §ar5dimtv=%char(%dec(dataiso))   ;
063700120329        endif    ;
063800120329
063900120329       ar5uni=dar5gen  ;
064000120328
064100120329       exsr UpdateAR5  ;
064200120329
064300120402       endif    ;
064400120328
064500120402       exsr InzParz02 ;
064600120328
064700120328       ENDSR  ;
064800080807       //--------------------------------------------------------------
064900080807       //?Inizializzazione video2
065000080807       //--------------------------------------------------------------
065100080807       BEGSR InzD02;
065200080807
065300120330         exsr Inzparz02 ;
065400080807         Protvid1=*on   ;
065500080807         clear v1crmn;
065600080807         clear v1crma;
065700080807         clear v1clnp;
065800080807         clear v1cnrs;
065900080807         clear v1cnsp;
066000110520         clear v1cpis;
066100080826         v1caas=%subdt(%date(*date):*y);
066200080807         clear v1ctot;
066300080807
066400080807         visNumer=*off    ;
066500080807         visAlfa =*off    ;
066600080807         vissped =*off    ;
066700110520         poscurs =*off    ;
066800080807         vistotBo=*off    ;
066900120328
067000120328         // pulisco campi video 3
067100120328            clear v1cdrdt  ;
067200120328            clear v1cmtvdt ;
067300120328            clear v1dmtvdt ;
067400120328            clear v1cdimtv ;
067500120402            v1cfil=dutpou  ;
067600080807
067700080807         select   ;
067800080807         when v1ctip='N'   ;
067900080826           visNumer  =*on   ;
068000110524           if  %subst(knsif:7:1)='P' ;
068100110524             %subst(WTAS32:7:4)='GRPS'    ;
068200110524           endif   ;
068300110524
068400110524           open(e)   titas32c           ;
068500080826           if      not %open(titas32c)  ;
068600080826             %subst(WTAS32:7:4)='GRPS'    ;
068700080826             open titas32c                ;
068800080826           endif                        ;
068900080807
069000080807         when v1ctip='A'  ;
069100080826           visAlfa =*on   ;
069200110524           if  %subst(knsif:7:1)='P' ;
069300110524             %subst(WTA435:7:4)='GRPS'    ;
069400110524             %subst(WTAS30:7:4)='GRPS'    ;
069500110524           endif   ;
069600110524
069700080826           open(e)   tita435c           ;
069800110524
069900080826           if      not %open(tita435c)  ;
070000080826             %subst(WTA435:7:4)='GRPS'    ;
070100080826             %subst(WTAs30:7:4)='GRPS'    ;
070200080826             open tita435c                ;
070300080826             open titas30c                ;
070400080826           else   ;
070500080826             open titas30c                ;
070600080826           endif                        ;
070700080826
070800080807         other   ;
070900080826           visSped =*on   ;
071000110520           // se barcode accendo indicatore per posizionamento cursore
071100110520           if v1ctip='B'   ;
071200110520             poscurs=*on  ;
071300110520           endif  ;
071400110520
071500110524           if  %subst(knsif:7:1)='P' ;
071600110524             %subst(WTAS30:7:4)='GRPS'    ;
071700110524           endif   ;
071800110524
071900080826           open(e)   titas30c           ;
072000080826           if      not %open(titas30c)  ;
072100080826             %subst(WTAS30:7:4)='GRPS'    ;
072200080826             open titas30c                ;
072300080826           endif                        ;
072400080807         endsl   ;
072500080807
072600080807       ENDSR  ;
072700080807       //--------------------------------------------------------------
072800080807       //?controlli   video2
072900080807       //--------------------------------------------------------------
073000080807       BEGSR Contrd02 ;
073100080826
073200080901       *in81=*off  ;
073300080901       *in82=*off  ;
073400080901       *in83=*off  ;
073500080901       *in84=*off  ;
073600080901       *in85=*off  ;
073700080901       *in86=*off  ;
073800080826       clear Wtitasp      ;
073900080826       clear contabolle   ;
074000080826       clear s_tasaas     ;
074100080826       clear s_taslnp     ;
074200080826       clear s_tasnrs     ;
074300080826       clear s_tasnsp     ;
074400080826
074500080826 1     select              ;
074600080826       // Immissione per riferimenti mitt NUMERICO
074700080826 1     when visNumer=*on     ;
074800080826 2      if v1crmn=0    ;
074900080826           errSPE=*on;
075000080826           v1cmsg = Msg(04);
075100080826           errMessage  = *on;
075200080826           errGenerico = *on;
075300080826           leavesr;
075400080826 2     endif               ;
075500080826
075600080826        setll (v1crmn:v1cksc) titas32c  ;
075700080826        reade (v1crmn:v1cksc) titas32c  ;
075800080826 2       dow   not %eof(titas32c)  ;
075900080826
076000080902         // Escludo se bolla figlia ;
076100080902         chain  (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l  ;
076200080902 2a      if not %found(fnlbl01l)    ;
076300080902
076400080826         // la tengo solo se ha particolarità giusta
076500080826         Indx=%lookup(tasgma:SKI7R)  ;
076600080826 3       if   Indx>0  and tasgma<>*blanks  ;
076700080826 4       if contabolle<99 ;
076800080826         contabolle=contabolle+1  ;
076900080826
077000080826         // Mi salvo se la bolla
077100080826 5         if s_tasaas=0    ;
077200080826            s_tasaas=tasaas  ;
077300080826            s_taslnp=taslnp  ;
077400080826            s_tasnrs=tasnrs  ;
077500080826            s_tasnsp=tasnsp  ;
077600080901 6           if *in86=*on ;
077700080826             wtitasp='S'   ;
077800080826 6           endif   ;
077900080826 5         endif   ;
078000080826 4       endif   ;
078100080826 3       endif   ;
078200080902 2a      endif   ;
078300080826
078400080901       *in84 =*off  ;
078500080901       *in85 =*off  ;
078600080901       *in86 =*off  ;
078700080826        reade (v1crmn:v1cksc) titas32c  ;
078800080826 2      enddo    ;
078900080826
079000080826 2      if contabolle<>1   ;
079100080826           errSPE=*on;
079200080826 3         if contabolle=0 ;
079300080826           v1cmsg = Msg(06);
079400080826           else      ;
079500080826           v1cmsg = Msg(05);
079600080826           %subst(v1cmsg:40:2)=%char(contabolle)  ;
079700080826 3         endif    ;
079800080826           errMessage  = *on;
079900080826           errGenerico = *on;
080000080826           leavesr;
080100080826 2      endif              ;
080200080901
080300080901        // Se la bolla è in TITASP non accetto più l'inserimento
080400080901        if wtitasp='S'   ;
080500080901           errSPE=*on;
080600080901           v1cmsg = Msg(15);
080700080901           errMessage  = *on;
080800080901           errGenerico = *on;
080900080901           leavesr;
081000080901        endif   ;
081100080826
081200080826       // Immissione per riferimenti mittente ALFANUMERICO
081300080826 1     when visAlfa =*on     ;
081400080826 2      if v1crma=*blanks ;
081500080826           errSPE=*on;
081600080826           v1cmsg = Msg(07);
081700080826           errMessage  = *on;
081800080826           errGenerico = *on;
081900080826           leavesr;
082000080826 2     endif               ;
082100080826
082200080826        setll (v1crma) tita435c  ;
082300080826        reade (v1crma) tita435c  ;
082400080826 2       dow   not %eof(tita435c)  ;
082500080901         *in81    =*off  ;
082600080901         *in82    =*off  ;
082700080901         *in83    =*off  ;
082800080826         chain  (ta4aas:ta4lnp:ta4nrs:ta4nsp) TITAS30C  ;
082900080826 3       if %found(titas30c) and tasccm=v1cksc and tasgma<>*blanks     ;
083000080902
083100080902         // Escludo se bolla figlia ;
083200080902         chain  (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l  ;
083300080902 3a      if not %found(fnlbl01l)    ;
083400080826
083500080826         // la tengo solo se ha particolarità giusta
083600080826         Indx=%lookup(tasgma:SKI7R)  ;
083700080826 4       if   Indx>0   ;
083800080826 5        if contabolle<99 ;
083900080826          contabolle=contabolle+1  ;
084000080826          // Memorizzo la prima bolla che trovo valida
084100080826 6         if s_tasaas=0    ;
084200080826            s_tasaas=tasaas  ;
084300080826            s_taslnp=taslnp  ;
084400080826            s_tasnrs=tasnrs  ;
084500080826            s_tasnsp=tasnsp  ;
084600080901 7           if *in83    =*on ;
084700080826             wtitasp='S'   ;
084800080826 7           endif   ;
084900080826 6         endif   ;
085000080826 5        endif   ;
085100080826 4       endif   ;
085200080902 3a      endif   ;
085300080902 3       endif   ;
085400080826
085500080826        reade (v1crma) tita435c  ;
085600080826 2      enddo    ;
085700080826
085800080826 2      if contabolle<>1   ;
085900080826           errSPE=*on;
086000080826 3         if contabolle=0 ;
086100080826           v1cmsg = Msg(06);
086200080826           else      ;
086300080826           v1cmsg = Msg(08);
086400080826           %subst(v1cmsg:42:2)=%char(contabolle)  ;
086500080826 3         endif    ;
086600080826           errMessage  = *on;
086700080826           errGenerico = *on;
086800080826           leavesr;
086900080826 2      endif              ;
087000080901
087100080901        // Se la bolla è in TITASP non accetto più l'inserimento
087200080901        if wtitasp='S'      ;
087300080901           errSPE=*on;
087400080901           v1cmsg = Msg(15);
087500080901           errMessage  = *on;
087600080901           errGenerico = *on;
087700080901           leavesr;
087800080901        endif   ;
087900080826
088000080826 x1    other  ;
088100080826
088200080826       // Immissione per numero di spedizione
088300110520 2      if (v1cnsp=0  and v1cpis=0 )  or
088400110520           (v1cnsp>0  and v1cpis<> 0 ) ;
088500080826           errSPE=*on;
088600080826           v1cmsg = Msg(10);
088700080826           errMessage  = *on;
088800080826           errGenerico = *on;
088900080826           leavesr;
089000080826 2     endif               ;
089100080826
089200110520       // Se pieno numero di spedizione --> uso quello altrimenti il barcode
089300110520       clear ds_pis ;
089400110520       if v1cpis<>0     ;
089500110524            ds_pis=%subst(%editc(v1cpis:'X'):4:15)  ;
089600110520       // controllo chkdigit
089700110520
089800110524       // barcode=%editc(v1cpis:'X') ;         //
089900110524         barcode=%subst(%editc(v1cpis:'X'):4:15)  ;
090000110520       clear trul28ds ;
090100110520       i28mod = 'BAR' ;
090200110520       i28cod = barcode  ;
090300110520       callp  TRUL28R1   (trul28ds) ;
090400110520
090500110520       if o28err<>*blanks or o28cod<>ds_pis    ;
090600110524           errBAR7=*on;
090700110520           errGenerico = *on;
090800110520           leavesr;
090900110520 2     endif               ;
091000110520 1     endif               ;
091100110520
091200080901         *in81    =*off  ;
091300080901         *in82    =*off  ;
091400080901         *in83    =*off  ;
091500110520         if v1cnsp>0   ;
091600080827         chain  (v1caas:v1clnp:v1cnrs:v1cnsp) TITAS30C  ;
091700110520         else  ;
091800110520         Kanno=(secolo*100)+ds_pisaas   ;
091900110520         chain  (kanno:ds_pislnp:ds_pisnrs:ds_pisnsp) TITAS30C  ;
092000110520         endif  ;
092100110520
092200080826 2       if    not %found(titas30c)  ;
092300110520         if v1cnsp>0   ;
092400110524           v1cmsg = Msg(11);
092500110524           ErrMessage  = *on;
092600080826           errSPE=*on;
092700110520           else  ;
092800110524           errBAR1=*on;
092900110520           endif  ;
093000110520
093100080826           errGenerico = *on;
093200080826           leavesr;
093300080826 x2      else         ;
093400080826 3       if tasccm<>v1cksc   ;
093500110520         if v1cnsp>0   ;
093600080826           errSPE=*on;
093700110524           v1cmsg = Msg(12);
093800110524           errMessage  = *on;
093900110520           else  ;
094000110524           errBAR2=*on;
094100110520           endif  ;
094200080826           errGenerico = *on;
094300080826           leavesr;
094400080826 3       endif    ;
094500080826
094600080826         // Se la bolla non ha una particolarità che prevede la restituz
094700080826         //  del DDT --> errore
094800080826         Indx=%lookup(tasgma:SKI7R)  ;
094900080826         if   Indx=0  or tasgma=*blanks  ;
095000110520         if v1cnsp>0   ;
095100110520           errSPE=*on;
095200110524           v1cmsg = Msg(14);
095300110524           errMessage  = *on;
095400110520           else  ;
095500110524           errBAR3=*on;
095600110520           endif  ;
095700080826           errGenerico = *on;
095800080826           leavesr;
095900080826       endif       ;
096000080902
096100120322         // Se la bolla è figlia errore
096200080902         chain  (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l  ;
096300080902         if %found(fnlbl01l)    ;
096400110520         if v1cnsp>0   ;
096500110520           errSPE=*on;
096600110524           v1cmsg = Msg(16);
096700110524           errMessage  = *on;
096800110520           else  ;
096900110524           errBAR4=*on;
097000110520           endif  ;
097100080902           errGenerico = *on;
097200080902           leavesr;
097300080902       endif       ;
097400080902
097500080901
097600080901        // Se la bolla è in TITASP non accetto più l'inserimento
097700080901        if *in83    =*on    ;
097800110520         if v1cnsp>0   ;
097900110520           errSPE=*on;
098000110524           v1cmsg = Msg(15);
098100110524           errMessage  = *on;
098200110520           else  ;
098300110524           errBAR5=*on;
098400110520           endif  ;
098500080901           errGenerico = *on;
098600080901           leavesr;
098700080901        endif   ;
098800080826
098900080826 2      endif              ;
099000080826
099100080826        // salvo il numero di spedizione
099200080826            s_tasaas=tasaas  ;
099300080826            s_taslnp=taslnp  ;
099400080826            s_tasnrs=tasnrs  ;
099500080826            s_tasnsp=tasnsp  ;
099600080901 2           if *in83    =*on ;
099700080826             wtitasp='S'   ;
099800080826 2           endif   ;
099900080826 1     ENDSL  ;
100000080826
100100120330       // Trovata la spedizione, avviso se già aggiornata in altra data
100200080826 1     if s_tasaas<>w5aas or s_taslnp<>w5lnp or s_tasnrs<>w5nrs or
100300080826        s_tasnsp<>w5nsp  ;
100400080826           w5aas=s_tasaas  ;
100500080826           w5lnp=s_taslnp  ;
100600080826           w5nrs=s_tasnrs  ;
100700080826           w5nsp=S_tasnsp  ;
100800080826
100900120329 2     if indManut=*off  ;
101000080826       chain  (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
101100120328       else   ;
101200120328       chain(n) (s_tasaas:s_taslnp:s_tasnrs:s_tasnsp:'GEN') fiar531c  ;
101300120329 2     endif  ;
101400120328
101500080826 2     if %found(fiar531c)   ;
101600080826       dar5gen=ar5uni        ;
101700080826       dataiso=%date             ;
101800120322
101900120322       //  Avviso se già aggiornata in altra data --> modo Immissione
102000120322 2a    if indManut=*off  ;
102100120322
102200080902 3      if §ar5drdt<>*blanks and §ar5drdt<>'99999999'   ;
102300120330 4       if visnumer=*off and visalfa=*off and v1cnsp=0 ;
102400120330           errBAR6=*on;
102500120330 x4      else  ;
102600120330           v1cmsg = Msg(13);
102700120330           DATAEUR=%DATE(%int(§ar5drdt):*iso) ;
102800120330           %subst(v1cmsg:36:10)=%editw(%dec(dataeur):'  /  /    ') ;
102900120330           errMessage  = *on;
103000120330           errSPE=*on;
103100120330 4        endif  ;
103200120330
103300080826           errGenerico = *on;
103400080826           leavesr;
103500080826 3      endif      ;
103600120330
103700120322
103800120322 x2a    else    ;
103900120328
104000120322        // In manutenzione imposto la data
104100120322 3      if §ar5drdt<>*blanks and §ar5drdt<>'99999999'   ;
104200120322
104300120322  4     monitor  ;
104400120322        dataiso=%date(%int(§ar5drdt))  ;
104500120322        on-error   ;
104600120326        clear dataiso  ;
104700120322  4     endmon  ;
104800120329
104900120329  x3    else   ;
105000120322        clear dataiso  ;
105100120329  3     endif  ;
105200120322
105300120329  3     if dataiso>*loval  ;
105400120326          dataeur=dataiso   ;
105500120328          V1CDrdt =%dec(dataeur)  ;
105600120329  3     endif  ;
105700120322
105800120328
105900120328        // Data inserimento motivo
106000120329 3      if §ar5dimtv<>*blanks   ;
106100120328
106200120328  4     monitor  ;
106300120328        dataiso=%date(%int(§ar5dimtv))  ;
106400120328        on-error   ;
106500120328        clear dataiso  ;
106600120328  4     endmon  ;
106700120329  x3    else   ;
106800120328        clear dataiso  ;
106900120329  3     endif  ;
107000120328
107100120329  3     if dataiso>*loval  ;
107200120328          dataeur=dataiso   ;
107300120328          V1CDimtv =%dec(dataeur)  ;
107400120329  3     endif  ;
107500120328
107600120328
107700120328        // Codice motivo
107800120402 3      if §ar5mtvdt<>*blanks   ;
107900120402
108000120402        v1cmtvdt=%subst(§ar5mtvdt:4:5)  ;
108100120402        v1cfil  =%int(%subst(§ar5mtvdt:1:3))  ;
108200120330        clear tbeke1 ;
108300120330        clear tbeke2 ;
108400120402        tbeke1=%editc(v1cfil:'X')  ;
108500120330        tbeke2=v1cmtvdt  ;
108600120330        chain ('MDE':tbeke1:tbeke2) tntbe01l  ;
108700120330         if %found(tntbe01l)  ;
108800120330          v1dmtvdt=tbeuni  ;
108900120330         endif  ;
1090001204023        endif  ;
109100120330
109200120329 2a     endif  ;
109300120322
109400080826 2      endif      ;
109500080826 1     endif                 ;
109600080826
109700120320       // Nessun errore --> Se manutenzione emetto 3° videata
109800120328 0     if IndManut=*on    ;
109900120320            $Video= 'D3' ;
110000120328 x0    else  ;
110100120330       // Trovata la spedizione, avviso se presente motivo che verrà tolto
110200120330 1     if §ar5mtvdt<>*blanks  and
110300120330         (s_tasaas<>b_w5aas or s_taslnp<>b_w5lnp or s_tasnrs<>b_w5nrs or
110400120330        s_tasnsp<>b_w5nsp)  ;
110500120330           b_w5aas=s_tasaas  ;
110600120330           b_w5lnp=s_taslnp  ;
110700120330           b_w5nrs=s_tasnrs  ;
110800120330           b_w5nsp=S_tasnsp  ;
110900120330 2       if visnumer=*off and visalfa=*off and v1cnsp=0 ;
111000120330           errBAR8=*on;
111100120330 x2      else  ;
111200120330           v1cmsg = Msg(19);
111300120521           %subst(v1cmsg:35:8)=§ar5mtvdt  ;
111400120330           errMessage  = *on;
111500120330           errSPE=*on;
111600120330 2        endif  ;
111700120330
111800120330           errGenerico = *on;
111900120330           leavesr;
112000120330 1      endif  ;
112100120320
112200080826       // Se nessun errore aggiorno con ENTER  fiar5 e contatore
112300080826 1     if not %found(fiar531c) or §ar5drdt<>%char(%dec(dataiso))   ;
112400080826
112500080826 2     if not %found(fiar531c)  ;
112600120328       exsr ImpostaAR5          ;
112700080826 x2    else                     ;
112800080826       dar5gen=ar5uni           ;
112900080826 2     endif                    ;
113000080826
113100080826       dataiso=%date             ;
113200080826       §ar5drdt=%char(%dec(dataiso))   ;
113300080826       clear §ar5fstd           ;
113400120330       clear §ar5mtvdt          ;
113500120330       clear §ar5dimtv          ;
113600080826       ar5uni=dar5gen           ;
113700080826
113800120329       exsr UpdateAR5  ;
113900080826
114000080826       // totale bolle aggiornate
114100080826       v1ctot=v1ctot+1    ;
114200080827       vistotbo=*on       ;
114300120625
114400120625       // Stampo LDV
114500120625       if v1cldv='SI'   ;
114600120625        clear tnsb25ds   ;
114700120625        d25ges='T'  ;
114800120625        d25aas=ar5aas  ;
114900120625        d25lnp=ar5lnp  ;
115000120625        d25nrs=ar5nrs  ;
115100120625        d25nsp=ar5nsp  ;
115200120625        callp tnsb25r (tnsb25ds) ;
115300120625       endif   ;
115400120625
115500080827       else   ;
115600080827
115700080827       unlock fiar531c   ;
115800080826 1     endif    ;
115900080826
116000120329       exsr InzParz02  ;
116100120328  0      endif  ;
116200080826
116300080826
116400080807       ENDSR  ;
116500120320       //--------------------------------------------------------------
116600120320       //?controlli formato 03
116700120320       //--------------------------------------------------------------
116800120320       BEGSR Contrd03  ;
116900120330
117000120329         clear v1dmtvdt  ;
117100120330
117200120329       // Verifico '?' sulla causale
117300120329       Indx=%scan ('?':v1cmtvdt)  ;
1174001203301      if indx>0   ;
117500120329         clear v1cmtvdt  ;
117600120329         clear tntb92ds  ;
117700120329         b92fun ='1'  ;
117800120402         b92ke1=%editc(v1cfil:'X')  ;
117900120329         kpjbu=tntb92ds  ;
118000120329         callp tntb92r (kpjba)  ;
118100120329
118200120329       tntb92ds=kpjbu  ;
1183001203302      if b92ke2<>*blanks  ;
118400120329         v1cmtvdt=b92ke2  ;
118500120329         v1dmtvdt=b92des  ;
1186001203302      endif  ;
118700120329
1188001203301      endif   ;
118900120329
119000120330       // controllo causale se immessa
1191001204021      if v1cmtvdt<>*blanks ;
119200120330        clear tbeke1 ;
119300120330        clear tbeke2 ;
119400120402        tbeke1=%editc(v1cfil:'X')  ;
119500120330        tbeke2=v1cmtvdt  ;
119600120330
119700120330        chain ('MDE':tbeke1:tbeke2) tntbe01l  ;
1198001204022       if not %found(tntbe01l) or tbeatb<>*blanks   ;
119900120330           errMTV=*on;
120000120330           v1cmsg = Msg(18);
120100120330           errMessage  = *on;
120200120330           errGenerico = *on;
120300120330           leavesr    ;
1204001204022       endif  ;
120500120330
120600120330        v1dmtvdt=tbeuni  ;
120700120330
1208001204021      endif  ;
120900120330
121000120402       // Se tolto MOTIVO AZZERO LA DATA
1211001204021      if v1cmtvdt=*blANKS  ;
121200120402       clear v1cdimtv  ;
121300120402       v1cfil=dutpou  ;
121400120402x1     else   ;
121500120402
121600120402       // se motivo = a quello del file stessa data
1217001204022      if v1cmtvdt=%subst(§ar5mtvdt:4:5) and §ar5dimtv>*zeros  ;
121800120402        dataiso=%date(%int(§ar5dimtv))  ;
121900120402       else   ;
122000120402         dataiso=%date             ;
1221001204022      endif  ;
122200120402
122300120402          dataeur=dataiso   ;
122400120402          V1CDimtv =%dec(dataeur)  ;
1225001204021      endif  ;
122600120402
122700120320       ENDSR  ;
122800080807
122900080807       //--------------------------------------------------------------
123000080807       //?Operazioni finali.
123100080807       //--------------------------------------------------------------
123200080807       BEGSR RoutEnd;
123300080807
123400080807         *inLR = *on;
123500080826
123600080807         return;
123700080807
123800080807       ENDSR;
123900120328       //--------------------------------------------------------------
124000120328       //?Imposta campi FIar5 per la write
124100120328       //--------------------------------------------------------------
124200120328       BEGSR ImpostaAR5         ;
124300120328
124400120328       clear fiar5000           ;
124500120328       clear fiar5p00           ;
124600120328       ar5aas=s_tasaas          ;
124700120328       ar5lnp=s_taslnp          ;
124800120328       ar5nrs=s_tasnrs          ;
124900120328       ar5nsp=s_tasnsp          ;
125000120328       ar5trd='GEN'             ;
125100120328       dataiso=%date             ;
125200120328       ar5dac=%dec(dataiso)     ;
125300120328       timeiso=%time             ;
125400120328       ar5orc=%dec(timeiso)     ;
125500120328       ar5ft1='R'               ;
125600120328       ar5dt1=%dec(dataiso)     ;
125700120328       ar5ft2='R'               ;
125800120328       ar5dt2=%dec(dataiso)     ;
125900120328       ar5ft3='R'               ;
126000120328       ar5dt3=%dec(dataiso)     ;
126100120328
126200120328       clear dar5gen            ;
126300120328       ENDSR                   ;
126400120329       //--------------------------------------------------------------
126500120329       //?Aggiorno record FIAR5
126600120329       //--------------------------------------------------------------
126700120329       BEGSR  UpdateAR5  ;
126800120329
126900120329 2     if not %found(fiar531c)  ;
127000120329 3       if wtitasp='S'  ;
127100120329         write  fiar5p00           ;
127200120329          else    ;
127300120329         write  fiar5000           ;
127400120329 3       endif  ;
127500120329
127600120329 x2    else   ;
127700120329 3       if wtitasp='S'  ;
127800120329          update fiar5p00           ;
127900120329           else    ;
128000120329          update fiar5000           ;
128100120329 3     endif    ;
128200120329 2     endif    ;
128300120329
128400120329       ENDSR  ;
128500120329       //--------------------------------------------------------------
128600120329       //?Pulizia parziale campoi video 2
128700120329       //--------------------------------------------------------------
128800120329       BEGSR InzParz02 ;
128900120329
129000120329         clear v1crmn;
129100120329         clear v1crma;
129200120329         clear v1cnsp;
129300120329         clear v1cpis;
129400120329         clear w5aas ;
129500120329         clear w5lnp ;
129600120329         clear w5nrs ;
129700120329         clear w5nsp ;
129800120330         clear b_w5aas ;
129900120330         clear b_w5lnp ;
130000120330         clear b_w5nrs ;
130100120330         clear b_w5nsp ;
130200120402         clear v1cdrdt  ;
130300120402         clear v1cmtvdt ;
130400120402         clear v1dmtvdt ;
130500120402         clear v1cdimtv ;
130600120402         v1cfil=dutpou  ;
130700120329       ENDSR  ;
130800080807
130900080807      /end-free
131000080807
131100080807
131200000606**  MSG
131300080826Immettere il codice Cliente Mittente                                           1
131400080826Codice Cliente Mittente inesistente                                            2
131500080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
131600080826Immettere il riferimento mittente NUMERICO                                     4
131700080826Per il rif. Mittente NUMERICO esistono xx bolle: inserire col numero di Spediz 5
131800080909Nessuna bolla trovata per il cliente con particolarità restituzione documenti  6
131900080826Immettere il riferimento mittente ALFANUMERICO                                 7
132000080826Per il RifMittente ALFANUMERICO esistono xx bolle:inserire col numero di Sped  8
132100120328Impossibile annullare : data non presente!                                     9
132200110520Immettere il numero di SPEDIZIONE  o il Barcode, in alternativa               10
132300080826Numero di SPEDIZIONE inesistente                                               1
132400080826La SPEDIZIONE non appartiene al cliente mittente indicato                      2
132500120521Già presente data ricezione doc.al xx/xx/xxxx:ENTER per forzare e riaggiornare 3
132600080909La SPEDIZIONE non prevede particolarità consegna con restituzione documenti    4
132700080901La spedizione è ormai troppo vecchia: impossibile eseguire l'inventario        5
132800120521Spedizione con legame bolla:inserire ricezione documenti sulla bolla originale 6
132900110520Errore nel Barcode                                                             7
133000120330Causale motivo inesistente                                                     8
133100120521Presente motivo documenti errati "xxxxxxxx": verrà eliminato                   9
