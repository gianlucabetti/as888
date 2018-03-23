000100080807     H DECEDIT('0,') DATEDIT(*ymd/)
000200120614     H* FNLSV5R *-----------------------------------------------------*
000300120614     H*   controllo documenti da restituire al mittente
000400930126     H*---------------------------------------------------------------*
000500120614     FFNLsv5d   CF   E             WORKSTN indds(IndDspF)
000600080807     f                                     infds(InfDspF)
000700080319     Ftabel00f  IF   E           K DISK
000800120920     fTNTBE01L  if   e           k disk
000900080807     Fcnaco00f  IF   E           K DISK
001000080902     Fazorg01l  IF   E           K DISK
001100080319     d*-----------------
001200080807      // - Tasti funzionali a video
001300080807     d c_F01           c                   const(x'31')
001400080807     d c_F02           c                   const(x'32')
001500080807     d c_F03           c                   const(x'33')
001600080807     d c_F05           c                   const(x'35')
001700080807     d c_F06           c                   const(x'36')
001800080807     d c_F07           c                   const(x'37')
001900080807     d c_F08           c                   const(x'38')
002000080807     d c_F09           c                   const(x'39')
002100080807     d c_F10           c                   const(x'3A')
002200080807     d c_F12           c                   const(x'3C')
002300080807     d c_F13           c                   const(x'B1')
002400080807     d c_F14           c                   const(x'B2')
002500080807     d c_F15           c                   const(x'B3')
002600080807     d c_F16           c                   const(x'B4')
002700080807     d c_F17           c                   const(x'B5')
002800080807     d c_F18           c                   const(x'B6')
002900080807     d c_F19           c                   const(x'B7')
003000080807     d c_F20           c                   const(x'B8')
003100080807     d c_F21           c                   const(x'B9')
003200080807     d c_F22           c                   const(x'BA')
003300080807     d c_F23           c                   const(x'BB')
003400080807     d c_F24           c                   const(x'BC')
003500080807     d InfDspF         ds
003600080807     d  dsp_aid              369    369a
003700080807     d  sfl_rrn              376    377i 0
003800080807     d  min_nrr              378    379i 0
003900080807     d  num_rcds             380    381i 0
004000080807
004100080807      // - Indicatori su DspF
004200080807     d IndDspF         ds
004300080807        // - Indicatori di errore
004400080807     d  errMessage                    1n   overlay(IndDspF : 28)
004500080807     d  errGenerico                   1n   overlay(IndDspF : 99)
004600080807        // - Indicatori vari
004700080807     d  ErrKSC                        1n   overlay(IndDspF : 50)
004800120402     d  Errsped                       1n   overlay(IndDspF : 52)
004900120402     d  Errspea                       1n   overlay(IndDspF : 53)
005000080827     d  ErrDSD                        1n   overlay(IndDspF : 54)
005100080827     d  ErrDSA                        1n   overlay(IndDspF : 55)
005200080902     d  ErrLNP                        1n   overlay(IndDspF : 56)
005300110617     d  Erremail                      1n   overlay(IndDspF : 57)
005400120411     d  Errinvrd                      1n   overlay(IndDspF : 58)
005500120411     d  Errinvra                      1n   overlay(IndDspF : 59)
005600120614     d  Errscand                      1n   overlay(IndDspF : 60)
005700120614     d  Errscana                      1n   overlay(IndDspF : 61)
005800120614     d  Errinvimd                     1n   overlay(IndDspF : 62)
005900120614     d  Errinvima                     1n   overlay(IndDspF : 63)
006000120824     d  Errscan                       1n   overlay(IndDspF : 64)
006100120828     d  Errsta                        1n   overlay(IndDspF : 65)
006200080807
006300080807     d*-----------------
006400120920     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
006500080827     D SKI7R           S              2    DIM(50)
006600080807     d*-----------------
006700911010     D WLBDAT          DS
006800941221     D  G02DAT                 1      8  0
006900941221     D  G02INV                 9     16  0
007000941221     D  G02ERR                17     17
007100941221     D  G02TGI                18     22  0
007200080828
007300120614     D fnlsv5ds        DS
007400120822     d* data inventario DAL/AL
007500120411     D  v1cinvrd              17     24  0
007600120411     D  cominvrd              25     32  0
007700120411     D  v1cinvra              33     40  0
007800120411     D  cominvra              41     48  0
007900120405
008000120713     d* Data CONSEGNA
008100080828     D  v1cdsd                49     56  0
008200080828     D  comdsd                57     64  0
008300080828     D  v1cdsa                65     72  0
008400080828     D  comdsa                73     80  0
008500120405     D
008600080829     D  v1cksc                81     87  0
008700080829     D  v1dksc                88    112
008800080902     D  v1clnp               113    115  0
008900080902     D  v1dlnp               116    140
009000110617     D  v1cemail             142    171
009100120713     d*
009200120713     d* data spedizione
009300120405     D  comsped              173    180  0
009400120405     D  comspea              181    188  0
009500120405     D  v1cinve              189    189
009600120405     D  v1cerra              190    190
009700120713     d* Data scannerizzazione
009800120614     D  comscand             191    198  0
009900120614     D  comscana             199    206  0
010000120713     d*   Per il momento i prossimi 3 campi
010100120713     d*                  le date restituzione al mittente sono oscurate e non
010200120704     d*    usate
010300120614     D  cominvimD            207    214  0
010400120614     D  cominvimA            215    222  0
010500120713     D  v1cinvim             223    223
010600120704     d
010700120704     D  v1cscan              224    224
010800120614     D  v1csta               225    225
010900080319     d
011000040604     D KPJBA         E DS
011100080807     D tibs34ds      E DS
011200080319     D ds7N          E DS
011300111228     D dsemail       E DS
011400080807     D ds7PQRS       E DS
011500080807     d kscabi                         2    DIM(30)  overlay(ds7cdp)
011600080319     d
011700080807     d DS7R          e ds
011800080826      // - Ricerca/Controllo tabelle
011900080826     d TIBS02ds      e ds                  inz
012000120920
012100120920       // - Tabella "CLI" = Abilitazione clienti
012200120920     d dCLI          e ds                  inz
012300040614     D*
012400080319     d Dataiso         s               d   datfmt(*iso)
012500080826     d timeiso         s               t   timfmt(*iso)
012600080319     d Dataeur         s               d   datfmt(*eur)
012700080807     D W007A           S              7
012800080807     D $video          S              2    inz('D1')
012900080807     D $inzd01         S              1
013000120614     D $inzd02         S              1
013100080807     d $Fine           s               n   inz(*off)
013200080807     d Primavolta      s              1    inz
013300080807     D XX              S              3  0 inz
013400080807     D Indx            S              3  0 inz
013500120704     d Diff            s             13  0
013600120424     d gg_speD         s                   like(g02tgi)
013700120424     d gg_speA         s                   like(g02tgi)
013800120614     d gg_scanD        s                   like(g02tgi)
013900120614     d gg_scanA        s                   like(g02tgi)
014000080807     d
014100080807     d  xpardut        s             30
014200080807     d  xparkut        s              1  0
014300080807     d  xparrag        s             48
014400080807     d  xparkcc        s              4  0
014500080807     d  xparsta        s              1  0
014600080807     d  xparflr        s             90
014700080807     d  xpardit        s              3
014800080807     d  xparnum        s              2  0
014900080807     d  xparkcm        s             80
015000080807     d  xparksm        s            140
015100080807     d  xparkdm        s             60
015200080807     d  xparesci       s              1
015300080807     d  xparerr        s              2
015400080807     d  xpariva        s             16
015500080807     d  xparcdf        s             16
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
017100080826      /copy gaitrasrc/srcprotopr,tibs02r
017200080827      /copy gaitrasrc/srcprotopr,xsrda8
017300111228      /copy gaitrasrc/srcprotopr,xemail
017400000000     C*---------------------------------------------------------------*
017500911010     C* INDICATORI
017600911010     C*---------------------------------------------------------------*
017700951115     C*  28    - EMETTE CAMPO MESSAGGI
017800040607     C*  40    - ERRORE
017900911010     C*  90    - ERRORE GENERICO
018000911010     C*---------------------------------------------------------------*
018100000000     C     *ENTRY        PLIST
018200000000     C                   PARM                    KPJBA
018300080807
018400080807      /free
018500080807
018600080807       // Operazioni iniziali
018700080807       exsr RoutInz;
018800080807
018900080807       // Gestione video
019000080807       DOW $Fine = *off;
019100080807         select;
019200080807
019300120614         // Videata di selezioni  1
019400080807           when $Video = 'D1';
019500080807             exsr GesD01;
019600080807
019700120614         // Videata di selezioni  2
019800120614           when $Video = 'D2';
019900120614             exsr GesD02;
020000120614
020100080807           other   ;
020200080807             $Fine = *on;
020300080807         endsl;
020400080807       ENDDO;
020500080807
020600080807       // Operazioni finali
020700080807       exsr RoutEnd;
020800080807
020900080807       //--------------------------------------------------------------
021000080807       //?Operazioni iniziali.
021100080807       //--------------------------------------------------------------
021200080807       BEGSR RoutInz;
021300080807       $inzD01=*on;
021400080807
021500080807       // Solo la prima volta
021600080807 1     if primavolta=' '   ;
021700080807
021800080807         // Reperimento dati job
021900080807         exsr DatiJob;
022000080807
022100080807         // Carico tutte le particolarità consegne che prevedono
022200080807         //  la restituzione della BAM
022300080807
022400080807         clear XX   ;
022500080807         setll (1:'7R')  tabel00f    ;
022600080807         reade (1:'7R')  tabel00f    ;
022700080807
022800080807         dow not %eof(tabel00f)  ;
022900080807
023000080807         if tblflg=' '   ;
023100080807           ds7r=tbluni   ;
023200080807           if §7r1bf='B' ;
023300080807            xx=xx+1   ;
023400080807            SKI7r(xx)=tblkey   ;
023500080807           endif     ;
023600080807         endif           ;
023700080807
023800080807         reade (1:'7R')  tabel00f    ;
023900080807         enddo     ;
024000080807 1       endif    ;
024100080807
024200080807         ENDSR;
024300080807
024400080807       //--------------------------------------------------------------
024500080807       //?Reperimento Dati del job (Utente/Operativi).
024600080807       //--------------------------------------------------------------
024700080807       BEGSR DatiJob;
024800080807
024900080807         in(E) §AzUte;
025000080807         if NOT %error;
025100080807           in(E) §DatiUte;
025200080807         endif;
025300080807         if %error or RSut = *blanks;
025400080807           clear TIBS34ds;
025500080807           tibs34r(tibs34ds);
025600080807           in §AzUte;
025700080807           in §DatiUte;
025800080807         endif;
025900080807
026000080807       ENDSR;
026100080807       //--------------------------------------------------------------
026200080807       //?Gestione video 1
026300080807       //--------------------------------------------------------------
026400080807       BEGSR GesD01;
026500080807
026600080807         // Inizializzazione videata
026700080807         if  $Inzd01 = *on;
026800080807            exsr InzD01;
026900080807            $Inzd01  = *off;
027000080807         endif;
027100080807
027200080807         // Emissione videata
027300120614         exfmt  lsv5d01;
027400080807
027500080807         reset errMessage;
027600080807         reset errGenerico;
027700080807         clear V1cmsg;
027800080807         errksc=*off     ;
027900080902         errlnp=*off     ;
028000110617         erremail=*off   ;
028100120828         errsta  =*off   ;
028200080807
028300080807 1       SELECT;
028400080807
028500080807         // - F3=Fine
028600080807 1         WHEN  dsp_aid = c_F03;
028700080807            $Fine = *on;
028800080807
028900080807 x1      // Invio
029000080807           OTHER;
029100080807             exsr ContrD01 ;
029200120614
029300080807 2           if  errGenerico = *on;
029400080807               leavesr;
029500080807 2           endif;
029600120614
029700120614            $Video='D2'  ;
029800120614            $inzD02=*on;
029900080807
030000080807 1       ENDSL;
030100080807
030200080807       ENDSR;
030300080807       //--------------------------------------------------------------
030400080807       //?controlli video1
030500080807       //--------------------------------------------------------------
030600080807       BEGSR ContrD01 ;
030700080807       select    ;
030800080807       when v1cksc=0 and v1dksc=*blanks               ;
030900080807           errksc=*on;
031000080807           v1cmsg = Msg(01);
031100080807           errMessage  = *on;
031200080807           errGenerico = *on;
031300080807           leavesr;
031400080807
031500080807       when v1cksc=0 and v1dksc<>*blanks   ;
031600080807       xparsta=9   ;
031700080807       xpardut=rsut   ;
031800080807       clear xparrag   ;
031900080807       xparrag  =v1dksc  ;
032000080807       xparkcc=dutkci    ;
032100080807       clear xparflr  ;
032200080807       clear xpardit  ;
032300080807       xparnum=1     ;
032400080807       clear xparkcm ;
032500080807       clear xparksm ;
032600080807       clear xparkdm ;
032700080807       clear xparesci;
032800080807       clear xparerr ;
032900080807       clear xpariva ;
033000080807       clear xparcdf ;
033100080807       xalfa3br (xpardut:xparkut:xparrag:xparkcc:xparsta:xparflr:xpardit
033200080807                 :xparnum:xparkcm:xparksm:xparkdm:xparesci:xparerr:xpariva
033300080807                 :xparcdf)             ;
033400080807       if xparsta<>-1  ;
033500080807       w007a=XPARKSM  ;
033600080807       V1CKSC=%int(W007A)  ;
033700080807       v1dksc=xparrag   ;
033800080807       else   ;
033900080807       clear v1dksc    ;
034000080807       endif    ;
034100080807       ErrGenerico=*on   ;
034200080807       leavesr   ;
034300080807
034400080807       other     ;
034500080807       chain   (1:dutkci:v1cksc)   cnaco000   ;
034600080807       if   not %found(cnaco00f) or acoflg<>' '    ;
034700080807           errksc=*on;
034800080807           v1cmsg = Msg(02);
034900080807           errMessage  = *on;
035000080807           errGenerico = *on;
035100080807           clear v1dksc    ;
035200080807           leavesr;
035300080807       endif   ;
035400080807
035500080807       v1dksc=acorag   ;
035600080807       ENDSL   ;
035700080807
035800080807       // Verifico se il cliente prevede la particolarità giusta
035900080807       clear ds7pqrs   ;
036000080807       ds7ksc=v1cksc   ;
036100080826       ds7ges='O'      ;
036200080807       ds7ric='S'      ;
036300080807       kpjbu=ds7pqrs   ;
036400080807       TRTB70R (KPJBA)   ;
036500080807       ds7pqrs=kpjbu     ;
036600080807
036700080807       xx=1   ;
036800080807       clear Indx    ;
036900080807       dow kscabi(xx)<>*blanks  and Indx=0;
037000080807       Indx=%lookup(kscabi(xx):SKI7R)  ;
037100080807       xx=xx+1  ;
037200080807       enddo    ;
037300080807
037400080807       if Indx=0   ;
037500080807           errksc=*on;
037600080807           v1cmsg = Msg(03);
037700080807           errMessage  = *on;
037800080807           errGenerico = *on;
037900080807           leavesr;
038000080807       endif       ;
038100120920
038200120920       // - controllo se filiale gestibile
038300120920       clear  TBEke1;
038400120920       clear  tbeke2;
038500120920       TBEke1 = %editc( V1CKsc : 'X' );
038600120920       chain  ('CLI' : TBEke1 : TBEke2)  TNTBE000;
038700120920       // do errore se il cliente non è sulla tab.CLI oppure c'è ma annullato
038800120920       if  not %found(TNTBE01L)
038900120920        or %found(TNTBE01L) and TBEATB <> *blank;
039000120920         errKSC = *on;
039100120920         V1Cmsg = Msg(20);
039200120920         errMessage  = *on;
039300120920         errGenerico = *on;
039400120920         leavesr;
039500120920       else;
039600120920         dCLI = TBEuni;
039700120920         // do errore il PO utente non è tra le filiali abilitate alla
039800120920         // retituzione doc. al mittente
039900120920         if  %editc( DUTpou : 'X' ) <> §CLIFi1 and
040000120920             %editc( DUTpou : 'X' ) <> §CLIFi2 and
040100120920             %editc( DUTpou : 'X' ) <> §CLIFi3;
040200120920           errKSC = *on;
040300120920           V1Cmsg = Msg(20);
040400120920           errMessage  = *on;
040500120920           errGenerico = *on;
040600120920           leavesr;
040700120920         endif;
040800120920       endif;
040900080827
041000080902       // controllo la linea di partenza immessa
041100080902       clear v1dlnp    ;
041200080902       if v1clnp>0        ;
041300080902       chain  v1clnp   azorg01l   ;
041400080903        if not %found(azorg01l) or orgfva<>' ' or (orgfag<>'A' and
041500080903                                                   orgfag<>'F') ;
041600080902           errlnp=*on;
041700080902           v1cmsg = Msg(10);
041800080902           errMessage  = *on;
041900080902           errGenerico = *on;
042000080902           leavesr;
042100080902        endif   ;
042200080903        v1dlnp=orgdes    ;
042300080902       endif   ;
042400110617
042500120614       // Richidere almeno una elaborazione
042600120827       if (v1csta  =' ' ) ;
042700120828           errsta  =*on;
042800110617           v1cmsg = Msg(12);
042900110617           errMessage  = *on;
043000110617           errGenerico = *on;
043100110617           leavesr;
043200110617       endif   ;
043300120827       if v1cemail<>*blanks  and v1csta<>'E'   and v1csta<>'X'  ;
043400120828           errsta  =*on;
043500120827           v1cmsg = Msg(17);
043600120827           errMessage  = *on;
043700120827           errGenerico = *on;
043800120827           leavesr;
043900120827       endif   ;
044000120828       if v1cemail= *blanks  and (v1csta='E'  or v1csta='X' ) ;
044100120828           erremail=*on;
044200120828           v1cmsg = Msg(18);
044300120828           errMessage  = *on;
044400120828           errGenerico = *on;
044500120828           leavesr;
044600120828       endif   ;
044700110613
044800111228       // controllo formale indirizzo email
044900111228       if v1cemail<>*blanks   ;
045000111228       clear dsemail  ;
045100120302       §EMLINDI=%trim(v1cemail)+'@brt.it'   ;
045200111228       callp XEMAIL   (dsemail)  ;
045300111228         if §EMLERRO<>*blanks and §EMLERRO<> '0';
045400111228             erremail=*on;
045500111228             v1cmsg = §emlmsgo   ;
045600111228             errMessage  = *on;
045700111228             errGenerico = *on;
045800111228             leavesr;
045900111228         endif   ;
046000111228       endif   ;
046100111228
046200080807       ENDSR   ;
046300080807       //--------------------------------------------------------------
046400080807       //?Inizializzazione video1
046500080807       //--------------------------------------------------------------
046600080807       BEGSR InzD01;
046700080807         clear v1cksc;
046800080807         clear v1dksc;
046900080903         clear v1clnp  ;
047000080903         clear v1dlnp  ;
047100110617         clear v1cemail;
047200120614         clear v1csta  ;
047300080807       ENDSR  ;
047400120614       //--------------------------------------------------------------
047500120614       //?Gestione video 2
047600120614       //--------------------------------------------------------------
047700120614       BEGSR GesD02;
047800120614
047900120614         // Inizializzazione videata
048000120614         if  $Inzd02 = *on;
048100120614            exsr InzD02;
048200120614            $Inzd02  = *off;
048300120614         endif;
048400120614
048500120614         // Emissione videata
048600120614         exfmt  lsv5d02;
048700120614
048800120614         reset errMessage;
048900120614         reset errGenerico;
049000120614         clear V1cmsg;
049100120614         errsped =*off     ;
049200120614         errspea =*off     ;
049300120614         errdsd=*off     ;
049400120614         errdsa=*off     ;
049500120614         errinvrd=*off     ;
049600120614         errinvra=*off     ;
049700120614         errscand=*off     ;
049800120614         errscana=*off     ;
049900120614         errinvimd=*off     ;
050000120614         errinvima=*off     ;
050100120824         errscan  =*off     ;
050200120614
050300120614 1       SELECT;
050400120614
050500120614         // - F3=Fine
050600120614 1         WHEN  dsp_aid = c_F03;
050700120614            $Fine = *on;
050800120614 1         WHEN  dsp_aid = c_F12;
050900120614            $video='D1' ;
051000120614
051100120614 x1      // Invio
051200120614           OTHER;
051300120614             exsr ContrD02 ;
051400120614 2           if  errGenerico = *on;
051500120614               leavesr;
051600120614 2           endif;
051700120614
051800120614           // F6- Conferma
051900120614 1         if  dsp_aid = c_F06;
052000120614            exsr Conferma      ;
052100120614            $Inzd01 = *on;
052200120614            $Video='D1'  ;
052300120614           endif              ;
052400120614
052500120614 1       ENDSL;
052600120614
052700120614       ENDSR;
052800120614       //--------------------------------------------------------------
052900120614       //?Inizializzazione video2
053000120614       //--------------------------------------------------------------
053100120614       BEGSR InzD02;
053200120614         clear v1cspeD ;
053300120614         clear v1cspeA ;
053400120614         clear v1cdsd  ;
053500120614         clear v1cdsa  ;
053600120614         clear v1cinvrd  ;
053700120614         clear v1cinvra  ;
053800120614         clear v1cscand  ;
053900120614         clear v1cscana  ;
054000120614         clear v1cinvimd ;
054100120614         clear v1cinvima ;
054200120614         v1cerra =' '  ;
054300120614         v1cinve =' '  ;
054400120614         v1cscan =' '  ;
054500120614         v1cinvim =' '  ;
054600120614       ENDSR  ;
054700120614       //--------------------------------------------------------------
054800120614       //?controlli video2
054900120614       //--------------------------------------------------------------
055000120614       BEGSR ContrD02 ;
055100120614
055200120614       // Immettere almeno una data
055300120614
055400120614       if v1csped=0 and v1cspea =0  and v1cdsd=0 and
055500120614          v1cdsa=0  and v1cinvrd=0 and v1cinvra=0  and
055600120614          v1cinvimD=0  and v1cinvimA=0 and v1cscanD=0  and v1cscanA=0 ;
055700120614           errspeD= *on;
055800120614           v1cmsg = Msg(04);
055900120614           errMessage  = *on;
056000120614           errGenerico = *on;
056100120614           leavesr;
056200120614       endif          ;
056300120614
056400120614       // Data SPEDIZIONE DAL
056500120614       clear comsped    ;
056600120704       clear gg_speD     ;
056700120704       clear gg_spea     ;
056800120614       if v1cspeD >0      ;
056900120614         g02dat=v1cspeD   ;
057000120614         EXSR ContrDATA ;
057100120614
057200120614         if errMessage=*on ;
057300120614           errspeD =*on;
057400120614           leavesr;
057500120614          else               ;
057600120614          v1cspeD =g02dat      ;
057700120614          comspeD =g02inv      ;
057800120614          gg_speD =g02tgi      ;
057900120614          endif            ;
058000120614       endif            ;
058100120614
058200120614       // Data spedizione  AL
058300120614       clear comspeA    ;
0584001206141      if v1cspeA >0      ;
058500120614         g02dat=v1cspeA  ;
058600120614         EXSR ContrDATA ;
058700120614
0588001206142        if errMessage=*on ;
058900120614           errspeA= *on;
059000120614           leavesr;
059100120614x2        else               ;
059200120614          v1cspeA =g02dat      ;
059300120614          comspeA =g02inv      ;
059400120614          gg_speA =g02tgi      ;
059500120614 2        endif            ;
059600120614
059700120614           // Data AL no < data DAL
059800120614 2        if comspeA <comspeD     ;
059900120614           errspeA =*on;
060000120614           v1cmsg = Msg(07);
060100120614           errMessage  = *on;
060200120614           errGenerico = *on;
060300120614           leavesr;
060400120614 2         endif                   ;
060500120614
060600120614          // non si può elaborare più di un mese
060700120614          Diff=gg_spea - gg_sped  ;
060800120614 2        if Diff>35  ;
060900120614           errspeD =*on;
061000120614           v1cmsg = Msg(14);
061100120614           errMessage  = *on;
061200120614           errGenerico = *on;
061300120614           leavesr;
061400120614 2        endif  ;
061500120614 1        endif            ;
061600120614
061700120614       // Data CONSEGNA    DAL
061800120614       clear comdsd     ;
061900120704       clear gg_speD     ;
062000120704       clear gg_spea     ;
062100120614       if v1cdsd  >0      ;
062200120614         g02dat=v1cdsd    ;
062300120614         EXSR ContrDATA ;
062400120614
062500120614         if errMessage=*on ;
062600120614           errdsd  =*on;
062700120614           leavesr;
062800120614          else               ;
062900120614          v1cdsd  =g02dat      ;
063000120614          comdsd  =g02inv      ;
063100120614          gg_speD =g02tgi      ;
063200120614          endif            ;
063300120614       endif            ;
063400120614
063500120614       // Data consegna    AL
063600120614       clear comdsa     ;
063700120614       if v1cdsa>0      ;
063800120614         g02dat=v1cdsa    ;
063900120614         EXSR ContrDATA ;
064000120614
064100120614         if errMessage=*on ;
064200120614           errdsa  =*on;
064300120614           leavesr;
064400120614          else               ;
064500120614          v1cdsa  =g02dat      ;
064600120614          comdsa  =g02inv      ;
064700120614          gg_speA =g02tgi      ;
064800120614          endif            ;
064900120614
065000120614           // Data AL no < data DAL
065100120614          if comdsa  <comdsd      ;
065200120614           errdsa  =*on;
065300120614           v1cmsg = Msg(07);
065400120614           errMessage  = *on;
065500120614           errGenerico = *on;
065600120614           leavesr;
065700120614           endif                   ;
065800120614
065900120614          // non si può elaborare più di un mese
066000120614          Diff=gg_spea - gg_sped  ;
066100120614 2        if Diff>35  ;
066200120614           errdsd  =*on;
066300120614           v1cmsg = Msg(14);
066400120614           errMessage  = *on;
066500120614           errGenerico = *on;
066600120614           leavesr;
066700120614 2        endif  ;
066800120614       endif            ;
066900120614
067000120614       // Data INVENTARIO  DAL
067100120614       clear comINVRD   ;
067200120704       clear gg_speD     ;
067300120704       clear gg_spea     ;
067400120614       if v1cinvrd>0      ;
067500120614         g02dat=v1cinvrd  ;
067600120614         EXSR ContrDATA ;
067700120614
067800120614         if errMessage=*on ;
067900120614           errinvrd=*on;
068000120614           leavesr;
068100120614          else               ;
068200120614          v1cinvrd =g02dat      ;
068300120614          cominvrd =g02inv      ;
068400120614          gg_speD =g02tgi      ;
068500120614          endif            ;
068600120614       endif            ;
068700120614
068800120614       // Data inventario  AL
068900120614       clear cominvra   ;
069000120614       if v1cinvra>0      ;
069100120614         g02dat=v1cinvra  ;
069200120614         EXSR ContrDATA ;
069300120614
069400120614         if errMessage=*on ;
069500120614           errinvra=*on;
069600120614           leavesr;
069700120614          else               ;
069800120614          v1cinvra=g02dat      ;
069900120614          cominvra=g02inv      ;
070000120614          gg_speA =g02tgi      ;
070100120614          endif            ;
070200120614
070300120614           // Data AL no < data DAL
070400120614          if cominvra<cominvrd    ;
070500120614           errinvra=*on;
070600120614           v1cmsg = Msg(07);
070700120614           errMessage  = *on;
070800120614           errGenerico = *on;
070900120614           leavesr;
071000120614           endif                   ;
071100120614
071200120614          // non si può elaborare più di un mese
071300120704          //  non controllo se richeiste le mai inventariate
071400120614          Diff=gg_spea - gg_sped  ;
071500120704 2        if Diff>35  and v1cinve<>'I'  ;
071600120614           errinvrd =*on;
071700120614           v1cmsg = Msg(14);
071800120614           errMessage  = *on;
071900120614           errGenerico = *on;
072000120614           leavesr;
072100120614 2        endif  ;
072200120614       endif            ;
072300120614
072400120614        // se impostata data inventario non posso chiedere le NON inventariate
072500120614        if (v1cinvrd>0  or v1cinvra>0) and v1cinve='E'    ;
072600120614           errinvrd=*on;
072700120614           v1cmsg = Msg(13);
072800120614           errMessage  = *on;
072900120614           errGenerico = *on;
073000120614           leavesr;
073100120614        endif;
073200120614
073300120704       // Se ho richiesto le MAI inventariate, non impostare la data DAL
073400120614       //   ma solo la data inventario AL
073500120822       if v1cinve='I'   and (v1cinvrd>0  or v1cscand>0 or v1cscana>0) ;
073600120822          select ;
073700120822          when v1cinvrd>0 ;
073800120614           errinvrd=*on;
073900120822          when v1cscand>0 ;
074000120822           errscand=*on;
074100120822          other   ;
074200120822           errscana=*on;
074300120822          endsl  ;
074400120822
074500120614           v1cmsg = Msg(15);
074600120614           errMessage  = *on;
074700120614           errGenerico = *on;
074800120614           leavesr;
074900120614       endif  ;
075000120614       if v1cinve='I'   and v1cinvra=0 ;
075100120614           errinvra=*on;
075200120614           v1cmsg = Msg(15);
075300120614           errMessage  = *on;
075400120614           errGenerico = *on;
075500120614           leavesr;
075600120614       endif  ;
075700120827       if v1cinve='I'   and (v1cscan<>' ' or v1cerra<>' ')  ;
075800120824           errscan =*on;
075900120824           v1cmsg = Msg(15);
076000120824           errMessage  = *on;
076100120824           errGenerico = *on;
076200120824           leavesr;
076300120824       endif  ;
076400120824
076500120614
076600120614       // Data SCANNERIZZAZIONE
076700120614       clear comSCAND   ;
076800120704       clear gg_speD     ;
076900120704       clear gg_spea     ;
077000120614       if v1cscanD >0      ;
077100120614         g02dat=v1cscanD   ;
077200120614         EXSR ContrDATA ;
077300120614
077400120614         if errMessage=*on ;
077500120614           errscanD =*on;
077600120614           leavesr;
077700120614          else               ;
077800120614          v1cscanD =g02dat      ;
077900120614          comscanD =g02inv      ;
078000120614          gg_speD  =g02tgi      ;
078100120614          endif            ;
078200120614       endif            ;
078300120614
078400120614       // Data scannerizzazione AL
078500120614       clear comscanA    ;
0786001206141      if v1cscanA >0      ;
078700120614         g02dat=v1cscanA  ;
078800120614         EXSR ContrDATA ;
078900120614
0790001206142        if errMessage=*on ;
079100120614           errscanA= *on;
079200120614           leavesr;
079300120614x2        else               ;
079400120614          v1cscanA =g02dat      ;
079500120614          comscanA =g02inv      ;
079600120614          gg_speA  =g02tgi      ;
079700120614 2        endif            ;
079800120614
079900120614           // Data AL no < data DAL
080000120614 2        if comscanA <comscanD     ;
080100120614           errscanA =*on;
080200120614           v1cmsg = Msg(07);
080300120614           errMessage  = *on;
080400120614           errGenerico = *on;
080500120614           leavesr;
080600120614 2         endif                   ;
080700120614
080800120614          // non si può elaborare più di un mese
080900120614          Diff=gg_speA  - gg_speD   ;
081000120614 2        if Diff>35  ;
081100120614           errscanD =*on;
081200120614           v1cmsg = Msg(14);
081300120614           errMessage  = *on;
081400120614           errGenerico = *on;
081500120614           leavesr;
081600120614 2        endif  ;
081700120614 1        endif            ;
081800120704
081900120704        // se impostata data scan non posso chiedere le NON scannerizzate
082000120704        if (v1cscand>0  or v1cscana>0) and v1cscan='E'    ;
082100120704           errscanD =*on;
082200120704           v1cmsg = Msg(16);
082300120704           errMessage  = *on;
082400120704           errGenerico = *on;
082500120704           leavesr;
082600120704        endif;
082700120614       // Data INVIO IMMAGINE AL CLIENTE
082800120614       clear comINVIMD  ;
082900120704       clear gg_speD     ;
083000120704       clear gg_spea     ;
083100120614       if v1cinvimD >0      ;
083200120614         g02dat=v1cinvimD  ;
083300120614         EXSR ContrDATA ;
083400120614
083500120614         if errMessage=*on ;
083600120614           errinvimD=*on;
083700120614           leavesr;
083800120614          else               ;
083900120614          v1cinvimD=g02dat      ;
084000120614          cominvimD=g02inv      ;
084100120614          gg_speD  =g02tgi      ;
084200120614          endif            ;
084300120614       endif            ;
084400120614
084500120614       // Data invio immagine  AL
084600120614       clear cominvima    ;
0847001206141      if v1cinvimA>0      ;
084800120614         g02dat=v1cinvimA ;
084900120614         EXSR ContrDATA ;
085000120614
0851001206142        if errMessage=*on ;
085200120614           errinvimA= *on;
085300120614           leavesr;
085400120614x2        else               ;
085500120614          v1cinvimA=g02dat      ;
085600120614          cominvimA=g02inv      ;
085700120614          gg_speA  =g02tgi      ;
085800120614 2        endif            ;
085900120614
086000120614           // Data AL no < data DAL
086100120614 2        if cominvimA<cominvimD    ;
086200120614           errinvimA=*on;
086300120614           v1cmsg = Msg(07);
086400120614           errMessage  = *on;
086500120614           errGenerico = *on;
086600120614           leavesr;
086700120614 2         endif                   ;
086800120614
086900120614          // non si può elaborare più di un mese
087000120614          Diff=gg_speA  - gg_speD   ;
087100120614 2        if Diff>35  ;
087200120614           errinvimD=*on;
087300120614           v1cmsg = Msg(14);
087400120614           errMessage  = *on;
087500120614           errGenerico = *on;
087600120614           leavesr;
087700120614 2        endif  ;
087800120614 1        endif            ;
087900120614
088000120614       ENDSR   ;
088100080828       //--------------------------------------------------------------
088200080828       //?controllo della Data
088300080828       //--------------------------------------------------------------
088400080828         BEGSR  CONTRData  ;
088500080828
088600080828         clear g02err     ;
088700080828         callp xsrda8 (wlbdat)  ;
088800080828          if g02err='1'      ;
088900080828           v1cmsg = Msg(05);
089000080828           errMessage  = *on;
089100080828           errGenerico = *on;
089200080828           leavesr;
089300080828          endif            ;
089400080828
089500080828        // Non deve essere > di oggi
089600080828        if g02inv>*date       ;
089700080828           v1cmsg = Msg(06);
089800080828           errMessage  = *on;
089900080828           errGenerico = *on;
090000080828           leavesr;
090100080828        endif                 ;
090200080828        ENDSR   ;
090300080807       //--------------------------------------------------------------
090400080807       //?Operazioni finali.
090500080807       //--------------------------------------------------------------
090600080807       BEGSR RoutEnd;
090700080807
090800080807         *inLR = *on;
090900080807         return;
091000080807
091100080807       ENDSR;
091200080807
091300080828       //--------------------------------------------------------------
091400080828       //?conferma elaborazione
091500080828       //--------------------------------------------------------------
091600080807      /end-free
091700080828     c     Conferma      BEGSR
091800080901     c
091900080828     c                   clear                   kpjbu
092000120614     c                   movel     fnlsv5ds      kpjbu
092100120823     c                   movel     'LSV6'        Kcoaz
092200120829     C                   CALL      'BCH10'
092300120829     C**                 CALL      'FNLSV6R'
092400080828     C                   PARM                    KPJBA
092500080828     c
092600080828     c                   ENDSR
092700080807
092800000606**  MSG
092900080826Immettere il codice Cliente Mittente                                           1
093000080826Codice Cliente Mittente inesistente                                            2
093100080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
093200120411Immettere almeno una data
093300080827Data Errata
093400080827La data non deve essere maggiore di oggi
093500080828Data AL non può essere minore della data DAL
093600080828Impossibile effettuare contemporanemanete stampa e ristampa inventario
093700080828Per il cliente non è stata attivata la lista delle non inventariate:TEL al CED
093800110617Linea di partenza errata                                                      10
093900110617File richiedibile soltanto per le bolle non inventariate                      11
094000120827Impostare il tipo elaborazione: stampa e/o file excel                          2
094100120411Se immessa data inventario impossibile richiedere le non inventariate         13
094200120424Non è possibile elaborare più di un mese alla volta                           14
094300120704Se richieste le inventariate MAI ELABORATE,impostare solo data inventario AL  15
094400120704Se immessa data scannerizzione impossibile richiedere le non scannerizzate    16
094500120827Non indicare l'indirizzo e-mail se non richiesto il file excel                17
094600120828Se richiesto file escel indicare indirizzo e-mail                             18
094700120920                                                                              19
094800120920Utente non abilitato alla gestione dei doc. del cliente - telefonare in sede  20
