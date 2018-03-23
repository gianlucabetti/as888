000100080807     H DECEDIT('0,') DATEDIT(*ymd/)
000200120614     H* FNLSV5R *-----------------------------------------------------*
000300120614     H*   controllo documenti da restituire al mittente
000400930126     H*---------------------------------------------------------------*
000500120614     FFNLsv5d   CF   E             WORKSTN indds(IndDspF)
000600080807     f                                     infds(InfDspF)
000700080319     Ftabel00f  IF   E           K DISK
000800080807     Fcnaco00f  IF   E           K DISK
000900080902     Fazorg01l  IF   E           K DISK
001000080319     d*-----------------
001100080807      // - Tasti funzionali a video
001200080807     d c_F01           c                   const(x'31')
001300080807     d c_F02           c                   const(x'32')
001400080807     d c_F03           c                   const(x'33')
001500080807     d c_F05           c                   const(x'35')
001600080807     d c_F06           c                   const(x'36')
001700080807     d c_F07           c                   const(x'37')
001800080807     d c_F08           c                   const(x'38')
001900080807     d c_F09           c                   const(x'39')
002000080807     d c_F10           c                   const(x'3A')
002100080807     d c_F12           c                   const(x'3C')
002200080807     d c_F13           c                   const(x'B1')
002300080807     d c_F14           c                   const(x'B2')
002400080807     d c_F15           c                   const(x'B3')
002500080807     d c_F16           c                   const(x'B4')
002600080807     d c_F17           c                   const(x'B5')
002700080807     d c_F18           c                   const(x'B6')
002800080807     d c_F19           c                   const(x'B7')
002900080807     d c_F20           c                   const(x'B8')
003000080807     d c_F21           c                   const(x'B9')
003100080807     d c_F22           c                   const(x'BA')
003200080807     d c_F23           c                   const(x'BB')
003300080807     d c_F24           c                   const(x'BC')
003400080807     d InfDspF         ds
003500080807     d  dsp_aid              369    369a
003600080807     d  sfl_rrn              376    377i 0
003700080807     d  min_nrr              378    379i 0
003800080807     d  num_rcds             380    381i 0
003900080807
004000080807      // - Indicatori su DspF
004100080807     d IndDspF         ds
004200080807        // - Indicatori di errore
004300080807     d  errMessage                    1n   overlay(IndDspF : 28)
004400080807     d  errGenerico                   1n   overlay(IndDspF : 99)
004500080807        // - Indicatori vari
004600080807     d  ErrKSC                        1n   overlay(IndDspF : 50)
004700120402     d  Errsped                       1n   overlay(IndDspF : 52)
004800120402     d  Errspea                       1n   overlay(IndDspF : 53)
004900080827     d  ErrDSD                        1n   overlay(IndDspF : 54)
005000080827     d  ErrDSA                        1n   overlay(IndDspF : 55)
005100080902     d  ErrLNP                        1n   overlay(IndDspF : 56)
005200110617     d  Erremail                      1n   overlay(IndDspF : 57)
005300120411     d  Errinvrd                      1n   overlay(IndDspF : 58)
005400120411     d  Errinvra                      1n   overlay(IndDspF : 59)
005500120614     d  Errscand                      1n   overlay(IndDspF : 60)
005600120614     d  Errscana                      1n   overlay(IndDspF : 61)
005700120614     d  Errinvimd                     1n   overlay(IndDspF : 62)
005800120614     d  Errinvima                     1n   overlay(IndDspF : 63)
005900120824     d  Errscan                       1n   overlay(IndDspF : 64)
006000120828     d  Errsta                        1n   overlay(IndDspF : 65)
006100080807
006200080807     d*-----------------
006300120828     D MSG             S             78    DIM(18) CTDATA PERRCD(1)
006400080827     D SKI7R           S              2    DIM(50)
006500080807     d*-----------------
006600911010     D WLBDAT          DS
006700941221     D  G02DAT                 1      8  0
006800941221     D  G02INV                 9     16  0
006900941221     D  G02ERR                17     17
007000941221     D  G02TGI                18     22  0
007100080828
007200120614     D fnlsv5ds        DS
007300120822     d* data inventario DAL/AL
007400120411     D  v1cinvrd              17     24  0
007500120411     D  cominvrd              25     32  0
007600120411     D  v1cinvra              33     40  0
007700120411     D  cominvra              41     48  0
007800120405
007900120713     d* Data CONSEGNA
008000080828     D  v1cdsd                49     56  0
008100080828     D  comdsd                57     64  0
008200080828     D  v1cdsa                65     72  0
008300080828     D  comdsa                73     80  0
008400120405     D
008500080829     D  v1cksc                81     87  0
008600080829     D  v1dksc                88    112
008700080902     D  v1clnp               113    115  0
008800080902     D  v1dlnp               116    140
008900110617     D  v1cemail             142    171
009000120713     d*
009100120713     d* data spedizione
009200120405     D  comsped              173    180  0
009300120405     D  comspea              181    188  0
009400120405     D  v1cinve              189    189
009500120405     D  v1cerra              190    190
009600120713     d* Data scannerizzazione
009700120614     D  comscand             191    198  0
009800120614     D  comscana             199    206  0
009900120713     d*   Per il momento i prossimi 3 campi
010000120713     d*                  le date restituzione al mittente sono oscurate e non
010100120704     d*    usate
010200120614     D  cominvimD            207    214  0
010300120614     D  cominvimA            215    222  0
010400120713     D  v1cinvim             223    223
010500120704     d
010600120704     D  v1cscan              224    224
010700120614     D  v1csta               225    225
010800080319     d
010900040604     D KPJBA         E DS
011000080807     D tibs34ds      E DS
011100080319     D ds7N          E DS
011200111228     D dsemail       E DS
011300080807     D ds7PQRS       E DS
011400080807     d kscabi                         2    DIM(30)  overlay(ds7cdp)
011500080319     d
011600080807     d DS7R          e ds
011700080826      // - Ricerca/Controllo tabelle
011800080826     d TIBS02ds      e ds                  inz
011900040614     D*
012000080319     d Dataiso         s               d   datfmt(*iso)
012100080826     d timeiso         s               t   timfmt(*iso)
012200080319     d Dataeur         s               d   datfmt(*eur)
012300080807     D W007A           S              7
012400080807     D $video          S              2    inz('D1')
012500080807     D $inzd01         S              1
012600120614     D $inzd02         S              1
012700080807     d $Fine           s               n   inz(*off)
012800080807     d Primavolta      s              1    inz
012900080807     D XX              S              3  0 inz
013000080807     D Indx            S              3  0 inz
013100120704     d Diff            s             13  0
013200120424     d gg_speD         s                   like(g02tgi)
013300120424     d gg_speA         s                   like(g02tgi)
013400120614     d gg_scanD        s                   like(g02tgi)
013500120614     d gg_scanA        s                   like(g02tgi)
013600080807     d
013700080807     d  xpardut        s             30
013800080807     d  xparkut        s              1  0
013900080807     d  xparrag        s             48
014000080807     d  xparkcc        s              4  0
014100080807     d  xparsta        s              1  0
014200080807     d  xparflr        s             90
014300080807     d  xpardit        s              3
014400080807     d  xparnum        s              2  0
014500080807     d  xparkcm        s             80
014600080807     d  xparksm        s            140
014700080807     d  xparkdm        s             60
014800080807     d  xparesci       s              1
014900080807     d  xparerr        s              2
015000080807     d  xpariva        s             16
015100080807     d  xparcdf        s             16
015200080807      //---------------------------------------------------------------
015300080807      //?Definizione aree dati.
015400080807      //---------------------------------------------------------------
015500080807
015600080807      // - Dati utente
015700080807     d §AzUte        e ds                  extname(AZUTE00F)
015800080807     d                                     dtaara
015900080807     d §DatiUte      e ds                  extname(dDatiUte)
016000080807     d                                     dtaara
016100080807      //---------------------------------------------------------------
016200080807      //?Definizione procedure usate.
016300080807      //---------------------------------------------------------------
016400080807      /copy gaitrasrc/srcprotopr,XALFA3BR
016500080807      /copy gaitrasrc/srcprotopr,tibs34r
016600080807      /copy gaitrasrc/srcprotopr,trtb70r
016700080826      /copy gaitrasrc/srcprotopr,tibs02r
016800080827      /copy gaitrasrc/srcprotopr,xsrda8
016900111228      /copy gaitrasrc/srcprotopr,xemail
017000000000     C*---------------------------------------------------------------*
017100911010     C* INDICATORI
017200911010     C*---------------------------------------------------------------*
017300951115     C*  28    - EMETTE CAMPO MESSAGGI
017400040607     C*  40    - ERRORE
017500911010     C*  90    - ERRORE GENERICO
017600911010     C*---------------------------------------------------------------*
017700000000     C     *ENTRY        PLIST
017800000000     C                   PARM                    KPJBA
017900080807
018000080807      /free
018100080807
018200080807       // Operazioni iniziali
018300080807       exsr RoutInz;
018400080807
018500080807       // Gestione video
018600080807       DOW $Fine = *off;
018700080807         select;
018800080807
018900120614         // Videata di selezioni  1
019000080807           when $Video = 'D1';
019100080807             exsr GesD01;
019200080807
019300120614         // Videata di selezioni  2
019400120614           when $Video = 'D2';
019500120614             exsr GesD02;
019600120614
019700080807           other   ;
019800080807             $Fine = *on;
019900080807         endsl;
020000080807       ENDDO;
020100080807
020200080807       // Operazioni finali
020300080807       exsr RoutEnd;
020400080807
020500080807       //--------------------------------------------------------------
020600080807       //?Operazioni iniziali.
020700080807       //--------------------------------------------------------------
020800080807       BEGSR RoutInz;
020900080807       $inzD01=*on;
021000080807
021100080807       // Solo la prima volta
021200080807 1     if primavolta=' '   ;
021300080807
021400080807         // Reperimento dati job
021500080807         exsr DatiJob;
021600080807
021700080807         // Carico tutte le particolarità consegne che prevedono
021800080807         //  la restituzione della BAM
021900080807
022000080807         clear XX   ;
022100080807         setll (1:'7R')  tabel00f    ;
022200080807         reade (1:'7R')  tabel00f    ;
022300080807
022400080807         dow not %eof(tabel00f)  ;
022500080807
022600080807         if tblflg=' '   ;
022700080807           ds7r=tbluni   ;
022800080807           if §7r1bf='B' ;
022900080807            xx=xx+1   ;
023000080807            SKI7r(xx)=tblkey   ;
023100080807           endif     ;
023200080807         endif           ;
023300080807
023400080807         reade (1:'7R')  tabel00f    ;
023500080807         enddo     ;
023600080807 1       endif    ;
023700080807
023800080807         ENDSR;
023900080807
024000080807       //--------------------------------------------------------------
024100080807       //?Reperimento Dati del job (Utente/Operativi).
024200080807       //--------------------------------------------------------------
024300080807       BEGSR DatiJob;
024400080807
024500080807         in(E) §AzUte;
024600080807         if NOT %error;
024700080807           in(E) §DatiUte;
024800080807         endif;
024900080807         if %error or RSut = *blanks;
025000080807           clear TIBS34ds;
025100080807           tibs34r(tibs34ds);
025200080807           in §AzUte;
025300080807           in §DatiUte;
025400080807         endif;
025500080807
025600080807       ENDSR;
025700080807       //--------------------------------------------------------------
025800080807       //?Gestione video 1
025900080807       //--------------------------------------------------------------
026000080807       BEGSR GesD01;
026100080807
026200080807         // Inizializzazione videata
026300080807         if  $Inzd01 = *on;
026400080807            exsr InzD01;
026500080807            $Inzd01  = *off;
026600080807         endif;
026700080807
026800080807         // Emissione videata
026900120614         exfmt  lsv5d01;
027000080807
027100080807         reset errMessage;
027200080807         reset errGenerico;
027300080807         clear V1cmsg;
027400080807         errksc=*off     ;
027500080902         errlnp=*off     ;
027600110617         erremail=*off   ;
027700120828         errsta  =*off   ;
027800080807
027900080807 1       SELECT;
028000080807
028100080807         // - F3=Fine
028200080807 1         WHEN  dsp_aid = c_F03;
028300080807            $Fine = *on;
028400080807
028500080807 x1      // Invio
028600080807           OTHER;
028700080807             exsr ContrD01 ;
028800120614
028900080807 2           if  errGenerico = *on;
029000080807               leavesr;
029100080807 2           endif;
029200120614
029300120614            $Video='D2'  ;
029400120614            $inzD02=*on;
029500080807
029600080807 1       ENDSL;
029700080807
029800080807       ENDSR;
029900080807       //--------------------------------------------------------------
030000080807       //?controlli video1
030100080807       //--------------------------------------------------------------
030200080807       BEGSR ContrD01 ;
030300080807       select    ;
030400080807       when v1cksc=0 and v1dksc=*blanks               ;
030500080807           errksc=*on;
030600080807           v1cmsg = Msg(01);
030700080807           errMessage  = *on;
030800080807           errGenerico = *on;
030900080807           leavesr;
031000080807
031100080807       when v1cksc=0 and v1dksc<>*blanks   ;
031200080807       xparsta=9   ;
031300080807       xpardut=rsut   ;
031400080807       clear xparrag   ;
031500080807       xparrag  =v1dksc  ;
031600080807       xparkcc=dutkci    ;
031700080807       clear xparflr  ;
031800080807       clear xpardit  ;
031900080807       xparnum=1     ;
032000080807       clear xparkcm ;
032100080807       clear xparksm ;
032200080807       clear xparkdm ;
032300080807       clear xparesci;
032400080807       clear xparerr ;
032500080807       clear xpariva ;
032600080807       clear xparcdf ;
032700080807       xalfa3br (xpardut:xparkut:xparrag:xparkcc:xparsta:xparflr:xpardit
032800080807                 :xparnum:xparkcm:xparksm:xparkdm:xparesci:xparerr:xpariva
032900080807                 :xparcdf)             ;
033000080807       if xparsta<>-1  ;
033100080807       w007a=XPARKSM  ;
033200080807       V1CKSC=%int(W007A)  ;
033300080807       v1dksc=xparrag   ;
033400080807       else   ;
033500080807       clear v1dksc    ;
033600080807       endif    ;
033700080807       ErrGenerico=*on   ;
033800080807       leavesr   ;
033900080807
034000080807       other     ;
034100080807       chain   (1:dutkci:v1cksc)   cnaco000   ;
034200080807       if   not %found(cnaco00f) or acoflg<>' '    ;
034300080807           errksc=*on;
034400080807           v1cmsg = Msg(02);
034500080807           errMessage  = *on;
034600080807           errGenerico = *on;
034700080807           clear v1dksc    ;
034800080807           leavesr;
034900080807       endif   ;
035000080807
035100080807       v1dksc=acorag   ;
035200080807       ENDSL   ;
035300080807
035400080807       // Verifico se il cliente prevede la particolarità giusta
035500080807       clear ds7pqrs   ;
035600080807       ds7ksc=v1cksc   ;
035700080826       ds7ges='O'      ;
035800080807       ds7ric='S'      ;
035900080807       kpjbu=ds7pqrs   ;
036000080807       TRTB70R (KPJBA)   ;
036100080807       ds7pqrs=kpjbu     ;
036200080807
036300080807       xx=1   ;
036400080807       clear Indx    ;
036500080807       dow kscabi(xx)<>*blanks  and Indx=0;
036600080807       Indx=%lookup(kscabi(xx):SKI7R)  ;
036700080807       xx=xx+1  ;
036800080807       enddo    ;
036900080807
037000080807       if Indx=0   ;
037100080807           errksc=*on;
037200080807           v1cmsg = Msg(03);
037300080807           errMessage  = *on;
037400080807           errGenerico = *on;
037500080807           leavesr;
037600080807       endif       ;
037700080827
037800080902       // controllo la linea di partenza immessa
037900080902       clear v1dlnp    ;
038000080902       if v1clnp>0        ;
038100080902       chain  v1clnp   azorg01l   ;
038200080903        if not %found(azorg01l) or orgfva<>' ' or (orgfag<>'A' and
038300080903                                                   orgfag<>'F') ;
038400080902           errlnp=*on;
038500080902           v1cmsg = Msg(10);
038600080902           errMessage  = *on;
038700080902           errGenerico = *on;
038800080902           leavesr;
038900080902        endif   ;
039000080903        v1dlnp=orgdes    ;
039100080902       endif   ;
039200110617
039300120614       // Richidere almeno una elaborazione
039400120827       if (v1csta  =' ' ) ;
039500120828           errsta  =*on;
039600110617           v1cmsg = Msg(12);
039700110617           errMessage  = *on;
039800110617           errGenerico = *on;
039900110617           leavesr;
040000110617       endif   ;
040100120827       if v1cemail<>*blanks  and v1csta<>'E'   and v1csta<>'X'  ;
040200120828           errsta  =*on;
040300120827           v1cmsg = Msg(17);
040400120827           errMessage  = *on;
040500120827           errGenerico = *on;
040600120827           leavesr;
040700120827       endif   ;
040800120828       if v1cemail= *blanks  and (v1csta='E'  or v1csta='X' ) ;
040900120828           erremail=*on;
041000120828           v1cmsg = Msg(18);
041100120828           errMessage  = *on;
041200120828           errGenerico = *on;
041300120828           leavesr;
041400120828       endif   ;
041500110613
041600111228       // controllo formale indirizzo email
041700111228       if v1cemail<>*blanks   ;
041800111228       clear dsemail  ;
041900120302       §EMLINDI=%trim(v1cemail)+'@brt.it'   ;
042000111228       callp XEMAIL   (dsemail)  ;
042100111228         if §EMLERRO<>*blanks and §EMLERRO<> '0';
042200111228             erremail=*on;
042300111228             v1cmsg = §emlmsgo   ;
042400111228             errMessage  = *on;
042500111228             errGenerico = *on;
042600111228             leavesr;
042700111228         endif   ;
042800111228       endif   ;
042900111228
043000080807       ENDSR   ;
043100080807       //--------------------------------------------------------------
043200080807       //?Inizializzazione video1
043300080807       //--------------------------------------------------------------
043400080807       BEGSR InzD01;
043500080807         clear v1cksc;
043600080807         clear v1dksc;
043700080903         clear v1clnp  ;
043800080903         clear v1dlnp  ;
043900110617         clear v1cemail;
044000120614         clear v1csta  ;
044100080807       ENDSR  ;
044200120614       //--------------------------------------------------------------
044300120614       //?Gestione video 2
044400120614       //--------------------------------------------------------------
044500120614       BEGSR GesD02;
044600120614
044700120614         // Inizializzazione videata
044800120614         if  $Inzd02 = *on;
044900120614            exsr InzD02;
045000120614            $Inzd02  = *off;
045100120614         endif;
045200120614
045300120614         // Emissione videata
045400120614         exfmt  lsv5d02;
045500120614
045600120614         reset errMessage;
045700120614         reset errGenerico;
045800120614         clear V1cmsg;
045900120614         errsped =*off     ;
046000120614         errspea =*off     ;
046100120614         errdsd=*off     ;
046200120614         errdsa=*off     ;
046300120614         errinvrd=*off     ;
046400120614         errinvra=*off     ;
046500120614         errscand=*off     ;
046600120614         errscana=*off     ;
046700120614         errinvimd=*off     ;
046800120614         errinvima=*off     ;
046900120824         errscan  =*off     ;
047000120614
047100120614 1       SELECT;
047200120614
047300120614         // - F3=Fine
047400120614 1         WHEN  dsp_aid = c_F03;
047500120614            $Fine = *on;
047600120614 1         WHEN  dsp_aid = c_F12;
047700120614            $video='D1' ;
047800120614
047900120614 x1      // Invio
048000120614           OTHER;
048100120614             exsr ContrD02 ;
048200120614 2           if  errGenerico = *on;
048300120614               leavesr;
048400120614 2           endif;
048500120614
048600120614           // F6- Conferma
048700120614 1         if  dsp_aid = c_F06;
048800120614            exsr Conferma      ;
048900120614            $Inzd01 = *on;
049000120614            $Video='D1'  ;
049100120614           endif              ;
049200120614
049300120614 1       ENDSL;
049400120614
049500120614       ENDSR;
049600120614       //--------------------------------------------------------------
049700120614       //?Inizializzazione video2
049800120614       //--------------------------------------------------------------
049900120614       BEGSR InzD02;
050000120614         clear v1cspeD ;
050100120614         clear v1cspeA ;
050200120614         clear v1cdsd  ;
050300120614         clear v1cdsa  ;
050400120614         clear v1cinvrd  ;
050500120614         clear v1cinvra  ;
050600120614         clear v1cscand  ;
050700120614         clear v1cscana  ;
050800120614         clear v1cinvimd ;
050900120614         clear v1cinvima ;
051000120614         v1cerra =' '  ;
051100120614         v1cinve =' '  ;
051200120614         v1cscan =' '  ;
051300120614         v1cinvim =' '  ;
051400120614       ENDSR  ;
051500120614       //--------------------------------------------------------------
051600120614       //?controlli video2
051700120614       //--------------------------------------------------------------
051800120614       BEGSR ContrD02 ;
051900120614
052000120614       // Immettere almeno una data
052100120614
052200120614       if v1csped=0 and v1cspea =0  and v1cdsd=0 and
052300120614          v1cdsa=0  and v1cinvrd=0 and v1cinvra=0  and
052400120614          v1cinvimD=0  and v1cinvimA=0 and v1cscanD=0  and v1cscanA=0 ;
052500120614           errspeD= *on;
052600120614           v1cmsg = Msg(04);
052700120614           errMessage  = *on;
052800120614           errGenerico = *on;
052900120614           leavesr;
053000120614       endif          ;
053100120614
053200120614       // Data SPEDIZIONE DAL
053300120614       clear comsped    ;
053400120704       clear gg_speD     ;
053500120704       clear gg_spea     ;
053600120614       if v1cspeD >0      ;
053700120614         g02dat=v1cspeD   ;
053800120614         EXSR ContrDATA ;
053900120614
054000120614         if errMessage=*on ;
054100120614           errspeD =*on;
054200120614           leavesr;
054300120614          else               ;
054400120614          v1cspeD =g02dat      ;
054500120614          comspeD =g02inv      ;
054600120614          gg_speD =g02tgi      ;
054700120614          endif            ;
054800120614       endif            ;
054900120614
055000120614       // Data spedizione  AL
055100120614       clear comspeA    ;
0552001206141      if v1cspeA >0      ;
055300120614         g02dat=v1cspeA  ;
055400120614         EXSR ContrDATA ;
055500120614
0556001206142        if errMessage=*on ;
055700120614           errspeA= *on;
055800120614           leavesr;
055900120614x2        else               ;
056000120614          v1cspeA =g02dat      ;
056100120614          comspeA =g02inv      ;
056200120614          gg_speA =g02tgi      ;
056300120614 2        endif            ;
056400120614
056500120614           // Data AL no < data DAL
056600120614 2        if comspeA <comspeD     ;
056700120614           errspeA =*on;
056800120614           v1cmsg = Msg(07);
056900120614           errMessage  = *on;
057000120614           errGenerico = *on;
057100120614           leavesr;
057200120614 2         endif                   ;
057300120614
057400120614          // non si può elaborare più di un mese
057500120614          Diff=gg_spea - gg_sped  ;
057600120614 2        if Diff>35  ;
057700120614           errspeD =*on;
057800120614           v1cmsg = Msg(14);
057900120614           errMessage  = *on;
058000120614           errGenerico = *on;
058100120614           leavesr;
058200120614 2        endif  ;
058300120614 1        endif            ;
058400120614
058500120614       // Data CONSEGNA    DAL
058600120614       clear comdsd     ;
058700120704       clear gg_speD     ;
058800120704       clear gg_spea     ;
058900120614       if v1cdsd  >0      ;
059000120614         g02dat=v1cdsd    ;
059100120614         EXSR ContrDATA ;
059200120614
059300120614         if errMessage=*on ;
059400120614           errdsd  =*on;
059500120614           leavesr;
059600120614          else               ;
059700120614          v1cdsd  =g02dat      ;
059800120614          comdsd  =g02inv      ;
059900120614          gg_speD =g02tgi      ;
060000120614          endif            ;
060100120614       endif            ;
060200120614
060300120614       // Data consegna    AL
060400120614       clear comdsa     ;
060500120614       if v1cdsa>0      ;
060600120614         g02dat=v1cdsa    ;
060700120614         EXSR ContrDATA ;
060800120614
060900120614         if errMessage=*on ;
061000120614           errdsa  =*on;
061100120614           leavesr;
061200120614          else               ;
061300120614          v1cdsa  =g02dat      ;
061400120614          comdsa  =g02inv      ;
061500120614          gg_speA =g02tgi      ;
061600120614          endif            ;
061700120614
061800120614           // Data AL no < data DAL
061900120614          if comdsa  <comdsd      ;
062000120614           errdsa  =*on;
062100120614           v1cmsg = Msg(07);
062200120614           errMessage  = *on;
062300120614           errGenerico = *on;
062400120614           leavesr;
062500120614           endif                   ;
062600120614
062700120614          // non si può elaborare più di un mese
062800120614          Diff=gg_spea - gg_sped  ;
062900120614 2        if Diff>35  ;
063000120614           errdsd  =*on;
063100120614           v1cmsg = Msg(14);
063200120614           errMessage  = *on;
063300120614           errGenerico = *on;
063400120614           leavesr;
063500120614 2        endif  ;
063600120614       endif            ;
063700120614
063800120614       // Data INVENTARIO  DAL
063900120614       clear comINVRD   ;
064000120704       clear gg_speD     ;
064100120704       clear gg_spea     ;
064200120614       if v1cinvrd>0      ;
064300120614         g02dat=v1cinvrd  ;
064400120614         EXSR ContrDATA ;
064500120614
064600120614         if errMessage=*on ;
064700120614           errinvrd=*on;
064800120614           leavesr;
064900120614          else               ;
065000120614          v1cinvrd =g02dat      ;
065100120614          cominvrd =g02inv      ;
065200120614          gg_speD =g02tgi      ;
065300120614          endif            ;
065400120614       endif            ;
065500120614
065600120614       // Data inventario  AL
065700120614       clear cominvra   ;
065800120614       if v1cinvra>0      ;
065900120614         g02dat=v1cinvra  ;
066000120614         EXSR ContrDATA ;
066100120614
066200120614         if errMessage=*on ;
066300120614           errinvra=*on;
066400120614           leavesr;
066500120614          else               ;
066600120614          v1cinvra=g02dat      ;
066700120614          cominvra=g02inv      ;
066800120614          gg_speA =g02tgi      ;
066900120614          endif            ;
067000120614
067100120614           // Data AL no < data DAL
067200120614          if cominvra<cominvrd    ;
067300120614           errinvra=*on;
067400120614           v1cmsg = Msg(07);
067500120614           errMessage  = *on;
067600120614           errGenerico = *on;
067700120614           leavesr;
067800120614           endif                   ;
067900120614
068000120614          // non si può elaborare più di un mese
068100120704          //  non controllo se richeiste le mai inventariate
068200120614          Diff=gg_spea - gg_sped  ;
068300120704 2        if Diff>35  and v1cinve<>'I'  ;
068400120614           errinvrd =*on;
068500120614           v1cmsg = Msg(14);
068600120614           errMessage  = *on;
068700120614           errGenerico = *on;
068800120614           leavesr;
068900120614 2        endif  ;
069000120614       endif            ;
069100120614
069200120614        // se impostata data inventario non posso chiedere le NON inventariate
069300120614        if (v1cinvrd>0  or v1cinvra>0) and v1cinve='E'    ;
069400120614           errinvrd=*on;
069500120614           v1cmsg = Msg(13);
069600120614           errMessage  = *on;
069700120614           errGenerico = *on;
069800120614           leavesr;
069900120614        endif;
070000120614
070100120704       // Se ho richiesto le MAI inventariate, non impostare la data DAL
070200120614       //   ma solo la data inventario AL
070300120822       if v1cinve='I'   and (v1cinvrd>0  or v1cscand>0 or v1cscana>0) ;
070400120822          select ;
070500120822          when v1cinvrd>0 ;
070600120614           errinvrd=*on;
070700120822          when v1cscand>0 ;
070800120822           errscand=*on;
070900120822          other   ;
071000120822           errscana=*on;
071100120822          endsl  ;
071200120822
071300120614           v1cmsg = Msg(15);
071400120614           errMessage  = *on;
071500120614           errGenerico = *on;
071600120614           leavesr;
071700120614       endif  ;
071800120614       if v1cinve='I'   and v1cinvra=0 ;
071900120614           errinvra=*on;
072000120614           v1cmsg = Msg(15);
072100120614           errMessage  = *on;
072200120614           errGenerico = *on;
072300120614           leavesr;
072400120614       endif  ;
072500120827       if v1cinve='I'   and (v1cscan<>' ' or v1cerra<>' ')  ;
072600120824           errscan =*on;
072700120824           v1cmsg = Msg(15);
072800120824           errMessage  = *on;
072900120824           errGenerico = *on;
073000120824           leavesr;
073100120824       endif  ;
073200120824
073300120614
073400120614       // Data SCANNERIZZAZIONE
073500120614       clear comSCAND   ;
073600120704       clear gg_speD     ;
073700120704       clear gg_spea     ;
073800120614       if v1cscanD >0      ;
073900120614         g02dat=v1cscanD   ;
074000120614         EXSR ContrDATA ;
074100120614
074200120614         if errMessage=*on ;
074300120614           errscanD =*on;
074400120614           leavesr;
074500120614          else               ;
074600120614          v1cscanD =g02dat      ;
074700120614          comscanD =g02inv      ;
074800120614          gg_speD  =g02tgi      ;
074900120614          endif            ;
075000120614       endif            ;
075100120614
075200120614       // Data scannerizzazione AL
075300120614       clear comscanA    ;
0754001206141      if v1cscanA >0      ;
075500120614         g02dat=v1cscanA  ;
075600120614         EXSR ContrDATA ;
075700120614
0758001206142        if errMessage=*on ;
075900120614           errscanA= *on;
076000120614           leavesr;
076100120614x2        else               ;
076200120614          v1cscanA =g02dat      ;
076300120614          comscanA =g02inv      ;
076400120614          gg_speA  =g02tgi      ;
076500120614 2        endif            ;
076600120614
076700120614           // Data AL no < data DAL
076800120614 2        if comscanA <comscanD     ;
076900120614           errscanA =*on;
077000120614           v1cmsg = Msg(07);
077100120614           errMessage  = *on;
077200120614           errGenerico = *on;
077300120614           leavesr;
077400120614 2         endif                   ;
077500120614
077600120614          // non si può elaborare più di un mese
077700120614          Diff=gg_speA  - gg_speD   ;
077800120614 2        if Diff>35  ;
077900120614           errscanD =*on;
078000120614           v1cmsg = Msg(14);
078100120614           errMessage  = *on;
078200120614           errGenerico = *on;
078300120614           leavesr;
078400120614 2        endif  ;
078500120614 1        endif            ;
078600120704
078700120704        // se impostata data scan non posso chiedere le NON scannerizzate
078800120704        if (v1cscand>0  or v1cscana>0) and v1cscan='E'    ;
078900120704           errscanD =*on;
079000120704           v1cmsg = Msg(16);
079100120704           errMessage  = *on;
079200120704           errGenerico = *on;
079300120704           leavesr;
079400120704        endif;
079500120614       // Data INVIO IMMAGINE AL CLIENTE
079600120614       clear comINVIMD  ;
079700120704       clear gg_speD     ;
079800120704       clear gg_spea     ;
079900120614       if v1cinvimD >0      ;
080000120614         g02dat=v1cinvimD  ;
080100120614         EXSR ContrDATA ;
080200120614
080300120614         if errMessage=*on ;
080400120614           errinvimD=*on;
080500120614           leavesr;
080600120614          else               ;
080700120614          v1cinvimD=g02dat      ;
080800120614          cominvimD=g02inv      ;
080900120614          gg_speD  =g02tgi      ;
081000120614          endif            ;
081100120614       endif            ;
081200120614
081300120614       // Data invio immagine  AL
081400120614       clear cominvima    ;
0815001206141      if v1cinvimA>0      ;
081600120614         g02dat=v1cinvimA ;
081700120614         EXSR ContrDATA ;
081800120614
0819001206142        if errMessage=*on ;
082000120614           errinvimA= *on;
082100120614           leavesr;
082200120614x2        else               ;
082300120614          v1cinvimA=g02dat      ;
082400120614          cominvimA=g02inv      ;
082500120614          gg_speA  =g02tgi      ;
082600120614 2        endif            ;
082700120614
082800120614           // Data AL no < data DAL
082900120614 2        if cominvimA<cominvimD    ;
083000120614           errinvimA=*on;
083100120614           v1cmsg = Msg(07);
083200120614           errMessage  = *on;
083300120614           errGenerico = *on;
083400120614           leavesr;
083500120614 2         endif                   ;
083600120614
083700120614          // non si può elaborare più di un mese
083800120614          Diff=gg_speA  - gg_speD   ;
083900120614 2        if Diff>35  ;
084000120614           errinvimD=*on;
084100120614           v1cmsg = Msg(14);
084200120614           errMessage  = *on;
084300120614           errGenerico = *on;
084400120614           leavesr;
084500120614 2        endif  ;
084600120614 1        endif            ;
084700120614
084800120614       ENDSR   ;
084900080828       //--------------------------------------------------------------
085000080828       //?controllo della Data
085100080828       //--------------------------------------------------------------
085200080828         BEGSR  CONTRData  ;
085300080828
085400080828         clear g02err     ;
085500080828         callp xsrda8 (wlbdat)  ;
085600080828          if g02err='1'      ;
085700080828           v1cmsg = Msg(05);
085800080828           errMessage  = *on;
085900080828           errGenerico = *on;
086000080828           leavesr;
086100080828          endif            ;
086200080828
086300080828        // Non deve essere > di oggi
086400080828        if g02inv>*date       ;
086500080828           v1cmsg = Msg(06);
086600080828           errMessage  = *on;
086700080828           errGenerico = *on;
086800080828           leavesr;
086900080828        endif                 ;
087000080828        ENDSR   ;
087100080807       //--------------------------------------------------------------
087200080807       //?Operazioni finali.
087300080807       //--------------------------------------------------------------
087400080807       BEGSR RoutEnd;
087500080807
087600080807         *inLR = *on;
087700080807         return;
087800080807
087900080807       ENDSR;
088000080807
088100080828       //--------------------------------------------------------------
088200080828       //?conferma elaborazione
088300080828       //--------------------------------------------------------------
088400080807      /end-free
088500080828     c     Conferma      BEGSR
088600080901     c
088700080828     c                   clear                   kpjbu
088800120614     c                   movel     fnlsv5ds      kpjbu
088900120823     c                   movel     'LSV6'        Kcoaz
089000120829     C                   CALL      'BCH10'
089100120829     C**                 CALL      'FNLSV6R'
089200080828     C                   PARM                    KPJBA
089300080828     c
089400080828     c                   ENDSR
089500080807
089600000606**  MSG
089700080826Immettere il codice Cliente Mittente                                           1
089800080826Codice Cliente Mittente inesistente                                            2
089900080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
090000120411Immettere almeno una data
090100080827Data Errata
090200080827La data non deve essere maggiore di oggi
090300080828Data AL non può essere minore della data DAL
090400080828Impossibile effettuare contemporanemanete stampa e ristampa inventario
090500080828Per il cliente non è stata attivata la lista delle non inventariate:TEL al CED
090600110617Linea di partenza errata                                                      10
090700110617File richiedibile soltanto per le bolle non inventariate                      11
090800120827Impostare il tipo elaborazione: stampa e/o file excel                          2
090900120411Se immessa data inventario impossibile richiedere le non inventariate         13
091000120424Non è possibile elaborare più di un mese alla volta                           14
091100120704Se richieste le inventariate MAI ELABORATE,impostare solo data inventario AL  15
091200120704Se immessa data scannerizzione impossibile richiedere le non scannerizzate    16
091300120827Non indicare l'indirizzo e-mail se non richiesto il file excel                17
091400120828Se richiesto file escel indicare indirizzo e-mail                             18
