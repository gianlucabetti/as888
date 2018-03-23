000100080807     H DECEDIT('0,') DATEDIT(*ymd/)
000200120402     H* FNLSV4R *-----------------------------------------------------*
000300120404     H*   File excel DDT      da restituire al mittente
000400930126     H*---------------------------------------------------------------*
000500120402     FFNLsv4d   CF   E             WORKSTN indds(IndDspF)
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
005500080807
005600080807     d*-----------------
005700080826     D MSG             S             78    DIM(14) CTDATA PERRCD(1)
005800080827     D SKI7R           S              2    DIM(50)
005900080807     d*-----------------
006000911010     D WLBDAT          DS
006100941221     D  G02DAT                 1      8  0
006200941221     D  G02INV                 9     16  0
006300941221     D  G02ERR                17     17
006400941221     D  G02TGI                18     22  0
006500080828
006600120402     D fnlsv4ds        DS
006700120405     D  v1cinv                 1      8  0   inz
006800120405     D  cominv                 9     16  0   inz
006900120411     D  v1cinvrd              17     24  0
007000120411     D  cominvrd              25     32  0
007100120411     D  v1cinvra              33     40  0
007200120411     D  cominvra              41     48  0
007300120405
007400080828     D  v1cdsd                49     56  0
007500080828     D  comdsd                57     64  0
007600080828     D  v1cdsa                65     72  0
007700080828     D  comdsa                73     80  0
007800120405     D
007900080829     D  v1cksc                81     87  0
008000080829     D  v1dksc                88    112
008100080902     D  v1clnp               113    115  0
008200080902     D  v1dlnp               116    140
008300110610     D  v1csfile             141    141
008400110617     D  v1cemail             142    171
008500120405     d
008600120405     d  listaGEN             172    172      inz('S')
008700120405     D  comsped              173    180  0
008800120405     D  comspea              181    188  0
008900120405     D  v1cinve              189    189
009000120405     D  v1cerra              190    190
009100080319     d
009200040604     D KPJBA         E DS
009300080807     D tibs34ds      E DS
009400080319     D ds7N          E DS
009500111228     D dsemail       E DS
009600110805     D** dcli          E DS
009700080807     D ds7PQRS       E DS
009800080807     d kscabi                         2    DIM(30)  overlay(ds7cdp)
009900080319     d
010000080807     d DS7R          e ds
010100080826      // - Ricerca/Controllo tabelle
010200080826     d TIBS02ds      e ds                  inz
010300040614     D*
010400080319     d Dataiso         s               d   datfmt(*iso)
010500080826     d timeiso         s               t   timfmt(*iso)
010600080319     d Dataeur         s               d   datfmt(*eur)
010700080807     D W007A           S              7
010800080807     D $video          S              2    inz('D1')
010900080807     D $inzd01         S              1
011000080807     d $Fine           s               n   inz(*off)
011100080807     d Primavolta      s              1    inz
011200080807     D XX              S              3  0 inz
011300080807     D Indx            S              3  0 inz
011400120424     d Diff            s              4  0
011500120424     d gg_speD         s                   like(g02tgi)
011600120424     d gg_speA         s                   like(g02tgi)
011700080807     d
011800080807     d  xpardut        s             30
011900080807     d  xparkut        s              1  0
012000080807     d  xparrag        s             48
012100080807     d  xparkcc        s              4  0
012200080807     d  xparsta        s              1  0
012300080807     d  xparflr        s             90
012400080807     d  xpardit        s              3
012500080807     d  xparnum        s              2  0
012600080807     d  xparkcm        s             80
012700080807     d  xparksm        s            140
012800080807     d  xparkdm        s             60
012900080807     d  xparesci       s              1
013000080807     d  xparerr        s              2
013100080807     d  xpariva        s             16
013200080807     d  xparcdf        s             16
013300080807      //---------------------------------------------------------------
013400080807      //?Definizione aree dati.
013500080807      //---------------------------------------------------------------
013600080807
013700080807      // - Dati utente
013800080807     d §AzUte        e ds                  extname(AZUTE00F)
013900080807     d                                     dtaara
014000080807     d §DatiUte      e ds                  extname(dDatiUte)
014100080807     d                                     dtaara
014200080807      //---------------------------------------------------------------
014300080807      //?Definizione procedure usate.
014400080807      //---------------------------------------------------------------
014500080807      /copy gaitrasrc/srcprotopr,XALFA3BR
014600080807      /copy gaitrasrc/srcprotopr,tibs34r
014700080807      /copy gaitrasrc/srcprotopr,trtb70r
014800080826      /copy gaitrasrc/srcprotopr,tibs02r
014900080827      /copy gaitrasrc/srcprotopr,xsrda8
015000111228      /copy gaitrasrc/srcprotopr,xemail
015100000000     C*---------------------------------------------------------------*
015200911010     C* INDICATORI
015300911010     C*---------------------------------------------------------------*
015400951115     C*  28    - EMETTE CAMPO MESSAGGI
015500040607     C*  40    - ERRORE
015600911010     C*  90    - ERRORE GENERICO
015700911010     C*---------------------------------------------------------------*
015800000000     C     *ENTRY        PLIST
015900000000     C                   PARM                    KPJBA
016000080807
016100080807      /free
016200080807
016300080807       // Operazioni iniziali
016400080807       exsr RoutInz;
016500080807
016600080807       // Gestione video
016700080807       DOW $Fine = *off;
016800080807         select;
016900080807
017000080807         // Videata di selezioni
017100080807           when $Video = 'D1';
017200080807             exsr GesD01;
017300080807
017400080807           other   ;
017500080807             $Fine = *on;
017600080807         endsl;
017700080807       ENDDO;
017800080807
017900080807       // Operazioni finali
018000080807       exsr RoutEnd;
018100080807
018200080807       //--------------------------------------------------------------
018300080807       //?Operazioni iniziali.
018400080807       //--------------------------------------------------------------
018500080807       BEGSR RoutInz;
018600080807       $inzD01=*on;
018700080807
018800080807       // Solo la prima volta
018900080807 1     if primavolta=' '   ;
019000080807
019100080807         // Reperimento dati job
019200080807         exsr DatiJob;
019300080807
019400080807         // Carico tutte le particolarità consegne che prevedono
019500080807         //  la restituzione della BAM
019600080807
019700080807         clear XX   ;
019800080807         setll (1:'7R')  tabel00f    ;
019900080807         reade (1:'7R')  tabel00f    ;
020000080807
020100080807         dow not %eof(tabel00f)  ;
020200080807
020300080807         if tblflg=' '   ;
020400080807           ds7r=tbluni   ;
020500080807           if §7r1bf='B' ;
020600080807            xx=xx+1   ;
020700080807            SKI7r(xx)=tblkey   ;
020800080807           endif     ;
020900080807         endif           ;
021000080807
021100080807         reade (1:'7R')  tabel00f    ;
021200080807         enddo     ;
021300080807 1       endif    ;
021400080807
021500080807         ENDSR;
021600080807
021700080807       //--------------------------------------------------------------
021800080807       //?Reperimento Dati del job (Utente/Operativi).
021900080807       //--------------------------------------------------------------
022000080807       BEGSR DatiJob;
022100080807
022200080807         in(E) §AzUte;
022300080807         if NOT %error;
022400080807           in(E) §DatiUte;
022500080807         endif;
022600080807         if %error or RSut = *blanks;
022700080807           clear TIBS34ds;
022800080807           tibs34r(tibs34ds);
022900080807           in §AzUte;
023000080807           in §DatiUte;
023100080807         endif;
023200080807
023300080807       ENDSR;
023400080807       //--------------------------------------------------------------
023500080807       //?Gestione video 1
023600080807       //--------------------------------------------------------------
023700080807       BEGSR GesD01;
023800080807
023900080807         // Inizializzazione videata
024000080807         if  $Inzd01 = *on;
024100080807            exsr InzD01;
024200080807            $Inzd01  = *off;
024300080807         endif;
024400080807
024500080807         // Emissione videata
024600120405         exfmt  lsv4d01;
024700080807
024800080807         reset errMessage;
024900080807         reset errGenerico;
025000080807         clear V1cmsg;
025100080807         errksc=*off     ;
025200120402         errsped =*off     ;
025300120402         errspea =*off     ;
025400080827         errdsd=*off     ;
025500080827         errdsa=*off     ;
025600080902         errlnp=*off     ;
025700110617         erremail=*off   ;
025800120411         errinvrd=*off     ;
025900120411         errinvra=*off     ;
026000080807
026100080807 1       SELECT;
026200080807
026300080807         // - F3=Fine
026400080807 1         WHEN  dsp_aid = c_F03;
026500080807            $Fine = *on;
026600080807
026700080807 x1      // Invio
026800080807           OTHER;
026900080807             exsr ContrD01 ;
027000080807 2           if  errGenerico = *on;
027100080807               leavesr;
027200080807 2           endif;
027300080828
027400080828           // F6- Conferma
027500080828 1         if  dsp_aid = c_F06;
027600080828           exsr Conferma      ;
027700080828           $Inzd01 = *on;
027800080828           endif              ;
027900080807
028000080807 1       ENDSL;
028100080807
028200080807       ENDSR;
028300080807       //--------------------------------------------------------------
028400080807       //?controlli video1
028500080807       //--------------------------------------------------------------
028600080807       BEGSR ContrD01 ;
028700080807       select    ;
028800080807       when v1cksc=0 and v1dksc=*blanks               ;
028900080807           errksc=*on;
029000080807           v1cmsg = Msg(01);
029100080807           errMessage  = *on;
029200080807           errGenerico = *on;
029300080807           leavesr;
029400080807
029500080807       when v1cksc=0 and v1dksc<>*blanks   ;
029600080807       xparsta=9   ;
029700080807       xpardut=rsut   ;
029800080807       clear xparrag   ;
029900080807       xparrag  =v1dksc  ;
030000080807       xparkcc=dutkci    ;
030100080807       clear xparflr  ;
030200080807       clear xpardit  ;
030300080807       xparnum=1     ;
030400080807       clear xparkcm ;
030500080807       clear xparksm ;
030600080807       clear xparkdm ;
030700080807       clear xparesci;
030800080807       clear xparerr ;
030900080807       clear xpariva ;
031000080807       clear xparcdf ;
031100080807       xalfa3br (xpardut:xparkut:xparrag:xparkcc:xparsta:xparflr:xpardit
031200080807                 :xparnum:xparkcm:xparksm:xparkdm:xparesci:xparerr:xpariva
031300080807                 :xparcdf)             ;
031400080807       if xparsta<>-1  ;
031500080807       w007a=XPARKSM  ;
031600080807       V1CKSC=%int(W007A)  ;
031700080807       v1dksc=xparrag   ;
031800080807       else   ;
031900080807       clear v1dksc    ;
032000080807       endif    ;
032100080807       ErrGenerico=*on   ;
032200080807       leavesr   ;
032300080807
032400080807       other     ;
032500080807       chain   (1:dutkci:v1cksc)   cnaco000   ;
032600080807       if   not %found(cnaco00f) or acoflg<>' '    ;
032700080807           errksc=*on;
032800080807           v1cmsg = Msg(02);
032900080807           errMessage  = *on;
033000080807           errGenerico = *on;
033100080807           clear v1dksc    ;
033200080807           leavesr;
033300080807       endif   ;
033400080807
033500080807       v1dksc=acorag   ;
033600080807       ENDSL   ;
033700080807
033800080807       // Verifico se il cliente prevede la particolarità giusta
033900080807       clear ds7pqrs   ;
034000080807       ds7ksc=v1cksc   ;
034100080826       ds7ges='O'      ;
034200080807       ds7ric='S'      ;
034300080807       kpjbu=ds7pqrs   ;
034400080807       TRTB70R (KPJBA)   ;
034500080807       ds7pqrs=kpjbu     ;
034600080807
034700080807       xx=1   ;
034800080807       clear Indx    ;
034900080807       dow kscabi(xx)<>*blanks  and Indx=0;
035000080807       Indx=%lookup(kscabi(xx):SKI7R)  ;
035100080807       xx=xx+1  ;
035200080807       enddo    ;
035300080807
035400080807       if Indx=0   ;
035500080807           errksc=*on;
035600080807           v1cmsg = Msg(03);
035700080807           errMessage  = *on;
035800080807           errGenerico = *on;
035900080807           leavesr;
036000080807       endif       ;
036100080827
036200080902       // controllo la linea di partenza immessa
036300080902       clear v1dlnp    ;
036400080902       if v1clnp>0        ;
036500080902       chain  v1clnp   azorg01l   ;
036600080903        if not %found(azorg01l) or orgfva<>' ' or (orgfag<>'A' and
036700080903                                                   orgfag<>'F') ;
036800080902           errlnp=*on;
036900080902           v1cmsg = Msg(10);
037000080902           errMessage  = *on;
037100080902           errGenerico = *on;
037200080902           leavesr;
037300080902        endif   ;
037400080903        v1dlnp=orgdes    ;
037500080902       endif   ;
037600080902
037700080827       // immettere almeno una data
037800120402       if v1csped=0 and v1cspea =0  and v1cdsd=0 and
037900120411          v1cdsa=0  and v1cinvrd=0 and v1cinvra=0  ;
038000120402           errspeD= *on;
038100080827           v1cmsg = Msg(04);
038200080827           errMessage  = *on;
038300080827           errGenerico = *on;
038400080827           leavesr;
038500080827       endif          ;
038600080828
038700120411       // Data SPEDIZIONE DAL
038800120402       clear comsped    ;
038900120402       if v1cspeD >0      ;
039000120402         g02dat=v1cspeD   ;
039100080828         EXSR ContrDATA ;
039200080828
039300080828         if errMessage=*on ;
039400120402           errspeD =*on;
039500080828           leavesr;
039600080828          else               ;
039700120402          v1cspeD =g02dat      ;
039800120402          comspeD =g02inv      ;
039900120424          gg_speD =g02tgi      ;
040000080828          endif            ;
040100080828       endif            ;
040200080828
040300120402       // Data spedizione  AL
040400120402       clear comspeA    ;
0405001204241      if v1cspeA >0      ;
040600120402         g02dat=v1cspeA  ;
040700080828         EXSR ContrDATA ;
040800080828
0409001204242        if errMessage=*on ;
041000120402           errspeA= *on;
041100080828           leavesr;
041200120424x2        else               ;
041300120402          v1cspeA =g02dat      ;
041400120402          comspeA =g02inv      ;
041500120424          gg_speA =g02tgi      ;
041600120424 2        endif            ;
041700080828
041800080828           // Data AL no < data DAL
041900120424 2        if comspeA <comspeD     ;
042000120402           errspeA =*on;
042100080828           v1cmsg = Msg(07);
042200080828           errMessage  = *on;
042300080828           errGenerico = *on;
042400080828           leavesr;
042500120424 2         endif                   ;
042600120424
042700120424          // non si può elaborare più di un mese
042800120424          Diff=gg_spea - gg_sped  ;
042900120510 2        if Diff>35  ;
043000120424           errspeD =*on;
043100120424           v1cmsg = Msg(14);
043200120424           errMessage  = *on;
043300120424           errGenerico = *on;
043400120424           leavesr;
043500120424 2        endif  ;
043600120424 1        endif            ;
043700080828
043800120411       // Data CONSEGNA    DAL
043900080828       clear comdsd     ;
044000080828       if v1cdsd  >0      ;
044100080828         g02dat=v1cdsd    ;
044200080828         EXSR ContrDATA ;
044300080828
044400080828         if errMessage=*on ;
044500080828           errdsd  =*on;
044600080828           leavesr;
044700080828          else               ;
044800080828          v1cdsd  =g02dat      ;
044900080828          comdsd  =g02inv      ;
045000120424          gg_speD =g02tgi      ;
045100080828          endif            ;
045200080828       endif            ;
045300080828
045400120404       // Data consegna    AL
045500080828       clear comdsa     ;
045600080828       if v1cdsa>0      ;
045700080828         g02dat=v1cdsa    ;
045800080828         EXSR ContrDATA ;
045900080828
046000080828         if errMessage=*on ;
046100080828           errdsa  =*on;
046200080828           leavesr;
046300080828          else               ;
046400080828          v1cdsa  =g02dat      ;
046500080828          comdsa  =g02inv      ;
046600120424          gg_speA =g02tgi      ;
046700080828          endif            ;
046800080828
046900080828           // Data AL no < data DAL
047000080828          if comdsa  <comdsd      ;
047100080828           errdsa  =*on;
047200080828           v1cmsg = Msg(07);
047300080828           errMessage  = *on;
047400080828           errGenerico = *on;
047500080828           leavesr;
047600080828           endif                   ;
047700120424
047800120424          // non si può elaborare più di un mese
047900120424          Diff=gg_spea - gg_sped  ;
048000120510 2        if Diff>35  ;
048100120424           errdsd  =*on;
048200120424           v1cmsg = Msg(14);
048300120424           errMessage  = *on;
048400120424           errGenerico = *on;
048500120424           leavesr;
048600120424 2        endif  ;
048700080828       endif            ;
048800080828
048900120411       // Data INVENTARIO  DAL
049000120411       clear comINVRD   ;
049100120411       if v1cinvrd>0      ;
049200120411         g02dat=v1cinvrd  ;
049300120411         EXSR ContrDATA ;
049400120411
049500120411         if errMessage=*on ;
049600120411           errinvrd=*on;
049700120411           leavesr;
049800120411          else               ;
049900120411          v1cinvrd =g02dat      ;
050000120411          cominvrd =g02inv      ;
050100120424          gg_speD =g02tgi      ;
050200120411          endif            ;
050300120411       endif            ;
050400120411
050500120411       // Data inventario  AL
050600120411       clear cominvra   ;
050700120411       if v1cinvra>0      ;
050800120411         g02dat=v1cinvra  ;
050900120411         EXSR ContrDATA ;
051000120411
051100120411         if errMessage=*on ;
051200120411           errinvra=*on;
051300120411           leavesr;
051400120411          else               ;
051500120411          v1cinvra=g02dat      ;
051600120411          cominvra=g02inv      ;
051700120424          gg_speA =g02tgi      ;
051800120411          endif            ;
051900120411
052000120411           // Data AL no < data DAL
052100120411          if cominvra<cominvrd    ;
052200120411           errinvra=*on;
052300120411           v1cmsg = Msg(07);
052400120411           errMessage  = *on;
052500120411           errGenerico = *on;
052600120411           leavesr;
052700120411           endif                   ;
052800120424
052900120424          // non si può elaborare più di un mese
053000120424          Diff=gg_spea - gg_sped  ;
053100120510 2        if Diff>35  ;
053200120424           errinvrd =*on;
053300120424           v1cmsg = Msg(14);
053400120424           errMessage  = *on;
053500120424           errGenerico = *on;
053600120424           leavesr;
053700120424 2        endif  ;
053800120411       endif            ;
053900120411
054000120411        // se impostata data invetnario non posso chiedere le NON inventariate
054100120411        if (v1cinvrd>0  or v1cinvra>0) and v1cinve='E'    ;
054200120411           errinvrd=*on;
054300120411           v1cmsg = Msg(13);
054400120411           errMessage  = *on;
054500120411           errGenerico = *on;
054600120411           leavesr;
054700120411        endif;
054800120411
054900120411
055000110805        // Se richiesto le non invetariate per data spedizione, il cliente
055100110805        //  deve essere abilitato dalla tabella DCLI
055200110805        // if v1cdsd>0 or v1cdsa>0 ;
055300110805        //  clear TIBS02ds;
055400110805        //  T02sif = knsif;
055500110805        //  T02cod = 'CLI';
055600110805        //  T02ke1 = %editc(v1cksc:'X') ;
055700110805        //  T02mod = 'C'  ;
055800110805        //  TNTBE_RicercaControllo  (kpjba : tibs02ds);
055900080828
056000110805 2      //  if t02err=' '  ;
056100110805        //  dcli=t02uni    ;
056200110805        //  else ;
056300110805        //  clear DCLI    ;
056400110805 2      //  endif   ;
056500080828
056600110805         // if §cliidt=' '   ;
056700110805         //   errdsd  =*on;
056800110805         //  v1cmsg = Msg(09);
056900110805         //  errMessage  = *on;
057000110805         //  errGenerico = *on;
057100110805         //  leavesr;
057200110805         // endif            ;
057300080828
057400110805       // endif    ;
057500110617
057600120402       // immettere indirizzo e.mail e viceversa
057700110617       if (v1csfile=' ' and v1cemail<>*blanks)
057800110617          or (v1csfile<>' ' and v1cemail= *blanks)  ;
057900110617           erremail=*on;
058000110617           v1cmsg = Msg(12);
058100110617           errMessage  = *on;
058200110617           errGenerico = *on;
058300110617           leavesr;
058400110617       endif   ;
058500110613
058600111228       // controllo formale indirizzo email
058700111228       if v1cemail<>*blanks   ;
058800111228       clear dsemail  ;
058900120302       §EMLINDI=%trim(v1cemail)+'@brt.it'   ;
059000111228       callp XEMAIL   (dsemail)  ;
059100111228         if §EMLERRO<>*blanks and §EMLERRO<> '0';
059200111228             erremail=*on;
059300111228             v1cmsg = §emlmsgo   ;
059400111228             errMessage  = *on;
059500111228             errGenerico = *on;
059600111228             leavesr;
059700111228         endif   ;
059800111228       endif   ;
059900111228
060000080807       ENDSR   ;
060100080807       //--------------------------------------------------------------
060200080807       //?Inizializzazione video1
060300080807       //--------------------------------------------------------------
060400080807       BEGSR InzD01;
060500080807         clear v1cksc;
060600080807         clear v1dksc;
060700120402         clear v1cspeD ;
060800120402         clear v1cspeA ;
060900080827         clear v1cdsd  ;
061000080827         clear v1cdsa  ;
061100080903         clear v1clnp  ;
061200080903         clear v1dlnp  ;
061300120402         v1csfile='S'  ;
061400120410         v1cerra =' '  ;
061500120410         v1cinve =' '  ;
061600110617         clear v1cemail;
061700120411         clear v1cinvrd  ;
061800120411         clear v1cinvra  ;
061900080807       ENDSR  ;
062000080828       //--------------------------------------------------------------
062100080828       //?controllo della Data
062200080828       //--------------------------------------------------------------
062300080828         BEGSR  CONTRData  ;
062400080828
062500080828         clear g02err     ;
062600080828         callp xsrda8 (wlbdat)  ;
062700080828          if g02err='1'      ;
062800080828           v1cmsg = Msg(05);
062900080828           errMessage  = *on;
063000080828           errGenerico = *on;
063100080828           leavesr;
063200080828          endif            ;
063300080828
063400080828        // Non deve essere > di oggi
063500080828        if g02inv>*date       ;
063600080828           v1cmsg = Msg(06);
063700080828           errMessage  = *on;
063800080828           errGenerico = *on;
063900080828           leavesr;
064000080828        endif                 ;
064100080828        ENDSR   ;
064200080807       //--------------------------------------------------------------
064300080807       //?Operazioni finali.
064400080807       //--------------------------------------------------------------
064500080807       BEGSR RoutEnd;
064600080807
064700080807         *inLR = *on;
064800080807         return;
064900080807
065000080807       ENDSR;
065100080807
065200080828       //--------------------------------------------------------------
065300080828       //?conferma elaborazione
065400080828       //--------------------------------------------------------------
065500080807      /end-free
065600080828     c     Conferma      BEGSR
065700080901     c
065800080828     c                   clear                   kpjbu
065900120405     c                   movel     fnlsv4ds      kpjbu
066000080828     c                   movel     'LSV3'        Kcoaz
066100120411     C                   CALL      'BCH10'
066200120411     C*                  CALL      'FNLSV3R'
066300080828     C                   PARM                    KPJBA
066400080828     c
066500080828     c                   ENDSR
066600080807
066700000606**  MSG
066800080826Immettere il codice Cliente Mittente                                           1
066900080826Codice Cliente Mittente inesistente                                            2
067000080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
067100120411Immettere almeno una data
067200080827Data Errata
067300080827La data non deve essere maggiore di oggi
067400080828Data AL non può essere minore della data DAL
067500080828Impossibile effettuare contemporanemanete stampa e ristampa inventario
067600080828Per il cliente non è stata attivata la lista delle non inventariate:TEL al CED
067700110617Linea di partenza errata                                                      10
067800110617File richiedibile soltanto per le bolle non inventariate                      11
067900120405Immettere  indirizzo e-mail
068000120411Se immessa data inventario impossibile richiedere le non inventariate         13
068100120424Non è possibile elaborare più di un mese alla volta                           14
