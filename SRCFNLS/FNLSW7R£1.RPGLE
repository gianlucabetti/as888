000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200161031      * FNLSW7R *----------------------------------------------------------*
000300161031      *   Invio bolle KPI  ai clienti mittenti
000400080319      *--------------------------------------------------------------------*
000500111202     Fcnaco00f  IF   E           K DISK
000600161102     Ffnblp31l  IF   E           K DISK    prefix(arb:3)
000700161103     Ffnarb01l  IF   E           K DISK    prefix(A_)
000800161102     Ffnblt01l  IF   E           K DISK
000900161102     Ffnart01l  IF   E           K DISK
001000110913     Fazorg01l  IF   E           K DISK
001100161103     Ffnbrv07l  IF   E           K DISK    rename(fnbrv000:fnbrv007)
001200161103     Ffnbrv09l  IF   E           K DISK
001300080327     Ftabel00f  IF   E           K DISK
001400161207     Ffnevb01l  IF   E           K DISK
001500161212     Ffnlbl01l  IF   E           K DISK
001600161031     Fwfkpi01L  uF a E           K DISK
001700050705      *
001800161102     d*  linee partenza selezionate prese una sola volta
001900161102     D lnpSel          S              3  0 DIM(20)
002000161209     D evedt           S              8  0 DIM(11)
002100161209     D evedes          S              3    DIM(11)
002200161209     D evedim          S              8  0 DIM(11)
002300161209     D evehim          S              6  0 DIM(11)
002400161209     D cev             S              3    DIM(199)
002500161102     d
002600110913     d KPJBA         e ds
002700080328     d tibs34ds      e ds                  inz
002800080410     d tibs02ds      e ds                  inz
002900080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
003000080328     d DDatiUte      e ds                  dtaara(§datiute)
003100110916     d fnlv55ds      e ds
003200161103     d fnlv53ds      e ds
003300161209     d ds2a          e ds
003400161209     d ds2z          e ds
003500161207     D TNSD99DS      E DS                  INZ
003600080829     d
003700161031     D fnlsw7ds        DS
003800161031     D  COMdsd                 1      8  0
003900161031     D  kaad                   1      4  0
004000161031     D  kmgd                   5      8  0
004100161031     D  COMdsa                 9     16  0
004200161031     D  kaaa                   9     12  0
004300161031     D  kmga                  13     16  0
004400110914     D  LNP                   17     56P 0
004500110914     D                                     DIM(20)
004600110914     D  CLI                   57    136P 0
004700110914     D                                     DIM(20)
004800110914     D  NRS                  137    176
004900110914     D                                     DIM(20)
005000110914     D  videmail             177    201
005100161207     d
005200161207     D WDAT8           DS
005300161207     D  DATADA                 1      8  0
005400161207     D  DATAA                  9     16  0
005500161207     D  GGL                   17     21  0
005600161207     d
005700161207     D tfptfa          DS
005800161207     D  p_tfp                  1      3  0
005900161207     D  p_tfa                  4      6  0
006000161207     d
006100161207     D                 DS
006200161207     D  arbaas                 1      4  0
006300161207     D  arbmgs                 5      8  0
006400161207     D  arbdsp                 1      8  0
006500110914     d
006600110914     d
006700110617     D  pInFILE        s             10
006800110617     D  pInIDJOB       s             26
006900110617     D  pInPWDXLS      s             15
007000110617     D  pInNOMXLS      s             78
007100111206     D  pInEMLVAR      s             80
007200110617     D  pInEMLDEST     s            121
007300120504     D  pInMRAKE2      s             15    inz
007400110617     D  pOutESITO      s              1
007500161102     d XX              s              3  0
007600110913     d yy              s              3  0
007700110916     d ee              s              3  0
007800110916     d zz              s              3  0
007900161102     d wccm            s              7  0
008000161102     d dateu           s              8  0
008100161102     d wdse            s              8  0
008200161102     d ora_u           s              6  0
008300161102     d alfanrs         s              2
008400161102     d ex88            s              1
008500161102     d wok             s              1
008600161102     d wfine           s              1
008700161102     d Indx            s              3  0
008800161102     d oggcli          s              7  0
008900161103     d IMPdata         s                   like(d53dfv)
009000161103     d IMPfgs          s                   like(brvfgs)
009100161103     d DISfgs          s                   like(brvfgs)
009200161103     d DISdata         s                   like(d53dfv)
009300161103     d DISnpg          s                   like(brvnpg)
009400161103     d DISora          s                   like(brvhcs)
009500161103     d IDdata          s                   like(d53dfv)
009600161103     d IDora           s                   like(brvhcs)
009700161103     d IDfgs           s                   like(brvfgs)
009800161103     d IDnpg           s              1
009900161209     d wnic            s              1
010000110613     d wlib            s             10    inz('GAITRAGRPS')
010100110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
010200110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
010300110613     d Ora_iso         s               t   timfmt(*iso)
010400110613     d Dataiso         s               d   datfmt(*iso)
010500111013     d Dataeur         s               d   datfmt(*eur)
010600080327     d Datadmy         s               d   datfmt(*dmy)
010700110916
010800110916     d $MaxElem        c                   const(998)
010900080829     d
011000080317      *------------------------------------------------------------------------*
011100080317      *   P R O T O T I P I
011200080317      *------------------------------------------------------------------------*
011300080829      /copy gaitrasrc/srcprotopr,tibs34r
011400110617      /copy gaitrasrc/srcprotopr,trtcm8r1
011500110916      /copy gaitrasrc/srcprotopr,fnlv55r
011600001027     C*---------------------------------------------------------------*
011700001027     C     *ENTRY        PLIST
011800080325     C                   PARM                    kpjba
011900161031     c                   movel     kpjbu         fnlsw7ds
012000080317      /free
0121001610311      if comdsa=0 ;
012200161031        comdsa=comdsd ;
0123001610311      endif  ;
012400161102
012500161102       // Linee partenza selezionate
012600161102          EXSR  PrepaLNP      ;
012700161031
0128001610311      if comdsa=comdsd ;
012900161031         *in01 =*on  ;
0130001610311       endif  ;
013100161102
013200161209       zz=1   ;
013300161031
0134001611021      dow lnpsel(zz)>0  ;
013500161102          setll (lnpsel(zz):kaad:kmgd) fnblp31l ;
013600161031          if *in01=*on  ;
013700161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
013800161031          else  ;
013900161102          reade (lnpsel(zz)) fnblp31l ;
014000161031          endif  ;
014100161031
0142001611022         dow not %eof(fnblp31l)   ;
014300161102
014400161102          exsr   Ctrsel    ;
014500161102
014600161102          if Wfine='S'  ;
014700161102          leave  ;
014800161102          endif  ;
014900161102
015000161102          if wok='S'   ;
015100161102          exsr elabora  ;
015200161102          endif  ;
015300161102
015400161102
015500161102          if *in01=*on  ;
015600161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
015700161102          else  ;
015800161102          reade (lnpsel(zz)) fnblp31l ;
015900161102          endif  ;
0160001611022         enddo  ;
016100161031
016200161102          zz=zz+1  ;
016300161031
0164001611021      enddo ;
016500111116
016600110617        // Se scritto almeno un record, invio all'indirizzo e-mail
016700161207        //pInFILE='WFKPI00F'   ;
016800161207        //pInIDJOB=knmeb       ;
016900161207          // Nell'oggetto imposto il primo codice cliente richiesto
017000161207  2     //if  oggcli>0   ;
017100161207        //chain (1:dutkci:oggcli) cnaco00f  ;
017200161207  3     //if not %found(cnaco00f)  ;
017300161207        //  acorag=*all'?'  ;
017400161207  3     //endif  ;
017500111202
017600161207        //  pinEMLVAR='*OBJM* Anomalie Cliente: '+%editc(oggcli:'X')+ '-'+
017700161207        //             %subst(acorag:1:15)  ;
017800161207  x2    //else  ;
017900111202          // Imposto la prima lnp/serie richiesta   ;
018000161207        // pinEMLVAR='*OBJM* Anomalie Lnp/Ser:'+%editc(lnp(1):'X')+' '+
018100161207        //                               nrs(1)      ;
018200161207  2     //endif  ;
018300111206
018400161207        //  pInEMLDEST=%trim(videmail)+'@brt.it' ;
018500161102        //callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
018600161102        //                pInEMLVAR:pInMRAKE2:pOutESITO) ;
018700080829
018800080317        *inlr ='1'   ;
018900080828
019000110913       //-------------------------------------------------------------------
019100161102        BEGSR   Ctrsel  ;
019200161102
019300161102        Wok='N'  ;
019400161102        Wfine=' '  ;
019500161209        Wnic =' '  ;
019600161212
019700161212        // Escludo le bolle con CTM 4 e 4F
019800161212        if blpctm ='4 ' or blpctm='4F' ;
019900161212          WOK='N'  ;
020000161212          leavesr  ;
020100161212        endif  ;
020200161212
020300161212        // escludo le bolle "figlie"
020400161212        chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl01l  ;
020500161212        if %found(fnlbl01l)  ;
020600161212          WOK='N'  ;
020700161212          leavesr  ;
020800161212        endif  ;
020900161102
0210001611021       if (arbaas*10000)+arbmgs>comdsa ;
021100161102        Wfine='S'  ;
021200161102        leavesr ;
0213001611021       endif  ;
021400161102
021500161102        // controllo cliente e serie
0216001611021       if arbccm>0  ;
021700161102         wccm= arbccm  ;
021800161102        else  ;
021900161102         wccm=arbksc  ;
0220001611021       endif  ;
022100161102
022200161102        xx= 1 ;
022300161102
022400161102 3      dow xx<= 20  ;
022500161102 4      if lnp(xx) >0  ;
022600161102
022700161102 5      if cli(XX)=0 or wccm= cli(xx)  ;
022800161102
022900161102        if   nrs(xx)='99'   ;
023000161102         WOK='S'  ;
023100161102          leavesr  ;
023200161102        endif   ;
023300161102
023400161102 6      if   nrs(XX)='SI' and arbnrs>0    ;
023500161102         WOK='S'  ;
023600161102          leavesr  ;
023700161102 x6     else  ;
023800161102
023900161102        alfanrs=%editc(arbnrs:'X')  ;
024000161102 7       if alfanrs= nrs(xx)  ;
024100161102         WOK='S'  ;
024200161102          leavesr  ;
024300161102 7       endif  ;
024400161102 6      endif  ;
024500161102
024600161102 5      endif  ;
024700161102 4      endif  ;
024800161102
024900161102        xx=xx+1  ;
025000161102 3      enddo  ;
025100161102
025200161102
025300161102
025400161102        ENDSR  ;
025500161102       //-------------------------------------------------------------------
025600161102       BEGSR  Elabora                ;
025700161207
025800161209       ex88=' '   ;
025900161209       clear wdse;
026000161209       clear                   DISfgs     ;
026100161209       clear                   DISdata    ;
026200161209       clear                   DISnpg     ;
026300161209       clear                   DISora     ;
026400161209       clear                   IMPdata    ;
026500161209       clear                   IMPfgs     ;
026600161209       clear                   IDnpg      ;
026700161209       clear                   IDdata     ;
026800161209       clear                   IDora      ;
026900161209       clear                   IDfgs      ;
027000161209       clear evedt    ;
027100161209       clear evedes   ;
027200161209       clear evedim   ;
027300161209       clear evehim   ;
027400161209
027500161207       clear wfkpi000  ;
027600161207
027700161209       // calcolo ore reali di consegna tra data sped e data consegna
027800161207       exsr cal_gg    ;
027900161207
028000161207       if kpidly_c > 0    ;
028100161102
028200161102       setll (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
028300161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0284001611021      dow  not %eof(fnblt01l)  ;
028500161102
028600161102       // verifico se presente una spunta 88
0287001612092      if bltdse>arbdbr or bltdse=0 or bltnpp=88 ;
028800161102       setll  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
028900161102       reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
0290001611023      dow not %eof(fnbrv09l)  ;
0291001611024       if brvnps=88 ;
029200161102         ex88='S'   ;
029300161102         wdse=bltdse;
029400161102         leave  ;
0295001611024       endif  ;
0296001611024       if brvnps=90 ;
029700161102         chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0298001611025        if artdam>bltdse  ;
029900161102          ex88='S'   ;
030000161102          wdse=bltdse;
030100161102          leave  ;
0302001611025        endif  ;
0303001611024       endif  ;
030400161103
030500161102
0306001611023      reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
030700161102       enddo  ;
0308001611022      endif   ;
030900161209
031000161209       // Verifico se in IMP/Disguido
031100161209       exsr IMPDISG  ;
031200161102
031300161102
031400161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0315001611021      enddo  ;
031600161102
031700161209       // verifico se la trazione è ritardo , usando la data arrivo ult collo
031800161103       if %found(fnarb01l)   ;
031900161103       endif  ;
032000161209
032100161209       // Lettura eventi
032200161209       exsr leggi_eve  ;
032300161207       endif  ;
032400161103
032500161102       exsr   Scrivifile  ;
032600161102       ENDSR   ;
032700161209       //-------------------------------------------------------------------
032800161209       BEGSR  Leggi_eve              ;
032900161209
033000161209       // Lettura eventi che "servono"
033100161209       clear YY  ;
033200161209
033300161209       setll  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
033400161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
033500161209  1    dow  not %eof(fnevb01l)   ;
033600161209
033700161209  2    if %lookup(evbcev:cev)>0   ;
033800161209
033900161209       yy=yy+1  ;
034000161209  3    if yy<11   ;
034100161209
034200161209        evedt(yy)=evbdev   ;
034300161209        evedes(yy)=evbcev   ;
034400161209        evedim(yy)=evbdtv   ;
034500161209        evehim(yy)=evborv   ;
034600161209
034700161209       // e si tratta di NIC memorizzo anche data e ora immissione
034800161209       // lo elimino se trovo un MIC successivo
034900161209  4     if evbcev='NIC' ;
035000161209         eval wnic='S'   ;
035100161209  4     endif  ;
035200161209  3     endif  ;
035300161209
035400161209  x2    else   ;
035500161209
035600161209        // solo se presente NIC verifico se da eliminare
035700161209   3      if wnic='S' and evbcev='MIC'    ;
035800161209        clear wnic  ;
035900161209
036000161209        yy=1   ;
036100161209   4    dow yy<=10  ;
036200161209   5    if evedes(yy)='NIC' ;
036300161209
036400161209   6    select  ;
036500161209        when evedt(yy)<>evbdev ;
036600161209        wnic='S'   ;
036700161209
036800161209        when evedim(yy)<evbdtv ;
036900161209        clear evedt(yy)   ;
037000161209        clear evedes(yy)   ;
037100161209        clear evedim(yy)   ;
037200161209        clear evehim(yy)   ;
037300161209
037400161209        when evedim(yy)=evbdtv and evehim(yy)<evborv ;
037500161209        clear evedt(yy)   ;
037600161209        clear evedes(yy)   ;
037700161209        clear evedim(yy)   ;
037800161209        clear evehim(yy)   ;
037900161209        other   ;
038000161209        wnic='S'   ;
038100161209        endsl  ;
038200161209
038300161209   5    endif   ;
038400161209
038500161209        yy=yy+1  ;
038600161209   4    enddo  ;
038700161209
038800161209   3      endif  ;
038900161209   2    endif  ;
039000161209
039100161209
039200161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
039300161209   1   enddo  ;
039400161209
039500161209       ENDSR   ;
039600161102       //-------------------------------------------------------------------
039700161102       BEGSR  Scrivifile             ;
039800111116
039900110913
040000161102       kpiidute=knmeb   ;
040100161102       kpiepo=dutpou  ;
040200110913
040300161102       kpiccm=wccm    ;
040400161102       kpirsm=arbrsm  ;
040500161102
040600161102       kpiaas=arbaas  ;
040700161102       kpilnp=arblnp  ;
040800161102       kpimes=%int(%subst(%editc(arbmgs:'X'):1:2))  ;
040900161102       kpirmn=arbrmn  ;
041000161102       kpirma=arbrma  ;
041100161102       kpinrs=arbnrs  ;
041200161102       kpinsp=arbnsp  ;
041300161102       kpimgs=arbmgs  ;
041400161102       kpilna=arblna  ;
041500161102       kpirsd=arbrsd  ;
041600161102       kpilod=arblod  ;
041700161102       kpiprd=arbprd  ;
041800161102       kpicad=arbcad  ;
041900161102       kpinzd=arbnzd  ;
042000161102       kpiffd=arbffd   ;
042100161102       kpitc1=arbtc1   ;
042200161102       kpitc2=arbtc2   ;
042300161102       kpigc1=arbgc1   ;
042400161102       kpigc2=arbgc2   ;
042500161102       kpincl=arbncl   ;
042600161102       kpipkb=arbpkb   ;
042700161102       kpivlb=arbvlb   ;
042800161102       kpitcr=arbtcr   ;
042900161102       kpidcr=arbdcr   ;
043000161102       kpidcp=arbdcp   ;
043100161102       kpidcm=arbdcm   ;
043200161102       kpihmc=arbhmc   ;
043300161209       kpicca=arbcca   ;
043400161209
043500161209       if arbdcm=0 and a_arbdcm>0  ;
043600161209       kpidcm=a_arbdcm   ;
043700161209       kpihmc=a_arbhmc   ;
043800161209       kpicca=a_arbcca   ;
043900161209       endif  ;
044000161209
044100161209       if kpicca<>' '  ;
044200161209       tblkey=kpicca   ;
044300161209       chain (1:'7O':tblkey) tabel  ;
044400161209         if %found(tabel00f )  ;
044500161209          kpicca_des=tbluni   ;
044600161209         endif  ;
044700161209       endif  ;
044800161103
044900161103        // pistola ex 88
045000161102        if ex88='S'     ;
045100161102        kpip88='S'      ;
045200161103        kpidse=wdse     ;
045300161102        endif           ;
045400161103
045500161103        //IMP disguido
045600161103        if IDnpg<>' '   ;
045700161103           if IDnpg='P'   ;
045800161103           kpiimpds='P'   ;
045900161103           else  ;
046000161103           kpiimpds='D'   ;
046100161103           endif ;
046200161103        endif ;
046300161209
046400161209       yy=1  ;
046500161209
046600161209       dow yy<=10                ;
046700161209       if  evedes(yy)<>*blanks   ;
046800161209
046900161209        select   ;
047000161209         when KPICEV1=*blanks  ;
047100161209         KPICEV1=evedes(yy)  ;
047200161209         KPIDEV1=evedt(yy)  ;
047300161209         when KPICEV2=*blanks  ;
047400161209         KPICEV2=evedes(yy)  ;
047500161209         KPIDEV2=evedt(yy)  ;
047600161209         when KPICEV3=*blanks  ;
047700161209         KPICEV3=evedes(yy)  ;
047800161209         KPIDEV3=evedt(yy)  ;
047900161209         when KPICEV4=*blanks  ;
048000161209         KPICEV4=evedes(yy)  ;
048100161209         KPIDEV4=evedt(yy)  ;
048200161209         when KPICEV5=*blanks  ;
048300161209         KPICEV5=evedes(yy)  ;
048400161209         KPIDEV5=evedt(yy)  ;
048500161209         when KPICEV6=*blanks  ;
048600161209         KPICEV6=evedes(yy)  ;
048700161209         KPIDEV6=evedt(yy)  ;
048800161209         when KPICEV7=*blanks  ;
048900161209         KPICEV7=evedes(yy)  ;
049000161209         KPIDEV7=evedt(yy)  ;
049100161209         when KPICEV8=*blanks  ;
049200161209         KPICEV8=evedes(yy)  ;
049300161209         KPIDEV8=evedt(yy)  ;
049400161209         when KPICEV9=*blanks  ;
049500161209         KPICEV9=evedes(yy)  ;
049600161209         KPIDEV9=evedt(yy)  ;
049700161209         when KPICEV0=*blanks  ;
049800161209         KPICEV0=evedes(yy)  ;
049900161209         KPIDEV0=evedt(yy)  ;
050000161209         endsl  ;
050100161209         endif  ;
050200161209
050300161209         yy=yy+ 1;
050400161209
050500161209       enddo  ;
050600110916
050700161102       write  wfkpi000 ;
050800110913
050900110926
051000110913       ENDSR                  ;
051100080327       //-------------------------------------------------------------------
051200080327       BEGSR  *INZSR                 ;
051300080328
051400080328       in(E) *dtaara   ;
051500080328       if    %error or rsut  =*blanks  ;
051600080328       callp TIBS34R (TIBS34DS)        ;
051700080328       in    *dtaara   ;
051800080328       endif                           ;
051900080829
052000110613       // imposto data e ora del giorno
052100110613       dataiso=datasys  ;
052200110613       dateu=%dec(dataiso)   ;
052300110616
052400110613       ora_iso=ora_sys  ;
052500110613       ora_u=%dec(ora_iso)   ;
052600080829
052700110913       // Se richiesto file pulisco il lancio precedente per filiale
052800161102       setll (dutpou)   wfKPI01l   ;
052900161102       reade (dutpou)   wfKPI01l   ;
053000110616
053100161102       dow  not %eof(wfKPI01l)   ;
053200161102       delete wfKPI000  ;
053300110616
053400161102       reade (dutpou)   wfKPI01l   ;
053500110616       enddo  ;
053600110616
053700161209       clear xx ;
053800161209       setll  (1:'2A') tabel00f ;
053900161209       reade  (1:'2A') tabel00f ;
054000161209
054100161209       dow not %eof(tabel00f ) ;
054200161209       if tblflg=' '  ;
054300161209        ds2a = tbluni   ;
054400161209
054500161209       if   §2aftg='S' or §2atip='N' or §2aftc='A'  ;
054600161209       xx=xx+1  ;
054700161209        cev(xx)=%Subst(tblkey:1:3)  ;
054800161209       endif  ;
054900161209       endif  ;
055000161209
055100161209       reade  (1:'2A') tabel00f ;
055200161209       enddo  ;
055300161209
055400161209       setll  (1:'2Z') tabel00f ;
055500161209       reade  (1:'2Z') tabel00f ;
055600161209
055700161209       dow not %eof(tabel00f ) ;
055800161209       if tblflg=' '  ;
055900161209        ds2z = tbluni   ;
056000161209
056100161209       if   §2zcep<>' '  ;
056200161209       xx=xx+1  ;
056300161209        cev(xx)=%subst(tblkey:1:3)  ;
056400161209       endif  ;
056500161209       endif  ;
056600161209
056700161209       reade  (1:'2Z') tabel00f ;
056800161209       enddo  ;
056900161209
057000161209       // infine aggiungo fisso il NIC
057100161209       xx=xx+1  ;
057200161209       cev(xx)='NIC' ;
057300161209
057400161209       ENDSR     ;
057500161102       //-------------------------------------------------------------------
057600161209       BEGSR  PrepaLNP               ;
057700161102
057800161102       clear  LnpSel  ;
057900161102       xx= 1 ;
058000161102       yy= 1 ;
058100161102
058200161102       dow xx <=20  ;
058300161102        if lnp(xx)>0  ;
058400161102         Indx=%lookup(lnp(xx):LnpSel)  ;
058500161102         if Indx=0  ;
058600161102           eval lnpSel(yy)=lnp(xx)  ;
058700161102           yy=yy+1   ;
058800161102         endif  ;
058900161102
059000161102        endif  ;
059100161102
059200161102        xx=xx+1   ;
059300161102        enddo     ;
059400161102
059500161102        // Memorizzo nel campo oggcli il codice cliente che devo
059600161102        //  mettere nell'ogggetto della mail
059700161102        clear oggcli  ;
059800161102        if cli(1)>0   ;
059900161102          oggcli=cli(1)  ;
060000161102        endif  ;
060100161102
060200161102       ENDSR  ;
060300161207
060400161103
060500161103      /end-free
060600161103
060700161207     c*-------------------------------------------------------------------
060800161207     c     Cal_gg        BEGSR
060900161209     c**                 clear                   wdat8
061000161209     c**                 clear                   tfptfa
061100161209     c**                 z-add     arbdsp        datada
061200161209     c**                 if        arbdcm>0
061300161209     c**                 movel     arbdcm        dataa
061400161209     c**                 else
061500161209     c**                 movel     datasys       dataa
061600161209     c**                 endif
061700161209     c**                 z-add     arbtfp        p_tfp
061800161209     c**                 z-add     arbtfa        p_tfa
061900161209     c**                 call      'XSRLAV8'
062000161209     c**                 parm                    wdat8
062100161209     c**                 parm                    tfptfa
062200161207     c
062300161209     c     karb          klist
062400161209     c                   kfld                    arbaas
062500161209     c                   kfld                    arblnp
062600161209     c                   kfld                    arbnrs
062700161209     c                   kfld                    arbnsp
062800161209
062900161209     c     karb          chain     fnarb01l
063000161207     c                   clear                   tnsd99ds
063100161207     C                   MOVEL     ' '           D98TLA
063200161209     c                   if        %found(fnarb01l)
063300161207     C                   MOVEL     'A'           D98TBO
063400161209     c                   else
063500161209     C                   MOVEL     'P'           D98TBO
063600161209     c                   endif
063700161207     C                   MOVEL     ARBAAS        D98AAS
063800161207     C                   MOVEL     ARBLNP        D98LNP
063900161207     C                   MOVEL     ARBNRS        D98NRS
064000161207     C                   MOVEL     ARBNSP        D98NSP
064100161207     c
064200161207     c                   if        arbtsp='D'
064300161207     c                   eval      I98TSP_FOR='C'
064400161207     c                   endif
064500161207     C*
064600161207     C                   CALL      'TNSD99R'
064700161207     C                   PARM                    TNSD99DS
064800161207     c
064900161207     c* ore teoriche
065000161209     c                   eval      KPIdly_T=d98trc
065100161209     c                   eval      KPIdag  =d98dag
065200161209     c                   eval      KPIdcg  =d98dcg
065300161207
065400161207     c* Richiamo il pgm utilizzando la DS eliminando il num sped
065500161207     c*  e la giacenza
065600161207     c                   clear                   d98nsp
065700161207     c                   clear                   d98dti
065800161207     c                   clear                   d98hti
065900161207     c                   clear                   d98cmc
066000161207     c                   clear                   d98dag
066100161207     c                   clear                   d98dcg
066200161207     c                   eval      d98duc=d98dsp
066300161207     c                   eval      d98dbr=d98dsp
066400161207     c                   clear                   d98dam
066500161207     C                   CALL      'TNSD99R'
066600161207     C                   PARM                    TNSD99DS
066700161207     c
066800161207     c* controllo le ore reali  con le ore teoriche
066900161209     c                   eval      KPIdly_R=d98rrc
067000161207     c                   if        kpidly_r<=kpidly_t
067100161207     c                   clear                   kpidly_c
067200161207     c                   else
067300161207     c
067400161207     c                   eval      kpidly_c =kpidly_r-kpidly_t
067500161207     c                   endif
067600161207     c*
067700161212     c* Se data richiesta = data consegna merce
067800161209     c*  sempre OK
067900161212     c**                 if        arbtc1='A' or arbtc2='A'  or
068000161212     c**                           arbtc1='S' or arbtc2='S'
068100161209     c                   if        arbdcr>0 and arbtcr=' ' and
068200161209     c                             arbdcr=arbdcm
068300161209     c                   clear                   kpidly_c
068400161209     c                   endif
068500161212     c**                 endif
068600161207
068700161212     c* se cons anomala RESO o DIROTTAMENTO --> sempre ok
068800161212     c                   if        arbcca='1' or arbcca='2' or
068900161212     c                             arbcca='6'
069000161212     c                   clear                   kpidly_c
069100161212     c                   endif
069200161207     c*
069300161207     c                   ENDSR
069400161103     c*-------------------------------------------------------------------
069500161103     c     IMPdisg       BEGSR
069600161209     c
069700161209     c     kbrv          Klist
069800161209     c                   Kfld                    bltfls
069900161209     c                   Kfld                    bltlna
070000161209     c                   Kfld                    bltnrs
070100161209     c                   Kfld                    bltnsc
070200161103     c
070300161103     c* per ogni collo verifico se in IMP o disguidato
070400161103     c* prendo la data più alta
070500161103     c     kbrv          setll     fnbrv07l
070600161103     c     kbrv          reade     fnbrv07l
070700161103    2c                   dow       not %eof(fnbrv07l)
070800161103    3c                   if        brvatr='D'  or brvnpg=3
070900161103     c* prendo la data della spunta
071000161103     c                   clear                   fnlv53ds
071100161103     C                   MOVEL     BRvNPG        D53NPG
071200161103     C                   Z-ADD     BRvNFV        D53NFV
071300161103     C                   MOVEL     BRvFGS        D53FGS
071400161103     C                   CALL      'FNLV53R'
071500161103     C                   PARM                    fnlv53ds
071600161103    4c                   if        d53err<>*blanks
071700161103    5c                   if        brvdcs>0
071800161103     c                   eval      d53dfv=brvdcs
071900161103     c                   else
072000161103    6c                   if        brvdfs>0
072100161103     c                   eval      d53dfv=brvdfs
072200161103    6c                   endif
072300161103    5c                   endif
072400161103    4c                   endif
072500161103     c
072600161103    4c                   select
072700161103     c* se si tratta di DISGUIDO
072800161103     c                   when      brvatr='D'
072900161103     c
073000161103    5c                   if        d53dfv=DISdata
073100161103     c* Verifico l'ora
073200161103    6c                   if        brvhcs>0 and brvhcs>DISora
073300161103     c                   eval      DISdata=d53dfv
073400161103     c                   eval      DISfgs =brvfgs
073500161103     c                   eval      DISnpg =brvnpg
073600161103     c                   eval      DISora =brvhcs
073700161103   x6c                   else
073800161103    7c                   if        brvhms>0 and brvhms>DISora
073900161103     c                   eval      DISdata=d53dfv
074000161103     c                   eval      DISfgs =brvfgs
074100161103     c                   eval      DISnpg =brvnpg
074200161103     c                   eval      DISora =brvhms
074300161103    7c                   endif
074400161103    6c                   endif
074500161103    5c                   endif
074600161103     c
074700161103    5c                   if        d53dfv>DISdata
074800161103     c                   eval      DISdata=d53dfv
074900161103     c                   eval      DISfgs =brvfgs
075000161103     c                   eval      DISnpg =brvnpg
075100161103     c                   eval      DISora =brvhcs
075200161103     c                   if        disora=0
075300161103     c                   eval      DISora =brvhms
075400161103     c                   endif
075500161103    5c                   endif
075600161103     c
075700161103     c* se si tratta di IMP
075800161103    4c                   when      d53spg='P' and d53dfv>impdata
075900161103     c                   eval      IMPdata=d53dfv
076000161103     c                   eval      IMPfgs =brvfgs
076100161103    4c                   endsl
076200161103    3c                   endif
076300161103
076400161103     c     kbrv          reade     fnbrv07l
076500161103    2c                   enddo
076600161103
076700161103     c* memorizzo la data più alta
076800161103     c* IMP vince sul disguido per stessa data, senza guardare l'ora
076900161103    2c                   if        IMPdata>0 and IMPdata>=IDdata
077000161103     c                   eval      IDdata=IMPdata
077100161103     c                   eval      IDfgs=IMPfgs
077200161103     c                   eval      IDora=0
077300161103     c                   eval      IDnpg='P'
077400161103    2c                   endif
077500161103    2c                   if        (DISdata>0 and DISdata=IDdata
077600161103     c                             and IDnpg<>'P' and DISora>IDora) or
077700161103     c                             (DISdata>0 and DISdata>IDdata)
077800161103     c                   eval      IDdata=DISdata
077900161103     c                   eval      IDfgs=DISfgs
078000161103     c                   eval      IDora=DISora
078100161103     c                   eval      IDnpg=%char(DISnpg)
078200161103    2c                   endif
078300161103     c
078400161103     C                   ENDSR
