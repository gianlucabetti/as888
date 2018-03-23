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
001600161212     Ffiabk01l  IF   E           K DISK
001700161212     Ffiabk02l  IF   E           K DISK    rename(fiabk000:fiabk002)
001800161031     Fwfkpi01L  uF a E           K DISK
001900161214     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
002000161214
002100050705      *
002200161102     d*  linee partenza selezionate prese una sola volta
002300161214     D CMD             S              1    DIM(61) CTDATA PERRCD(61)
002400161214     d
002500161102     D lnpSel          S              3  0 DIM(20)
002600161227     D evedt           S              8  0 DIM(32)
002700161227     D evedes          S              3    DIM(32)
002800161227     D evedim          S              8  0 DIM(32)
002900161227     D evehim          S              6  0 DIM(32)
003000161209     D cev             S              3    DIM(199)
003100161102     d
003200110913     d KPJBA         e ds
003300080328     d tibs34ds      e ds                  inz
003400080410     d tibs02ds      e ds                  inz
003500080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
003600080328     d DDatiUte      e ds                  dtaara(§datiute)
003700110916     d fnlv55ds      e ds
003800161103     d fnlv53ds      e ds
003900161209     d ds2a          e ds
004000161209     d ds2z          e ds
004100161207     D TNSD99DS      E DS                  INZ
004200080829     d
004300161212     D fnlsw7ds        DS
004400161031     D  COMdsd                 1      8  0
004500161031     D  kaad                   1      4  0
004600161031     D  kmgd                   5      8  0
004700161031     D  COMdsa                 9     16  0
004800161031     D  kaaa                   9     12  0
004900161031     D  kmga                  13     16  0
005000110914     D  LNP                   17     56P 0
005100110914     D                                     DIM(20)
005200110914     D  CLI                   57    136P 0
005300110914     D                                     DIM(20)
005400110914     D  NRS                  137    176
005500110914     D                                     DIM(20)
005600110914     D  videmail             177    201
005700161212     D  parges               202    202
005800161212     D  vidok                203    203
005900161212     D  vidokeve             204    204
006000161216     D  vidcons              205    205
006100161207     d
006200161207     D WDAT8           DS
006300161207     D  DATADA                 1      8  0
006400161207     D  DATAA                  9     16  0
006500161207     D  GGL                   17     21  0
006600161207     d
006700161207     D tfptfa          DS
006800161207     D  p_tfp                  1      3  0
006900161207     D  p_tfa                  4      6  0
007000161207     d
007100161207     D                 DS
007200161207     D  arbaas                 1      4  0
007300161207     D  arbmgs                 5      8  0
007400161207     D  arbdsp                 1      8  0
007500110914     d
007600110914     d
007700110617     D  pInFILE        s             10
007800110617     D  pInIDJOB       s             26
007900110617     D  pInPWDXLS      s             15
008000110617     D  pInNOMXLS      s             78
008100111206     D  pInEMLVAR      s             80
008200110617     D  pInEMLDEST     s            121
008300120504     D  pInMRAKE2      s             15    inz
008400110617     D  pOutESITO      s              1
008500161102     d XX              s              3  0
008600110913     d yy              s              3  0
008700110916     d ee              s              3  0
008800110916     d zz              s              3  0
008900161102     d wccm            s              7  0
009000161102     d dateu           s              8  0
009100161214     d dataogg         s              8  0
009200161102     d wdse            s              8  0
009300161214     d memdse          s              8  0
009400161214     d CONTAB          s              9  0 INZ
009500161102     d ora_u           s              6  0
009600161227     d conta           s              3  0
009700161227     d evemem          s              3  0
009800161227     d evesalta        s              3  0
009900161102     d alfanrs         s              2
010000161102     d ex88            s              1
010100161102     d wok             s              1
010200161212     d wabi            s              1
010300161102     d wfine           s              1
010400161216     d wesARB          s              1
010500161102     d Indx            s              3  0
010600161102     d oggcli          s              7  0
010700161103     d IMPdata         s                   like(d53dfv)
010800161103     d IMPfgs          s                   like(brvfgs)
010900161103     d DISfgs          s                   like(brvfgs)
011000161103     d DISdata         s                   like(d53dfv)
011100161103     d DISnpg          s                   like(brvnpg)
011200161103     d DISora          s                   like(brvhcs)
011300161103     d IDdata          s                   like(d53dfv)
011400161103     d IDora           s                   like(brvhcs)
011500161103     d IDfgs           s                   like(brvfgs)
011600161103     d IDnpg           s              1
011700161209     d wnic            s              1
011800110613     d wlib            s             10    inz('GAITRAGRPS')
011900110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
012000110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
012100110613     d Ora_iso         s               t   timfmt(*iso)
012200110613     d Dataiso         s               d   datfmt(*iso)
012300111013     d Dataeur         s               d   datfmt(*eur)
012400080327     d Datadmy         s               d   datfmt(*dmy)
012500110916
012600110916     d $MaxElem        c                   const(998)
012700080829     d
012800080317      *------------------------------------------------------------------------*
012900080317      *   P R O T O T I P I
013000080317      *------------------------------------------------------------------------*
013100080829      /copy gaitrasrc/srcprotopr,tibs34r
013200110617      /copy gaitrasrc/srcprotopr,trtcm8r1
013300110916      /copy gaitrasrc/srcprotopr,fnlv55r
013400001027     C*---------------------------------------------------------------*
013500001027     C     *ENTRY        PLIST
013600080325     C                   PARM                    kpjba
013700161031     c                   movel     kpjbu         fnlsw7ds
013800161214     C* ESEGUO OVRPRTF
013900161214     C                   Z-ADD     61            LUNG             15 5
014000161214     C                   MOVEL     *BLANKS       COMMAN           80
014100161214     C                   MOVEA     CMD           COMMAN
014200161214     C                   CALL      'QCMDEXC'
014300161214     C                   PARM                    COMMAN
014400161214     C                   PARM                    LUNG
014500161214     C*
014600161214     C                   Z-ADD     1             C                 3 0
014700161214     C                   OPEN      QSYSPRT
014800080317      /free
0149001610311      if comdsa=0 ;
015000161031        comdsa=comdsd ;
0151001610311      endif  ;
015200161102
015300161102       // Linee partenza selezionate
015400161102          EXSR  PrepaLNP      ;
015500161212
015600161031
0157001610311      if comdsa=comdsd ;
015800161031         *in01 =*on  ;
0159001610311       endif  ;
016000161102
016100161209       zz=1   ;
016200161031
0163001611021      dow lnpsel(zz)>0  ;
016400161102          setll (lnpsel(zz):kaad:kmgd) fnblp31l ;
016500161031          if *in01=*on  ;
016600161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
016700161031          else  ;
016800161102          reade (lnpsel(zz)) fnblp31l ;
016900161031          endif  ;
017000161031
0171001611022         dow not %eof(fnblp31l)   ;
017200161102
017300161102          exsr   Ctrsel    ;
017400161102
017500161102          if Wfine='S'  ;
017600161102          leave  ;
017700161102          endif  ;
017800161102
017900161102          if wok='S'   ;
018000161102          exsr elabora  ;
018100161102          endif  ;
018200161102
018300161102
018400161102          if *in01=*on  ;
018500161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
018600161102          else  ;
018700161102          reade (lnpsel(zz)) fnblp31l ;
018800161102          endif  ;
0189001611022         enddo  ;
019000161031
019100161102          zz=zz+1  ;
019200161031
0193001611021      enddo ;
019400111116
019500110617        // Se scritto almeno un record, invio all'indirizzo e-mail
019600161207        //pInFILE='WFKPI00F'   ;
019700161207        //pInIDJOB=knmeb       ;
019800161207          // Nell'oggetto imposto il primo codice cliente richiesto
019900161207  2     //if  oggcli>0   ;
020000161207        //chain (1:dutkci:oggcli) cnaco00f  ;
020100161207  3     //if not %found(cnaco00f)  ;
020200161207        //  acorag=*all'?'  ;
020300161207  3     //endif  ;
020400111202
020500161207        //  pinEMLVAR='*OBJM* Anomalie Cliente: '+%editc(oggcli:'X')+ '-'+
020600161207        //             %subst(acorag:1:15)  ;
020700161207  x2    //else  ;
020800111202          // Imposto la prima lnp/serie richiesta   ;
020900161207        // pinEMLVAR='*OBJM* Anomalie Lnp/Ser:'+%editc(lnp(1):'X')+' '+
021000161207        //                               nrs(1)      ;
021100161207  2     //endif  ;
021200111206
021300161207        //  pInEMLDEST=%trim(videmail)+'@brt.it' ;
021400161102        //callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
021500161102        //                pInEMLVAR:pInMRAKE2:pOutESITO) ;
021600080829
021700161214        except fine   ;
021800161214
021900080317        *inlr ='1'   ;
022000080828
022100110913       //-------------------------------------------------------------------
022200161102        BEGSR   Ctrsel  ;
022300161102
022400161102        Wok='N'  ;
022500161102        Wfine=' '  ;
022600161209        Wnic =' '  ;
022700161216        clear wesARB  ;
022800161227        clear evemem  ;
022900161227        clear evesalta;
023000161227        clear conta   ;
023100161212
023200161212        // Escludo le bolle con CTM 4 e 4F
023300161212        if arbctm ='4 ' or arbctm='4F' ;
023400161212          WOK='N'  ;
023500161212          leavesr  ;
023600161212        endif  ;
023700161216
023800161216        // Escludo le bolle consegnate o non consegnate se richiesto
023900161216        //  verifico la data consegna da ARB se non è dirottata
024000161216        if vidcons<>' '  and arbcca<>'1'  ;
024100161216        chain  (arbaas:arblnp:arbnrs:arbnsp)   fnarb01l     ;
024200161216          if %found(fnarb01l)  ;
024300161216           wesARB='S'  ;
024400161216          else  ;
024500161216           wesARB='N'  ;
024600161216          endif  ;
024700161216        endif  ;
024800161216
024900161216        if wesarb<>'S'   ;
025000161216        if vidcons='S' and arbdcm=0 or vidcons='N' and arbdcm>0 ;
025100161216          WOK='N'  ;
025200161216          leavesr  ;
025300161216        endif  ;
025400161216        else  ;
025500161216        if vidcons='S' and a_arbdcm=0 or vidcons='N' and a_arbdcm>0 ;
025600161216          WOK='N'  ;
025700161216          leavesr  ;
025800161216        endif  ;
025900161216        endif  ;
026000161212
026100161212        // escludo le bolle "figlie"
026200161212        chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl01l  ;
026300161212        if %found(fnlbl01l)  ;
026400161212          WOK='N'  ;
026500161212          leavesr  ;
026600161212        endif  ;
026700161102
0268001611021       if (arbaas*10000)+arbmgs>comdsa ;
026900161102        Wfine='S'  ;
027000161102        leavesr ;
0271001611021       endif  ;
027200161102
027300161102        // controllo cliente e serie
0274001611021       if arbccm>0  ;
027500161102         wccm= arbccm  ;
027600161102        else  ;
027700161102         wccm=arbksc  ;
0278001611021       endif  ;
027900161102
028000161102        xx= 1 ;
028100161212        clear wabi  ;
028200161102
028300161102 3      dow xx<= 20  ;
028400161102 4      if lnp(xx) >0  ;
028500161212 4a     if lnp(xx) = arblnp  ;
028600161102
028700161212        if cli(xx)>0 and wccm<>cli(xx)  ;
028800161212        chain (cli(xx):wccm) fiabk01l  ;
028900161212          if %found(fiabk01l) ;
029000161212           eval wabi='S'  ;
029100161213       //   else   ;
029200161213       //     chain (wccm) fiabk02l  ;
029300161213       //     if %found(fiabk02l) ;
029400161213       //      eval wabi='S'  ;
029500161213       //     endif  ;
029600161212          endif  ;
029700161212        endif  ;
029800161212
029900161212 5      if cli(XX)=0 or wccm= cli(xx) or wabi='S'         ;
030000161102
030100161102        if   nrs(xx)='99'   ;
030200161102         WOK='S'  ;
030300161102          leavesr  ;
030400161102        endif   ;
030500161102
030600161102 6      if   nrs(XX)='SI' and arbnrs>0    ;
030700161102         WOK='S'  ;
030800161102          leavesr  ;
030900161102 x6     else  ;
031000161102
031100161102        alfanrs=%editc(arbnrs:'X')  ;
031200161102 7       if alfanrs= nrs(xx)  ;
031300161102         WOK='S'  ;
031400161102          leavesr  ;
031500161102 7       endif  ;
031600161102 6      endif  ;
031700161102
031800161102 5      endif  ;
031900161102 4      endif  ;
032000161212 4a     endif  ;
032100161102
032200161102        xx=xx+1  ;
032300161102 3      enddo  ;
032400161102
032500161102
032600161102
032700161102        ENDSR  ;
032800161102       //-------------------------------------------------------------------
032900161102       BEGSR  Elabora                ;
033000161207
033100161209       ex88=' '   ;
033200161209       clear wdse;
033300161209       clear                   DISfgs     ;
033400161209       clear                   DISdata    ;
033500161209       clear                   DISnpg     ;
033600161209       clear                   DISora     ;
033700161209       clear                   IMPdata    ;
033800161209       clear                   IMPfgs     ;
033900161209       clear                   IDnpg      ;
034000161209       clear                   IDdata     ;
034100161209       clear                   IDora      ;
034200161209       clear                   IDfgs      ;
034300161209       clear evedt    ;
034400161209       clear evedes   ;
034500161209       clear evedim   ;
034600161209       clear evehim   ;
034700161209
034800161207       clear wfkpi000  ;
034900161207
035000161209       // calcolo ore reali di consegna tra data sped e data consegna
035100161207       exsr cal_gg    ;
035200161212
035300161216
035400161212       // Se le bolle OK non le voglio --> salto
0355001612120      if vidok <>'N' or  kpidly_c>0  ;
035600161207
035700161212       // se si vogliono gli eventi anche bolle ok elaboro
0358001612120a     if kpidly_c > 0  or vidokeve<>'N' ;
035900161102
036000161102       setll (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
036100161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0362001611021      dow  not %eof(fnblt01l)  ;
036300161214       clear memdse  ;
036400161102
036500161214       // verifico se presente una spunta 88/90
0366001612142      if bltdse>arbdbr or bltdse=0 or bltnpp=88 or bltnpp=90  ;
036700161102       setll  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
036800161102       reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
0369001611023      dow not %eof(fnbrv09l)  ;
037000161214
037100161214       // per testimoniare la presenza del collo metto qualsiasi data
037200161214       // entrata, disguido, arrivo. Quindi tratto la pistola 88 e 90
037300161214       //  allo stesso modo
037400161214
0375001612144       if brvnps=88 or brvnps=90 ;
037600161102         ex88='S'   ;
0377001612145        if  bltdse>0  ;
037800161214           memdse=bltdse;
037900161214x5       else  ;
038000161214          chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0381001612146         if %found(fnart01l) ;
0382001612147         if artdet>0 and artdet<artdam ;
038300161214           memdse=artdet;
038400161214x7        else   ;
0385001612148         if artdam>0  ;
038600161214           memdse=artdam;
0387001612148         endif  ;
0388001612147         endif  ;
0389001612146         endif  ;
0390001612145         endif  ;
039100161214
039200161214 4       endif  ;
039300161214
039400161214        if   memdse>0  ;
039500161214         leave ;
039600161214        endif  ;
039700161214
0398001612144       //if brvnps=90 ;
039900161214        //  chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0400001612145       // if artdam>bltdse  ;
040100161214        //  ex88='S'   ;
040200161214        //  wdse=bltdse;
040300161214        //  leave  ;
0404001612145       // endif  ;
0405001612144       //endif  ;
040600161103
040700161102
0408001611023      reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
040900161102       enddo  ;
0410001611022      endif   ;
041100161209
041200161214       // se era spunta in 88 memorizzo la più recente come data ;
041300161214       if memdse>wdse  ;
041400161214         wdse=memdse  ;
041500161214       endif  ;
041600161214
041700161214       // Verifico se in IMP/Disguido ne basta uno per bolla
041800161214       if idnpg=' '  ;
041900161214         exsr IMPDISG  ;
042000161214       endif  ;
042100161102
042200161102
042300161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0424001611021      enddo  ;
042500161102
042600161209       // verifico se la trazione è ritardo , usando la data arrivo ult collo
042700161216       if wesARB='S'         ;
042800161103       endif  ;
042900161209
043000161209       // Lettura eventi
043100161209       exsr leggi_eve  ;
0432001612270a     endif  ;
043300161103
043400161102       exsr   Scrivifile  ;
0435001612120      endif  ;
043600161212
043700161102       ENDSR   ;
043800161209       //-------------------------------------------------------------------
043900161209       BEGSR  Leggi_eve              ;
044000161209
044100161209       // Lettura eventi che "servono"
044200161209       clear YY  ;
044300161227       clear indx;
044400161209
044500161209       setll  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
044600161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
044700161209  1    dow  not %eof(fnevb01l)   ;
044800161209
044900161209  2    if %lookup(evbcev:cev)>0   ;
045000161209
045100161227       // Se si tratta di MIC, memorizzo quello con data e ora più alta
045200161227  3    if evbcev='MIC'   ;
045300161227       indx=%lookup(evbcev:evedes)  ;
045400161227  4    if indx>0 ;
045500161227        clear evedt(indx)   ;
045600161227        clear evedes(indx)   ;
045700161227        clear evedim(indx)   ;
045800161227        clear evehim(indx)   ;
045900161227  4    endif  ;
046000161227  3    endif;
046100161227
046200161209       yy=yy+1  ;
046300161227
046400161227  3    if yy<32   ;
046500161209
046600161209        evedt(yy)=evbdev   ;
046700161209        evedes(yy)=evbcev   ;
046800161209        evedim(yy)=evbdtv   ;
046900161209        evehim(yy)=evborv   ;
047000161209
047100161209       // e si tratta di NIC memorizzo anche data e ora immissione
047200161209       // lo elimino se trovo un MIC successivo
047300161209  4     if evbcev='NIC' ;
047400161209         eval wnic='S'   ;
047500161209  4     endif  ;
047600161209  3     endif  ;
047700161209
047800161227  2     endif  ;
047900161209
048000161209        // solo se presente NIC verifico se da eliminare
048100161209   3      if wnic='S' and evbcev='MIC'    ;
048200161209
048300161227        ee=1   ;
048400161227   4    dow ee<=32  ;
048500161227   5    if evedes(ee)='NIC' ;
048600161209
048700161209   6    select  ;
048800161227        when evedt(ee)<>evbdev ;
048900161209
049000161227        when evedim(ee)<evbdtv ;
049100161227        clear evedt(ee)   ;
049200161227        clear evedes(ee)   ;
049300161227        clear evedim(ee)   ;
049400161227        clear evehim(ee)   ;
049500161227        wnic=' '   ;
049600161209
049700161227        when evedim(ee)=evbdtv and evehim(ee)<evborv ;
049800161227        clear evedt(ee)   ;
049900161227        clear evedes(ee)   ;
050000161227        clear evedim(ee)   ;
050100161227        clear evehim(ee)   ;
050200161227        wnic=' '   ;
050300161209        endsl  ;
050400161209
050500161209   5    endif   ;
050600161209
050700161227        ee=ee+1  ;
050800161209   4    enddo  ;
050900161209
051000161209   3      endif  ;
051100161209
051200161209
051300161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
051400161209   1   enddo  ;
051500161209
051600161227       // il MIC lo tengo solo se è l'ultimo degli eventi altrimenti
051700161227       // lo emimino
051800161227       indx=%lookup('MIC':evedes)  ;
051900161227  1    if indx>0 ;
052000161227       yy  =indx+1  ;
052100161227  2    dow yy<=32   ;
052200161227  3     if  evedes(yy)<>*blanks   ;
052300161227         clear evedt(indx)   ;
052400161227         clear evedes(indx)   ;
052500161227         clear evedim(indx)   ;
052600161227         clear evehim(indx)   ;
052700161227         leave   ;
052800161227  3     endif  ;
052900161227
053000161227       yy=yy+1  ;
053100161227  2    enddo  ;
053200161227  1    endif  ;
053300161227
053400161227       // conto anche quanti eventi ci sono per memorizzare solo gli
053500161227       // ultimi 10
053600161227       yy=1  ;
053700161227       clear conta   ;
053800161227       dow yy<=32                ;
053900161227         if  evedes(yy)<>*blanks   ;
054000161227          conta=conta+1  ;
054100161227         endif  ;
054200161227       yy=yy+1  ;
054300161227       enddo  ;
054400161227
054500161227       clear   evesalta  ;
054600161227       if conta>10  ;
054700161227       evesalta=conta-10  ;
054800161227       endif   ;
054900161227
055000161209       ENDSR   ;
055100161102       //-------------------------------------------------------------------
055200161102       BEGSR  Scrivifile             ;
055300111116
055400110913
055500161102       kpiidute=knmeb   ;
055600161102       kpiepo=dutpou  ;
055700110913
055800161102       kpiccm=wccm    ;
055900161102       kpirsm=arbrsm  ;
056000161102
056100161102       kpiaas=arbaas  ;
056200161102       kpilnp=arblnp  ;
056300161102       kpimes=%int(%subst(%editc(arbmgs:'X'):1:2))  ;
056400161102       kpirmn=arbrmn  ;
056500161102       kpirma=arbrma  ;
056600161102       kpinrs=arbnrs  ;
056700161102       kpinsp=arbnsp  ;
056800161102       kpimgs=arbmgs  ;
056900161102       kpilna=arblna  ;
057000161102       kpirsd=arbrsd  ;
057100161102       kpilod=arblod  ;
057200161102       kpiprd=arbprd  ;
057300161102       kpicad=arbcad  ;
057400161215       kpifin=arbfin  ;
057500161102       kpinzd=arbnzd  ;
057600161102       kpiffd=arbffd   ;
057700161102       kpitc1=arbtc1   ;
057800161102       kpitc2=arbtc2   ;
057900161102       kpigc1=arbgc1   ;
058000161102       kpigc2=arbgc2   ;
058100161102       kpincl=arbncl   ;
058200161102       kpipkb=arbpkb   ;
058300161102       kpivlb=arbvlb   ;
058400161102       kpitcr=arbtcr   ;
058500161102       kpidcr=arbdcr   ;
058600161102       kpidcp=arbdcp   ;
058700161102       kpidcm=arbdcm   ;
058800161102       kpihmc=arbhmc   ;
058900161209       kpicca=arbcca   ;
059000161209
059100161216       if arbdcm=0 and wesarb='S' and a_arbdcm>0 and arbcca<>'1';
059200161209       kpidcm=a_arbdcm   ;
059300161209       kpihmc=a_arbhmc   ;
059400161209       kpicca=a_arbcca   ;
059500161209       endif  ;
059600161209
059700161209       if kpicca<>' '  ;
059800161209       tblkey=kpicca   ;
059900161209       chain (1:'7O':tblkey) tabel  ;
060000161209         if %found(tabel00f )  ;
060100161209          kpicca_des=tbluni   ;
060200161209         endif  ;
060300161209       endif  ;
060400161103
060500161103        // pistola ex 88
060600161102        if ex88='S'     ;
060700161102        kpip88='S'      ;
060800161103        kpidse=wdse     ;
060900161102        endif           ;
061000161103
061100161103        //IMP disguido
061200161103        if IDnpg<>' '   ;
061300161103           if IDnpg='P'   ;
061400161103           kpiimpds='P'   ;
061500161103           else  ;
061600161103           kpiimpds='D'   ;
061700161103           endif ;
061800161103        endif ;
061900161209
062000161209       yy=1  ;
062100161227       clear evemem   ;
062200161209
062300161227 1     dow yy<=10                ;
062400161227 2     if  evedes(yy)<>*blanks   ;
062500161227
062600161227       evemem=evemem+1  ;
062700161227
062800161227       // se conta < 10 memorizzo tutti altrimenti salto i primi
062900161227
063000161227 3     if conta <=10 or evemem>evesalta  ;
063100161227 4      select   ;
063200161227         when KPICEV1=*blanks and evedes(yy)<>'MIC';
063300161209         KPICEV1=evedes(yy)  ;
063400161209         KPIDEV1=evedt(yy)  ;
063500161227         when KPICEV2=*blanks and evedes(yy)<>'MIC';
063600161209         KPICEV2=evedes(yy)  ;
063700161209         KPIDEV2=evedt(yy)  ;
063800161227         when KPICEV3=*blanks and evedes(yy)<>'MIC';
063900161209         KPICEV3=evedes(yy)  ;
064000161209         KPIDEV3=evedt(yy)  ;
064100161227         when KPICEV4=*blanks and evedes(yy)<>'MIC';
064200161209         KPICEV4=evedes(yy)  ;
064300161209         KPIDEV4=evedt(yy)  ;
064400161227         when KPICEV5=*blanks  and evedes(yy)<>'MIC';
064500161209         KPICEV5=evedes(yy)  ;
064600161209         KPIDEV5=evedt(yy)  ;
064700161227         when KPICEV6=*blanks  and evedes(yy)<>'MIC';
064800161209         KPICEV6=evedes(yy)  ;
064900161209         KPIDEV6=evedt(yy)  ;
065000161227         when KPICEV7=*blanks  and evedes(yy)<>'MIC';
065100161227         KPICEV7=evedes(yy)  ;
065200161209         KPIDEV7=evedt(yy)  ;
065300161227         when KPICEV8=*blanks  and evedes(yy)<>'MIC';
065400161209         KPICEV8=evedes(yy)  ;
065500161209         KPIDEV8=evedt(yy)  ;
065600161227         when KPICEV9=*blanks  and evedes(yy)<>'MIC';
065700161209         KPICEV9=evedes(yy)  ;
065800161209         KPIDEV9=evedt(yy)  ;
065900161209         when KPICEV0=*blanks  ;
066000161209         KPICEV0=evedes(yy)  ;
066100161209         KPIDEV0=evedt(yy)  ;
066200161227 4       endsl  ;
066300161227 3       endif  ;
066400161227 2       endif  ;
066500161209
066600161209         yy=yy+ 1;
066700161209
066800161227 1     enddo  ;
066900110916
067000161102       write  wfkpi000 ;
067100110913
067200161214       CONTAB=CONTAB+1   ;
067300110926
067400110913       ENDSR                  ;
067500080327       //-------------------------------------------------------------------
067600080327       BEGSR  *INZSR                 ;
067700080328
067800080328       in(E) *dtaara   ;
067900080328       if    %error or rsut  =*blanks  ;
068000080328       callp TIBS34R (TIBS34DS)        ;
068100080328       in    *dtaara   ;
068200080328       endif                           ;
068300080829
068400110613       // imposto data e ora del giorno
068500110613       dataiso=datasys  ;
068600161214       dataeur=datasys  ;
068700110613       dateu=%dec(dataiso)   ;
068800161214       dataogg=%dec(dataeur)   ;
068900110616
069000110613       ora_iso=ora_sys  ;
069100110613       ora_u=%dec(ora_iso)   ;
069200080829
069300110913       // Se richiesto file pulisco il lancio precedente per filiale
069400161102       setll (dutpou)   wfKPI01l   ;
069500161102       reade (dutpou)   wfKPI01l   ;
069600110616
069700161102       dow  not %eof(wfKPI01l)   ;
069800161102       delete wfKPI000  ;
069900110616
070000161102       reade (dutpou)   wfKPI01l   ;
070100110616       enddo  ;
070200110616
070300161209       clear xx ;
070400161209       setll  (1:'2A') tabel00f ;
070500161209       reade  (1:'2A') tabel00f ;
070600161209
070700161209       dow not %eof(tabel00f ) ;
070800161209       if tblflg=' '  ;
070900161209        ds2a = tbluni   ;
071000161209
071100161209       if   §2aftg='S' or §2atip='N' or §2aftc='A'  ;
071200161209       xx=xx+1  ;
071300161209        cev(xx)=%Subst(tblkey:1:3)  ;
071400161209       endif  ;
071500161209       endif  ;
071600161209
071700161209       reade  (1:'2A') tabel00f ;
071800161209       enddo  ;
071900161209
072000161209       setll  (1:'2Z') tabel00f ;
072100161209       reade  (1:'2Z') tabel00f ;
072200161209
072300161209       dow not %eof(tabel00f ) ;
072400161209       if tblflg=' '  ;
072500161209        ds2z = tbluni   ;
072600161209
072700161209       if   §2zcep<>' '  ;
072800161209       xx=xx+1  ;
072900161209        cev(xx)=%subst(tblkey:1:3)  ;
073000161209       endif  ;
073100161209       endif  ;
073200161209
073300161209       reade  (1:'2Z') tabel00f ;
073400161209       enddo  ;
073500161209
073600161209       // infine aggiungo fisso il NIC
073700161209       xx=xx+1  ;
073800161209       cev(xx)='NIC' ;
073900161227       xx=xx+1  ;
074000161227       cev(xx)='MIC' ;
074100161209
074200161209       ENDSR     ;
074300161102       //-------------------------------------------------------------------
074400161209       BEGSR  PrepaLNP               ;
074500161102
074600161102       clear  LnpSel  ;
074700161102       xx= 1 ;
074800161102       yy= 1 ;
074900161102
075000161102       dow xx <=20  ;
075100161102        if lnp(xx)>0  ;
075200161102         Indx=%lookup(lnp(xx):LnpSel)  ;
075300161102         if Indx=0  ;
075400161102           eval lnpSel(yy)=lnp(xx)  ;
075500161102           yy=yy+1   ;
075600161102         endif  ;
075700161102
075800161102        endif  ;
075900161102
076000161102        xx=xx+1   ;
076100161102        enddo     ;
076200161102
076300161102        // Memorizzo nel campo oggcli il codice cliente che devo
076400161102        //  mettere nell'ogggetto della mail
076500161212        // clear oggcli  ;
076600161212        // if cli(1)>0   ;
076700161212        //   oggcli=cli(1)  ;
076800161212        // endif  ;
076900161102
077000161102       ENDSR  ;
077100161207
077200161103
077300161103      /end-free
077400161103
077500161207     c*-------------------------------------------------------------------
077600161207     c     Cal_gg        BEGSR
077700161207     c
077800161209     c     karb          klist
077900161209     c                   kfld                    arbaas
078000161209     c                   kfld                    arblnp
078100161209     c                   kfld                    arbnrs
078200161209     c                   kfld                    arbnsp
078300161209
078400161216     c* chain solo se non l'ho già fatto prima
078500161216     c                   if        wesarb=' '
078600161209     c     karb          chain     fnarb01l
078700170105     c                   if        %found(fnarb01l)
078800170105     c                   eval      wesARB='S'
078900170105     c                   else
079000170105     c                   eval      wesARB='N'
079100170105     c                   endif
079200161216     c                   endif
079300161216
079400161207     c                   clear                   tnsd99ds
079500161207     C                   MOVEL     ' '           D98TLA
079600161216     c                   if        wesARB='S'
079700161207     C                   MOVEL     'A'           D98TBO
079800161209     c                   else
079900161209     C                   MOVEL     'P'           D98TBO
080000161209     c                   endif
080100161207     C                   MOVEL     ARBAAS        D98AAS
080200161207     C                   MOVEL     ARBLNP        D98LNP
080300161207     C                   MOVEL     ARBNRS        D98NRS
080400161207     C                   MOVEL     ARBNSP        D98NSP
080500161207     c
080600161207     c                   if        arbtsp='D'
080700161207     c                   eval      I98TSP_FOR='C'
080800161207     c                   endif
080900161207     C*
081000161207     C                   CALL      'TNSD99R'
081100161207     C                   PARM                    TNSD99DS
081200161207     c
081300161207     c* ore teoriche
081400161209     c                   eval      KPIdly_T=d98trc
081500161209     c                   eval      KPIdag  =d98dag
081600161209     c                   eval      KPIdcg  =d98dcg
081700161215     c                   eval      KPIdac  =d98dac
081800161215     c                   eval      KPIdcc  =d98dcc
081900161207
082000161207     c* Richiamo il pgm utilizzando la DS eliminando il num sped
082100161207     c*  e la giacenza
082200161207     c                   clear                   d98nsp
082300161207     c                   clear                   d98dti
082400161207     c                   clear                   d98hti
082500161207     c                   clear                   d98cmc
082600161207     c                   clear                   d98dag
082700161207     c                   clear                   d98dcg
082800161207     c                   eval      d98duc=d98dsp
082900161207     c                   eval      d98dbr=d98dsp
083000170105     c                   eval      D98FDRT='S'
083100161207     c                   clear                   d98dam
083200161207     C                   CALL      'TNSD99R'
083300161207     C                   PARM                    TNSD99DS
083400161207     c
083500161207     c* controllo le ore reali  con le ore teoriche
083600161209     c                   eval      KPIdly_R=d98rrc
083700161215     c
083800161215     c* se la spedizione è borderizzata in data festva --> aggiungo 24 ore
083900161215     c*  alle ore reali
084000170105     c* ES 05/1/17 non lo faccio più in quanto  ho modificato allo stesso modo
084100170105     c*  il calcolo della affidabilità tnsd99r
084200170105     c**                 if        arbdbr>0
084300170105     c**                 clear                   wdat8
084400170105     c**                 clear                   tfptfa
084500170105     c**                 z-add     arbdbr        datada
084600170105     c*+                 z-add     arbdbr        dataa
084700170105     c**                 z-add     arbtfp        p_tfp
084800170105     c*+                 z-add     arbtfa        p_tfa
084900170105     c**                 call      'XSRLAV8'
085000170105     c**                 parm                    wdat8
085100170105     c**                 parm                    tfptfa
085200161215     c* se giorno FESTIVO aggiungo 24 ore
085300170105     c**                 if        ggl=0
085400170105     c**                 eval      KPIdly_R=KPIdly_R+24
085500170105     c**                 endif
085600161215
085700170105     c**                 endif
085800161215     c
085900161207     c                   if        kpidly_r<=kpidly_t
086000161207     c                   clear                   kpidly_c
086100161207     c                   else
086200161207     c
086300161207     c                   eval      kpidly_c =kpidly_r-kpidly_t
086400161207     c                   endif
086500161207     c*
086600161213     c* CONSEGNA RICHIESTA IL
086700161212     c* Se data richiesta = data consegna merce
086800161209     c*  sempre OK
086900161212     c**                 if        arbtc1='A' or arbtc2='A'  or
087000161212     c**                           arbtc1='S' or arbtc2='S'
087100161213     c                   if        arbdcr>0 and arbtcr=' '
087200161213     c                   if        arbdcr=arbdcm
087300161209     c                   clear                   kpidly_c
087400161209     c                   endif
087500161213     c* se bolla NON consegnata e data cons richiesta > di oggi
087600161213     c* per ora ON TIME
087700161213     c                   if        arbdcm=0 and arbdcr>=dateu
087800161213     c                   clear                   kpidly_c
087900161213     c                   endif
088000161213     c                   endif
088100161213     c
088200161212     c**                 endif
088300161213
088400161213     c* CONSEGNA RICHIESTA PRIMA DEL
088500161213     c                   if        arbdcr>0 and arbtcr='P'
088600161213     C                   IF        ARBDCM>0 AND ARBDCM<ARBDCR
088700161213     c                   clear                   kpidly_c
088800161213     c                   endif
088900161213     C                   IF        ARBDCM=0
089000161213     c                   clear                   kpidly_c
089100161213     c                   endif
089200161213     c                   endif
089300161213     c* CONSEGNA RICHIESTA DOPO IL
089400161213     c                   if        arbdcr>0 and arbtcr='D'
089500161213     C                   IF        ARBDCM>0 AND ARBDCM>ARBDCR
089600161213     c                   clear                   kpidly_c
089700161213     c                   endif
089800161213     C                   IF        ARBDCM=0
089900161213     c                   clear                   kpidly_c
090000161213     c                   endif
090100161213     c                   endif
090200161207
090300161212     c* se cons anomala RESO o DIROTTAMENTO --> sempre ok
090400161212     c                   if        arbcca='1' or arbcca='2' or
090500161212     c                             arbcca='6'
090600161212     c                   clear                   kpidly_c
090700161212     c                   endif
090800161207     c*
090900161207     c                   ENDSR
091000161103     c*-------------------------------------------------------------------
091100161103     c     IMPdisg       BEGSR
091200161209     c
091300161209     c     kbrv          Klist
091400161209     c                   Kfld                    bltfls
091500161209     c                   Kfld                    bltlna
091600161209     c                   Kfld                    bltnrs
091700161209     c                   Kfld                    bltnsc
091800161103     c
091900161103     c* per ogni collo verifico se in IMP o disguidato
092000161103     c* prendo la data più alta
092100161103     c     kbrv          setll     fnbrv07l
092200161103     c     kbrv          reade     fnbrv07l
092300161103    2c                   dow       not %eof(fnbrv07l)
092400161103    3c                   if        brvatr='D'  or brvnpg=3
092500161103     c* prendo la data della spunta
092600161103     c                   clear                   fnlv53ds
092700161103     C                   MOVEL     BRvNPG        D53NPG
092800161103     C                   Z-ADD     BRvNFV        D53NFV
092900161103     C                   MOVEL     BRvFGS        D53FGS
093000161103     C                   CALL      'FNLV53R'
093100161103     C                   PARM                    fnlv53ds
093200161103    4c                   if        d53err<>*blanks
093300161103    5c                   if        brvdcs>0
093400161103     c                   eval      d53dfv=brvdcs
093500161103     c                   else
093600161103    6c                   if        brvdfs>0
093700161103     c                   eval      d53dfv=brvdfs
093800161103    6c                   endif
093900161103    5c                   endif
094000161103    4c                   endif
094100161103     c
094200161103    4c                   select
094300161103     c* se si tratta di DISGUIDO
094400161103     c                   when      brvatr='D'
094500161103     c
094600161103    5c                   if        d53dfv=DISdata
094700161103     c* Verifico l'ora
094800161103    6c                   if        brvhcs>0 and brvhcs>DISora
094900161103     c                   eval      DISdata=d53dfv
095000161103     c                   eval      DISfgs =brvfgs
095100161103     c                   eval      DISnpg =brvnpg
095200161103     c                   eval      DISora =brvhcs
095300161103   x6c                   else
095400161103    7c                   if        brvhms>0 and brvhms>DISora
095500161103     c                   eval      DISdata=d53dfv
095600161103     c                   eval      DISfgs =brvfgs
095700161103     c                   eval      DISnpg =brvnpg
095800161103     c                   eval      DISora =brvhms
095900161103    7c                   endif
096000161103    6c                   endif
096100161103    5c                   endif
096200161103     c
096300161103    5c                   if        d53dfv>DISdata
096400161103     c                   eval      DISdata=d53dfv
096500161103     c                   eval      DISfgs =brvfgs
096600161103     c                   eval      DISnpg =brvnpg
096700161103     c                   eval      DISora =brvhcs
096800161103     c                   if        disora=0
096900161103     c                   eval      DISora =brvhms
097000161103     c                   endif
097100161103    5c                   endif
097200161103     c
097300161103     c* se si tratta di IMP
097400161103    4c                   when      d53spg='P' and d53dfv>impdata
097500161103     c                   eval      IMPdata=d53dfv
097600161103     c                   eval      IMPfgs =brvfgs
097700161103    4c                   endsl
097800161103    3c                   endif
097900161103
098000161103     c     kbrv          reade     fnbrv07l
098100161103    2c                   enddo
098200161103
098300161103     c* memorizzo la data più alta
098400161103     c* IMP vince sul disguido per stessa data, senza guardare l'ora
098500161103    2c                   if        IMPdata>0 and IMPdata>=IDdata
098600161103     c                   eval      IDdata=IMPdata
098700161103     c                   eval      IDfgs=IMPfgs
098800161103     c                   eval      IDora=0
098900161103     c                   eval      IDnpg='P'
099000161103    2c                   endif
099100161103    2c                   if        (DISdata>0 and DISdata=IDdata
099200161103     c                             and IDnpg<>'P' and DISora>IDora) or
099300161103     c                             (DISdata>0 and DISdata>IDdata)
099400161103     c                   eval      IDdata=DISdata
099500161103     c                   eval      IDfgs=DISfgs
099600161103     c                   eval      IDora=DISora
099700161103     c                   eval      IDnpg=%char(DISnpg)
099800161103    2c                   endif
099900161103     c
100000161103     C                   ENDSR
100100161214     OQSYSPRT   E            fine             02
100200161214     O                                              'Elaborazione esiti consegn'
100300161214     o                                              'a il'
100400161214     o                       dataogg           +  2 '  /  /    '
100500161214     o                       ora_u             +  2 '  :  :  '
100600161214     o                                         +  2 'Bolle scritte :'
100700161214     o                       contab        2   +  2
100800161214**
100900161214OVRPRTF FILE(QSYSPRT) USRDTA('EsitiCons') HOLD(*YES)
