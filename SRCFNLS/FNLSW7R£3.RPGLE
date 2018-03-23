000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200161031      * FNLSW7R *----------------------------------------------------------*
000300161031      *   Invio bolle KPI  ai clienti mittenti
000400080319      *--------------------------------------------------------------------*
000500111202     Fcnaco00f  IF   E           K DISK
000600161102     Ffnblp31l  IF   E           K DISK    prefix(arb:3)
000700161103     Ffnarb01l  IF   E           K DISK    prefix(A_)
000800170524     Ffiarbf2c  IF   E           K DISK    prefix(V_)
000900170524     Ffiar501l  IF   E           K DISK
001000161102     Ffnblt01l  IF   E           K DISK
001100161102     Ffnart01l  IF   E           K DISK
001200110913     Fazorg01l  IF   E           K DISK
001300161103     Ffnbrv07l  IF   E           K DISK    rename(fnbrv000:fnbrv007)
001400161103     Ffnbrv09l  IF   E           K DISK
001500080327     Ftabel00f  IF   E           K DISK
001600161207     Ffnevb01l  IF   E           K DISK
001700161212     Ffnlbl01l  IF   E           K DISK
001800161212     Ffiabk01l  IF   E           K DISK
001900161212     Ffiabk02l  IF   E           K DISK    rename(fiabk000:fiabk002)
002000161031     Fwfkpi01L  uF a E           K DISK
002100170609     FTIVGD00F  O    E             DISK    USROPN COMMIT
002200161214     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
002300161214
002400050705      *
002500161102     d*  linee partenza selezionate prese una sola volta
002600161214     D CMD             S              1    DIM(61) CTDATA PERRCD(61)
002700161214     d
002800161102     D lnpSel          S              3  0 DIM(20)
002900161227     D evedt           S              8  0 DIM(32)
003000161227     D evedes          S              3    DIM(32)
003100161227     D evedim          S              8  0 DIM(32)
003200161227     D evehim          S              6  0 DIM(32)
003300161209     D cev             S              3    DIM(199)
003400170523     D cevt            S              1    DIM(199)
003500161102     d
003600110913     d KPJBA         e ds
003700080328     d tibs34ds      e ds                  inz
003800080410     d tibs02ds      e ds                  inz
003900080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
004000080328     d DDatiUte      e ds                  dtaara(§datiute)
004100110916     d fnlv55ds      e ds
004200161103     d fnlv53ds      e ds
004300161209     d ds2a          e ds
004400161209     d ds2z          e ds
004500161207     D TNSD99DS      E DS                  INZ
004600170524     D dar5gen       E DS                  INZ
004700170609     D TIS7VASDS     E DS
004800170609     D DS_KPI        E DS                  extname(wfkpi00f)
004900170609     D psds           sds
005000170609     D  procname         *PROC
005100080829     d
005200161212     D fnlsw7ds        DS
005300161031     D  COMdsd                 1      8  0
005400161031     D  kaad                   1      4  0
005500161031     D  kmgd                   5      8  0
005600161031     D  COMdsa                 9     16  0
005700161031     D  kaaa                   9     12  0
005800161031     D  kmga                  13     16  0
005900110914     D  LNP                   17     56P 0
006000110914     D                                     DIM(20)
006100110914     D  CLI                   57    136P 0
006200110914     D                                     DIM(20)
006300110914     D  NRS                  137    176
006400110914     D                                     DIM(20)
006500110914     D  videmail             177    201
006600161212     D  parges               202    202
006700161212     D  vidok                203    203
006800161212     D  vidokeve             204    204
006900161216     D  vidcons              205    205
007000161207     d
007100161207     D WDAT8           DS
007200161207     D  DATADA                 1      8  0
007300161207     D  DATAA                  9     16  0
007400161207     D  GGL                   17     21  0
007500161207     d
007600161207     D tfptfa          DS
007700161207     D  p_tfp                  1      3  0
007800161207     D  p_tfa                  4      6  0
007900161207     d
008000161207     D                 DS
008100161207     D  arbaas                 1      4  0
008200161207     D  arbmgs                 5      8  0
008300161207     D  arbdsp                 1      8  0
008400110914     d
008500110914     d
008600110617     D  pInFILE        s             10
008700110617     D  pInIDJOB       s             26
008800110617     D  pInPWDXLS      s             15
008900110617     D  pInNOMXLS      s             78
009000111206     D  pInEMLVAR      s             80
009100110617     D  pInEMLDEST     s            121
009200120504     D  pInMRAKE2      s             15    inz
009300110617     D  pOutESITO      s              1
009400161102     d XX              s              3  0
009500110913     d yy              s              3  0
009600110916     d ee              s              3  0
009700110916     d zz              s              3  0
009800161102     d wccm            s              7  0
009900161102     d dateu           s              8  0
010000161214     d dataogg         s              8  0
010100161102     d wdse            s              8  0
010200161214     d memdse          s              8  0
010300170524     d datadcr         s              8  0
010400161214     d CONTAB          s              9  0 INZ
010500161102     d ora_u           s              6  0
010600161227     d conta           s              3  0
010700161227     d evemem          s              3  0
010800161227     d evesalta        s              3  0
010900161102     d alfanrs         s              2
011000161102     d ex88            s              1
011100161102     d wok             s              1
011200161212     d wabi            s              1
011300161102     d wfine           s              1
011400161216     d wesARB          s              1
011500161102     d Indx            s              3  0
011600161102     d oggcli          s              7  0
011700170525     d dtprimoeve      s              8  0
011800161103     d IMPdata         s                   like(d53dfv)
011900161103     d IMPfgs          s                   like(brvfgs)
012000161103     d DISfgs          s                   like(brvfgs)
012100161103     d DISdata         s                   like(d53dfv)
012200161103     d DISnpg          s                   like(brvnpg)
012300161103     d DISora          s                   like(brvhcs)
012400161103     d IDdata          s                   like(d53dfv)
012500161103     d IDora           s                   like(brvhcs)
012600161103     d IDfgs           s                   like(brvfgs)
012700170524     d ktrd            s                   like(ar5trd) inz('GEN')
012800161103     d IDnpg           s              1
012900161209     d wnic            s              1
013000110613     d wlib            s             10    inz('GAITRAGRPS')
013100110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
013200110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
013300110613     d Ora_iso         s               t   timfmt(*iso)
013400110613     d Dataiso         s               d   datfmt(*iso)
013500111013     d Dataeur         s               d   datfmt(*eur)
013600080327     d Datadmy         s               d   datfmt(*dmy)
013700110916
013800110916     d $MaxElem        c                   const(998)
013900080829     d
014000080317      *------------------------------------------------------------------------*
014100080317      *   P R O T O T I P I
014200080317      *------------------------------------------------------------------------*
014300080829      /copy gaitrasrc/srcprotopr,tibs34r
014400110617      /copy gaitrasrc/srcprotopr,trtcm8r1
014500110916      /copy gaitrasrc/srcprotopr,fnlv55r
014600001027     C*---------------------------------------------------------------*
014700001027     C     *ENTRY        PLIST
014800080325     C                   PARM                    kpjba
014900161031     c                   movel     kpjbu         fnlsw7ds
015000161214     C* ESEGUO OVRPRTF
015100161214     C                   Z-ADD     61            LUNG             15 5
015200161214     C                   MOVEL     *BLANKS       COMMAN           80
015300161214     C                   MOVEA     CMD           COMMAN
015400161214     C                   CALL      'QCMDEXC'
015500161214     C                   PARM                    COMMAN
015600161214     C                   PARM                    LUNG
015700161214     C*
015800161214     C                   Z-ADD     1             C                 3 0
015900161214     C                   OPEN      QSYSPRT
016000170609     C*
016100170609     C* Apertura file DOWNLOAD
016200170609     C                   if        not %open(tivgd00f)
016300170609     C                   open      tivgd00f
016400170609     C                   endif
016500170609     C*
016600170609     C* Inizializzo la transazione DOWNLOAD
016700170609     C                   CLEAR                   TIS7VASDS
016800170609     C                   EVAL      i§7VASOPZ = 'PRG'
016900170609     C                   CALL(e)   'TIS7VASR1'
017000170609     C                   PARM                    TIS7VASDS
017100170609     C*
017200170609     C* Se OK => proseguo
017300170609     C                   if        not %error AND
017400170609     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
017500170609     C                   movel     'S'           wProcedi          1
017600170609     C                   else
017700170609     C                   movel     'N'           wProcedi
017800170609     C                   endif
017900170609     C*
018000170609     C* Se ok a procedere => elaboro
018100170609     C                   if        wProcedi = 'S'
018200080317      /free
0183001610311      if comdsa=0 ;
018400161031        comdsa=comdsd ;
0185001610311      endif  ;
018600161102
018700161102       // Linee partenza selezionate
018800161102          EXSR  PrepaLNP      ;
018900161212
019000161031
0191001610311      if comdsa=comdsd ;
019200161031         *in01 =*on  ;
0193001610311       endif  ;
019400161102
019500161209       zz=1   ;
019600161031
0197001611021      dow lnpsel(zz)>0  ;
019800161102          setll (lnpsel(zz):kaad:kmgd) fnblp31l ;
019900161031          if *in01=*on  ;
020000161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
020100161031          else  ;
020200161102          reade (lnpsel(zz)) fnblp31l ;
020300161031          endif  ;
020400161031
0205001611022         dow not %eof(fnblp31l)   ;
020600161102
020700161102          exsr   Ctrsel    ;
020800161102
020900161102          if Wfine='S'  ;
021000161102          leave  ;
021100161102          endif  ;
021200161102
021300161102          if wok='S'   ;
021400161102          exsr elabora  ;
021500161102          endif  ;
021600161102
021700161102
021800161102          if *in01=*on  ;
021900161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
022000161102          else  ;
022100161102          reade (lnpsel(zz)) fnblp31l ;
022200161102          endif  ;
0223001611022         enddo  ;
022400161031
022500161102          zz=zz+1  ;
022600161031
0227001611021      enddo ;
022800111116
022900110617        // Se scritto almeno un record, invio all'indirizzo e-mail
023000161207        //pInFILE='WFKPI00F'   ;
023100161207        //pInIDJOB=knmeb       ;
023200161207          // Nell'oggetto imposto il primo codice cliente richiesto
023300161207  2     //if  oggcli>0   ;
023400161207        //chain (1:dutkci:oggcli) cnaco00f  ;
023500161207  3     //if not %found(cnaco00f)  ;
023600161207        //  acorag=*all'?'  ;
023700161207  3     //endif  ;
023800111202
023900161207        //  pinEMLVAR='*OBJM* Anomalie Cliente: '+%editc(oggcli:'X')+ '-'+
024000161207        //             %subst(acorag:1:15)  ;
024100161207  x2    //else  ;
024200111202          // Imposto la prima lnp/serie richiesta   ;
024300161207        // pinEMLVAR='*OBJM* Anomalie Lnp/Ser:'+%editc(lnp(1):'X')+' '+
024400161207        //                               nrs(1)      ;
024500161207  2     //endif  ;
024600111206
024700161207        //  pInEMLDEST=%trim(videmail)+'@brt.it' ;
024800161102        //callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
024900161102        //                pInEMLVAR:pInMRAKE2:pOutESITO) ;
025000170609
025100170609     C*
025200170609     C* Chiudo il file di output
025300170609     C                   if        %open(tivgd00f)
025400170609     C                   close     tivgd00f
025500170609     C                   endif
025600170609     C*
025700170609     C* Finalizzo la transazione
025800170609     C                   EVAL      i§7VASOPZ  = 'RLS'
025900170609     C                   EVAL      i§7VASTIP  = '&1'
026000170609     C                   EVAL      i§7VASKSU  = '0BRT0'+%editc(dutpou:'X')
026100170609     C                   EVAL      i§7VASTSC  = 'WW'
026200170609     C                   EVAL      i§7VASSTO  = '?'
026300170609     C                   EVAL      i§7VASSTTO = 'L'
026400170609     C                   EVAL      i§7VASPRG  = o§7VASPRG
026500170609     C                   CALL(e)   'TIS7VASR1'
026600170609     C                   PARM                    TIS7VASDS
026700170609     C*
026800170609     C* Verifico esito chiamata
026900170609     C                   if        %error OR o§7VASOK = *off
027000170609     C                   exsr      EXEERR
027100170609     C                   endif
027200170609     C*
027300170609     C                   endif
027400080829
027500161214        except fine   ;
027600161214
027700080317        *inlr ='1'   ;
027800170609
027900170609
028000170609
028100170609     C*------------------------------------------------------------------------*
028200170609     C* EXEERR - Routine di esecuzione azioni su Errore
028300170609     C*------------------------------------------------------------------------*
028400170609     C     EXEERR        BEGSR
028500170609     C*
028600170609     C                   dump(A)
028700170609     C                   rolbk(e)
028800170609     C*
028900170609     C                   ENDSR
029000170609     C*------------------------------------------------------------------------*
029100170609
029200170609
029300080828
029400110913       //-------------------------------------------------------------------
029500161102        BEGSR   Ctrsel  ;
029600161102
029700161102        Wok='N'  ;
029800161102        Wfine=' '  ;
029900161209        Wnic =' '  ;
030000161216        clear wesARB  ;
030100161227        clear evemem  ;
030200161227        clear evesalta;
030300161227        clear conta   ;
030400161212
030500161212        // Escludo le bolle con CTM 4 e 4F
030600161212        if arbctm ='4 ' or arbctm='4F' ;
030700161212          WOK='N'  ;
030800161212          leavesr  ;
030900161212        endif  ;
031000161216
031100161216        // Escludo le bolle consegnate o non consegnate se richiesto
031200161216        //  verifico la data consegna da ARB se non è dirottata
031300161216        if vidcons<>' '  and arbcca<>'1'  ;
031400161216        chain  (arbaas:arblnp:arbnrs:arbnsp)   fnarb01l     ;
031500161216          if %found(fnarb01l)  ;
031600161216           wesARB='S'  ;
031700161216          else  ;
031800161216           wesARB='N'  ;
031900161216          endif  ;
032000161216        endif  ;
032100161216
032200161216        if wesarb<>'S'   ;
032300161216        if vidcons='S' and arbdcm=0 or vidcons='N' and arbdcm>0 ;
032400161216          WOK='N'  ;
032500161216          leavesr  ;
032600161216        endif  ;
032700161216        else  ;
032800161216        if vidcons='S' and a_arbdcm=0 or vidcons='N' and a_arbdcm>0 ;
032900161216          WOK='N'  ;
033000161216          leavesr  ;
033100161216        endif  ;
033200161216        endif  ;
033300161212
033400161212        // escludo le bolle "figlie"
033500161212        chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl01l  ;
033600161212        if %found(fnlbl01l)  ;
033700161212          WOK='N'  ;
033800161212          leavesr  ;
033900161212        endif  ;
034000161102
0341001611021       if (arbaas*10000)+arbmgs>comdsa ;
034200161102        Wfine='S'  ;
034300161102        leavesr ;
0344001611021       endif  ;
034500161102
034600161102        // controllo cliente e serie
0347001611021       if arbccm>0  ;
034800161102         wccm= arbccm  ;
034900161102        else  ;
035000161102         wccm=arbksc  ;
0351001611021       endif  ;
035200161102
035300161102        xx= 1 ;
035400161212        clear wabi  ;
035500161102
035600161102 3      dow xx<= 20  ;
035700161102 4      if lnp(xx) >0  ;
035800161212 4a     if lnp(xx) = arblnp  ;
035900161102
036000161212        if cli(xx)>0 and wccm<>cli(xx)  ;
036100161212        chain (cli(xx):wccm) fiabk01l  ;
036200161212          if %found(fiabk01l) ;
036300161212           eval wabi='S'  ;
036400161213       //   else   ;
036500161213       //     chain (wccm) fiabk02l  ;
036600161213       //     if %found(fiabk02l) ;
036700161213       //      eval wabi='S'  ;
036800161213       //     endif  ;
036900161212          endif  ;
037000161212        endif  ;
037100161212
037200161212 5      if cli(XX)=0 or wccm= cli(xx) or wabi='S'         ;
037300161102
037400161102        if   nrs(xx)='99'   ;
037500161102         WOK='S'  ;
037600161102          leavesr  ;
037700161102        endif   ;
037800161102
037900161102 6      if   nrs(XX)='SI' and arbnrs>0    ;
038000161102         WOK='S'  ;
038100161102          leavesr  ;
038200161102 x6     else  ;
038300161102
038400161102        alfanrs=%editc(arbnrs:'X')  ;
038500161102 7       if alfanrs= nrs(xx)  ;
038600161102         WOK='S'  ;
038700161102          leavesr  ;
038800161102 7       endif  ;
038900161102 6      endif  ;
039000161102
039100161102 5      endif  ;
039200161102 4      endif  ;
039300161212 4a     endif  ;
039400161102
039500161102        xx=xx+1  ;
039600161102 3      enddo  ;
039700161102
039800161102
039900161102
040000161102        ENDSR  ;
040100161102       //-------------------------------------------------------------------
040200161102       BEGSR  Elabora                ;
040300161207
040400161209       ex88=' '   ;
040500161209       clear wdse;
040600161209       clear                   DISfgs     ;
040700161209       clear                   DISdata    ;
040800161209       clear                   DISnpg     ;
040900161209       clear                   DISora     ;
041000161209       clear                   IMPdata    ;
041100161209       clear                   IMPfgs     ;
041200161209       clear                   IDnpg      ;
041300161209       clear                   IDdata     ;
041400161209       clear                   IDora      ;
041500161209       clear                   IDfgs      ;
041600161209       clear evedt    ;
041700161209       clear evedes   ;
041800161209       clear evedim   ;
041900161209       clear evehim   ;
042000161209
042100161207       clear wfkpi000  ;
042200170525
042300170525       // Lettura eventi
042400170525       exsr leggi_eve  ;
042500161207
042600161209       // calcolo ore reali di consegna tra data sped e data consegna
042700161207       exsr cal_gg    ;
042800161212
042900161216
043000161212       // Se le bolle OK non le voglio --> salto
0431001612120      if vidok <>'N' or  kpidly_c>0  ;
043200161207
043300161212       // se si vogliono gli eventi anche bolle ok elaboro
0434001612120a     if kpidly_c > 0  or vidokeve<>'N' ;
043500161102
043600161102       setll (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
043700161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0438001611021      dow  not %eof(fnblt01l)  ;
043900161214       clear memdse  ;
044000161102
044100161214       // verifico se presente una spunta 88/90
0442001612142      if bltdse>arbdbr or bltdse=0 or bltnpp=88 or bltnpp=90  ;
044300161102       setll  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
044400161102       reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
0445001611023      dow not %eof(fnbrv09l)  ;
044600161214
044700161214       // per testimoniare la presenza del collo metto qualsiasi data
044800161214       // entrata, disguido, arrivo. Quindi tratto la pistola 88 e 90
044900161214       //  allo stesso modo
045000161214
0451001612144       if brvnps=88 or brvnps=90 ;
045200161102         ex88='S'   ;
0453001612145        if  bltdse>0  ;
045400161214           memdse=bltdse;
045500161214x5       else  ;
045600161214          chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0457001612146         if %found(fnart01l) ;
0458001612147         if artdet>0 and artdet<artdam ;
045900161214           memdse=artdet;
046000161214x7        else   ;
0461001612148         if artdam>0  ;
046200161214           memdse=artdam;
0463001612148         endif  ;
0464001612147         endif  ;
0465001612146         endif  ;
0466001612145         endif  ;
046700161214
046800161214 4       endif  ;
046900161214
047000161214        if   memdse>0  ;
047100161214         leave ;
047200161214        endif  ;
047300161214
0474001612144       //if brvnps=90 ;
047500161214        //  chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0476001612145       // if artdam>bltdse  ;
047700161214        //  ex88='S'   ;
047800161214        //  wdse=bltdse;
047900161214        //  leave  ;
0480001612145       // endif  ;
0481001612144       //endif  ;
048200161103
048300161102
0484001611023      reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
048500161102       enddo  ;
0486001611022      endif   ;
048700161209
048800161214       // se era spunta in 88 memorizzo la più recente come data ;
048900161214       if memdse>wdse  ;
049000161214         wdse=memdse  ;
049100161214       endif  ;
049200161214
049300161214       // Verifico se in IMP/Disguido ne basta uno per bolla
049400161214       if idnpg=' '  ;
049500161214         exsr IMPDISG  ;
049600161214       endif  ;
049700161102
049800161102
049900161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0500001611021      enddo  ;
050100161102
050200161209       // verifico se la trazione è ritardo , usando la data arrivo ult collo
050300161216       if wesARB='S'         ;
050400161103       endif  ;
050500161209
0506001612270a     endif  ;
050700161103
050800161102       exsr   Scrivifile  ;
0509001612120      endif  ;
051000161212
051100161102       ENDSR   ;
051200161209       //-------------------------------------------------------------------
051300161209       BEGSR  Leggi_eve              ;
051400161209
051500161209       // Lettura eventi che "servono"
051600161209       clear YY  ;
051700161227       clear indx;
051800161209
051900161209       setll  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
052000161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
052100161209  1    dow  not %eof(fnevb01l)   ;
052200161209
052300161209  2    if %lookup(evbcev:cev)>0   ;
052400161209
052500161227       // Se si tratta di MIC, memorizzo quello con data e ora più alta
052600161227  3    if evbcev='MIC'   ;
052700161227       indx=%lookup(evbcev:evedes)  ;
052800161227  4    if indx>0 ;
052900161227        clear evedt(indx)   ;
053000161227        clear evedes(indx)   ;
053100161227        clear evedim(indx)   ;
053200161227        clear evehim(indx)   ;
053300161227  4    endif  ;
053400161227  3    endif;
053500161227
053600161209       yy=yy+1  ;
053700161227
053800161227  3    if yy<32   ;
053900161209
054000161209        evedt(yy)=evbdev   ;
054100161209        evedes(yy)=evbcev   ;
054200161209        evedim(yy)=evbdtv   ;
054300161209        evehim(yy)=evborv   ;
054400161209
054500161209       // e si tratta di NIC memorizzo anche data e ora immissione
054600161209       // lo elimino se trovo un MIC successivo
054700161209  4     if evbcev='NIC' ;
054800161209         eval wnic='S'   ;
054900161209  4     endif  ;
055000161209  3     endif  ;
055100161209
055200161227  2     endif  ;
055300161209
055400161209        // solo se presente NIC verifico se da eliminare
055500161209   3      if wnic='S' and evbcev='MIC'    ;
055600161209
055700161227        ee=1   ;
055800161227   4    dow ee<=32  ;
055900161227   5    if evedes(ee)='NIC' ;
056000161209
056100161209   6    select  ;
056200161227        when evedt(ee)<>evbdev ;
056300161209
056400161227        when evedim(ee)<evbdtv ;
056500161227        clear evedt(ee)   ;
056600161227        clear evedes(ee)   ;
056700161227        clear evedim(ee)   ;
056800161227        clear evehim(ee)   ;
056900161227        wnic=' '   ;
057000161209
057100161227        when evedim(ee)=evbdtv and evehim(ee)<evborv ;
057200161227        clear evedt(ee)   ;
057300161227        clear evedes(ee)   ;
057400161227        clear evedim(ee)   ;
057500161227        clear evehim(ee)   ;
057600161227        wnic=' '   ;
057700161209        endsl  ;
057800161209
057900161209   5    endif   ;
058000161209
058100161227        ee=ee+1  ;
058200161209   4    enddo  ;
058300161209
058400161209   3      endif  ;
058500161209
058600161209
058700161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
058800161209   1   enddo  ;
058900161209
059000161227       // il MIC lo tengo solo se è l'ultimo degli eventi altrimenti
059100161227       // lo emimino
059200161227       indx=%lookup('MIC':evedes)  ;
059300161227  1    if indx>0 ;
059400161227       yy  =indx+1  ;
059500161227  2    dow yy<=32   ;
059600161227  3     if  evedes(yy)<>*blanks   ;
059700161227         clear evedt(indx)   ;
059800161227         clear evedes(indx)   ;
059900161227         clear evedim(indx)   ;
060000161227         clear evehim(indx)   ;
060100161227         leave   ;
060200161227  3     endif  ;
060300161227
060400161227       yy=yy+1  ;
060500161227  2    enddo  ;
060600161227  1    endif  ;
060700161227
060800161227       // conto anche quanti eventi ci sono per memorizzare solo gli
060900161227       // ultimi 10
061000161227       yy=1  ;
061100161227       clear conta   ;
061200161227       dow yy<=32                ;
061300161227         if  evedes(yy)<>*blanks   ;
061400161227          conta=conta+1  ;
061500161227         endif  ;
061600161227       yy=yy+1  ;
061700161227       enddo  ;
061800161227
061900161227       clear   evesalta  ;
062000161227       if conta>10  ;
062100161227       evesalta=conta-10  ;
062200161227       endif   ;
062300161227
062400161209       ENDSR   ;
062500161102       //-------------------------------------------------------------------
062600161102       BEGSR  Scrivifile             ;
062700111116
062800110913
062900161102       kpiidute=knmeb   ;
063000161102       kpiepo=dutpou  ;
063100110913
063200161102       kpiccm=wccm    ;
063300161102       kpirsm=arbrsm  ;
063400161102
063500161102       kpiaas=arbaas  ;
063600161102       kpilnp=arblnp  ;
063700161102       kpimes=%int(%subst(%editc(arbmgs:'X'):1:2))  ;
063800161102       kpirmn=arbrmn  ;
063900161102       kpirma=arbrma  ;
064000161102       kpinrs=arbnrs  ;
064100161102       kpinsp=arbnsp  ;
064200161102       kpimgs=arbmgs  ;
064300161102       kpilna=arblna  ;
064400161102       kpirsd=arbrsd  ;
064500161102       kpilod=arblod  ;
064600161102       kpiprd=arbprd  ;
064700161102       kpicad=arbcad  ;
064800161215       kpifin=arbfin  ;
064900161102       kpinzd=arbnzd  ;
065000161102       kpiffd=arbffd   ;
065100161102       kpitc1=arbtc1   ;
065200161102       kpitc2=arbtc2   ;
065300161102       kpigc1=arbgc1   ;
065400161102       kpigc2=arbgc2   ;
065500161102       kpincl=arbncl   ;
065600161102       kpipkb=arbpkb   ;
065700161102       kpivlb=arbvlb   ;
065800161102       kpitcr=arbtcr   ;
065900161102       kpidcr=arbdcr   ;
066000161102       kpidcp=arbdcp   ;
066100161102       kpidcm=arbdcm   ;
066200161102       kpihmc=arbhmc   ;
066300161209       kpicca=arbcca   ;
066400161209
066500161216       if arbdcm=0 and wesarb='S' and a_arbdcm>0 and arbcca<>'1';
066600161209       kpidcm=a_arbdcm   ;
066700161209       kpihmc=a_arbhmc   ;
066800161209       kpicca=a_arbcca   ;
066900161209       endif  ;
067000161209
067100161209       if kpicca<>' '  ;
067200161209       tblkey=kpicca   ;
067300161209       chain (1:'7O':tblkey) tabel  ;
067400161209         if %found(tabel00f )  ;
067500161209          kpicca_des=tbluni   ;
067600161209         endif  ;
067700161209       endif  ;
067800161103
067900161103        // pistola ex 88
068000161102        if ex88='S'     ;
068100161102        kpip88='S'      ;
068200161103        kpidse=wdse     ;
068300161102        endif           ;
068400161103
068500161103        //IMP disguido
068600161103        if IDnpg<>' '   ;
068700161103           if IDnpg='P'   ;
068800161103           kpiimpds='P'   ;
068900161103           else  ;
069000161103           kpiimpds='D'   ;
069100161103           endif ;
069200161103        endif ;
069300161209
069400161209       yy=1  ;
069500161227       clear evemem   ;
069600161209
069700170525 1     dow yy<=32               ;
069800161227 2     if  evedes(yy)<>*blanks   ;
069900170524
070000170524       // Verifico se data evento uguale alla data prevista consegna
070100170524 2a    if kpidce>0 ;
070200170524
070300170525 3     if evedt(yy)<=kpidce and kpidcesn<>'S'  ;
070400170524          indx= %lookup(evedes(yy):cev) ;
070500170524 4     if cevt(indx)='E';
070600170524        kpidcesn='S'  ;
070700170524 4     ENDIF;
070800170524       // memorizzo diversamente se c'è un evento particolare
070900170525 4     if cevt(indx)='P' and  evedt(yy) =kpidce ;
071000170524        kpidcesn='P'  ;
071100170524 4     ENDIF;
071200170524
071300170524 3     ENDIF;
071400170524 2a    ENDIF;
071500161227
071600161227       evemem=evemem+1  ;
071700161227
071800161227       // se conta < 10 memorizzo tutti altrimenti salto i primi
071900161227
072000170525 2a    if kpidly_c > 0  or vidokeve<>'N' ;
072100161227 3     if conta <=10 or evemem>evesalta  ;
072200161227 4      select   ;
072300161227         when KPICEV1=*blanks and evedes(yy)<>'MIC';
072400161209         KPICEV1=evedes(yy)  ;
072500161209         KPIDEV1=evedt(yy)  ;
072600161227         when KPICEV2=*blanks and evedes(yy)<>'MIC';
072700161209         KPICEV2=evedes(yy)  ;
072800161209         KPIDEV2=evedt(yy)  ;
072900161227         when KPICEV3=*blanks and evedes(yy)<>'MIC';
073000161209         KPICEV3=evedes(yy)  ;
073100161209         KPIDEV3=evedt(yy)  ;
073200161227         when KPICEV4=*blanks and evedes(yy)<>'MIC';
073300161209         KPICEV4=evedes(yy)  ;
073400161209         KPIDEV4=evedt(yy)  ;
073500161227         when KPICEV5=*blanks  and evedes(yy)<>'MIC';
073600161209         KPICEV5=evedes(yy)  ;
073700161209         KPIDEV5=evedt(yy)  ;
073800161227         when KPICEV6=*blanks  and evedes(yy)<>'MIC';
073900161209         KPICEV6=evedes(yy)  ;
074000161209         KPIDEV6=evedt(yy)  ;
074100161227         when KPICEV7=*blanks  and evedes(yy)<>'MIC';
074200161227         KPICEV7=evedes(yy)  ;
074300161209         KPIDEV7=evedt(yy)  ;
074400161227         when KPICEV8=*blanks  and evedes(yy)<>'MIC';
074500161209         KPICEV8=evedes(yy)  ;
074600161209         KPIDEV8=evedt(yy)  ;
074700161227         when KPICEV9=*blanks  and evedes(yy)<>'MIC';
074800161209         KPICEV9=evedes(yy)  ;
074900161209         KPIDEV9=evedt(yy)  ;
075000161209         when KPICEV0=*blanks  ;
075100161209         KPICEV0=evedes(yy)  ;
075200161209         KPIDEV0=evedt(yy)  ;
075300161227 4       endsl  ;
075400161227 3       endif  ;
075500161227 2       endif  ;
075600170525 2a      endif  ;
075700161209
075800161209         yy=yy+ 1;
075900161209
076000161227 1     enddo  ;
076100170523
076200170523        // Se la data consegna prevista corrisponde alla data cons merce--> ok
076300170526 1      if kpidcm>0 and kpidce>0 and kpidcesn<>'S' ;
076400170526        // utilizzo la data  di consegna merce di arb se esiste per le bolle
076500170526        //  legate
076600170526
076700170526 2        if wesarb='S' and a_arbdcm<=kpidce ;
076800170526            kpidcesn='S' ;
076900170526 x2       else  ;
077000170526 3        if kpidcm<=kpidce ;
077100170523            kpidcesn='S' ;
077200170526 3        ENDIF;
077300170526 2      ENDIF;
077400170526 1      ENDIF;
077500170526 1          if kpidcesn=' '  and kpidce>0 ;
077600170525            kpidcesn='N' ;
077700170526 1          ENDIF;
077800110916
077900161102       write  wfkpi000 ;
078000170609       exsr   WRITIVGD ;
078100110913
078200161214       CONTAB=CONTAB+1   ;
078300110926
078400110913       ENDSR                  ;
078500170609
078600170609
078700170609
078800170609     C*------------------------------------------------------------------------*
078900170609     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
079000170609     C*------------------------------------------------------------------------*
079100170609     C     WRITIVGD      BEGSR
079200170609     C*
079300170609     C                   clear                   tivgd000
079400170609     C                   eval      vgdDTA = %subst(DS_KPI:1:%size(DS_KPI))
079500170609     C                   eval      vgdTIP = '&1'
079600170609     C                   eval      vgdKSU = '0BRT0'+%editc(dutpou:'X')
079700170609     C                   eval      vgdTSC = 'WW'
079800170609     C                   eval      vgdDAT = dateu
079900170609     C                   eval      vgdPRG = o§7VASPRG
080000170609     C                   eval      vgdPGM = procname
080100170609     C                   eval      vgdSTO = '?'
080200170609     C                   write     tivgd000
080300170609     C*
080400170609     C                   ENDSR
080500170609     C*------------------------------------------------------------------------*
080600170609
080700170609
080800170609
080900080327       //-------------------------------------------------------------------
081000080327       BEGSR  *INZSR                 ;
081100080328
081200080328       in(E) *dtaara   ;
081300080328       if    %error or rsut  =*blanks  ;
081400080328       callp TIBS34R (TIBS34DS)        ;
081500080328       in    *dtaara   ;
081600080328       endif                           ;
081700080829
081800110613       // imposto data e ora del giorno
081900110613       dataiso=datasys  ;
082000161214       dataeur=datasys  ;
082100110613       dateu=%dec(dataiso)   ;
082200161214       dataogg=%dec(dataeur)   ;
082300110616
082400110613       ora_iso=ora_sys  ;
082500110613       ora_u=%dec(ora_iso)   ;
082600080829
082700110913       // Se richiesto file pulisco il lancio precedente per filiale
082800161102       setll (dutpou)   wfKPI01l   ;
082900161102       reade (dutpou)   wfKPI01l   ;
083000110616
083100161102       dow  not %eof(wfKPI01l)   ;
083200161102       delete wfKPI000  ;
083300110616
083400161102       reade (dutpou)   wfKPI01l   ;
083500110616       enddo  ;
083600110616
083700161209       clear xx ;
083800161209       setll  (1:'2A') tabel00f ;
083900161209       reade  (1:'2A') tabel00f ;
084000161209
084100161209       dow not %eof(tabel00f ) ;
084200161209       if tblflg=' '  ;
084300161209        ds2a = tbluni   ;
084400161209
084500161209       if   §2aftg='S' or §2atip='N' or §2aftc='A'  ;
084600161209       xx=xx+1  ;
084700161209        cev(xx)=%Subst(tblkey:1:3)  ;
084800170524        if §2antc='S' ;
084900170524          cevt(xx)='E' ;
085000170524        ENDIF;
085100161209       endif  ;
085200161209       endif  ;
085300161209
085400161209       reade  (1:'2A') tabel00f ;
085500161209       enddo  ;
085600161209
085700161209       setll  (1:'2Z') tabel00f ;
085800161209       reade  (1:'2Z') tabel00f ;
085900161209
086000161209       dow not %eof(tabel00f ) ;
086100161209       if tblflg=' '  ;
086200161209        ds2z = tbluni   ;
086300161209
086400161209       if   §2zcep<>' '  ;
086500161209       xx=xx+1  ;
086600161209        cev(xx)=%subst(tblkey:1:3)  ;
086700170523        cevt(xx)='P' ;
086800161209       endif  ;
086900161209       endif  ;
087000161209
087100161209       reade  (1:'2Z') tabel00f ;
087200161209       enddo  ;
087300161209
087400161209       // infine aggiungo fisso il NIC
087500161209       xx=xx+1  ;
087600161209       cev(xx)='NIC' ;
087700161227       xx=xx+1  ;
087800161227       cev(xx)='MIC' ;
087900161209
088000161209       ENDSR     ;
088100161102       //-------------------------------------------------------------------
088200161209       BEGSR  PrepaLNP               ;
088300161102
088400161102       clear  LnpSel  ;
088500161102       xx= 1 ;
088600161102       yy= 1 ;
088700161102
088800161102       dow xx <=20  ;
088900161102        if lnp(xx)>0  ;
089000161102         Indx=%lookup(lnp(xx):LnpSel)  ;
089100161102         if Indx=0  ;
089200161102           eval lnpSel(yy)=lnp(xx)  ;
089300161102           yy=yy+1   ;
089400161102         endif  ;
089500161102
089600161102        endif  ;
089700161102
089800161102        xx=xx+1   ;
089900161102        enddo     ;
090000161102
090100161102        // Memorizzo nel campo oggcli il codice cliente che devo
090200161102        //  mettere nell'ogggetto della mail
090300161212        // clear oggcli  ;
090400161212        // if cli(1)>0   ;
090500161212        //   oggcli=cli(1)  ;
090600161212        // endif  ;
090700161102
090800161102       ENDSR  ;
090900161207
091000161103
091100161103      /end-free
091200161103
091300161207     c*-------------------------------------------------------------------
091400161207     c     Cal_gg        BEGSR
091500161207     c
091600161209     c     karb          klist
091700161209     c                   kfld                    arbaas
091800161209     c                   kfld                    arblnp
091900161209     c                   kfld                    arbnrs
092000161209     c                   kfld                    arbnsp
092100170524     c     kar5          klist
092200170524     c                   kfld                    arbaas
092300170524     c                   kfld                    arblnp
092400170524     c                   kfld                    arbnrs
092500170524     c                   kfld                    arbnsp
092600170524     c                   kfld                    ktrd
092700161209
092800161216     c* chain solo se non l'ho già fatto prima
092900161216     c                   if        wesarb=' '
093000161209     c     karb          chain     fnarb01l
093100170105     c                   if        %found(fnarb01l)
093200170105     c                   eval      wesARB='S'
093300170105     c                   else
093400170105     c                   eval      wesARB='N'
093500170105     c                   endif
093600161216     c                   endif
093700170524
093800170524     c     kar5          chain     fiar501l
093900170524     c                   if        %found(fiar501l)
094000170524     c                   eval      dar5gen=ar5uni
094100170524     c                   else
094200170524     c
094300170524     c                   clear                   dar5gen
094400170524     c                   endif
094500170525
094600170525     c* prendo il primo evento pieno "NO MIC/NIC"
094700170525     c                   clear                   dtprimoeve
094800170525     c                   eval      yy=1
094900170525     c                   dow       yy<=10 and dtprimoeve=0
095000170525     c                   if        evedes(yy)<>*blanks and
095100170525     c                             evedes(yy)<>'MIC' and
095200170525     c                             evedes(yy)<>'NIC'
095300170525     c                   EVAL      DTPRIMOEVE=EVEDT(YY)
095400170525     c                   ENDIF
095500170525     c                   ADD       1             YY
095600170525     c                   ENDDO
095700170525
095800161216
095900161207     c                   clear                   tnsd99ds
096000161207     C                   MOVEL     ' '           D98TLA
096100161216     c                   if        wesARB='S'
096200161207     C                   MOVEL     'A'           D98TBO
096300161209     c                   else
096400161209     C                   MOVEL     'P'           D98TBO
096500161209     c                   endif
096600161207     C                   MOVEL     ARBAAS        D98AAS
096700161207     C                   MOVEL     ARBLNP        D98LNP
096800161207     C                   MOVEL     ARBNRS        D98NRS
096900161207     C                   MOVEL     ARBNSP        D98NSP
097000161207     c
097100161207     c                   if        arbtsp='D'
097200161207     c                   eval      I98TSP_FOR='C'
097300161207     c                   endif
097400170524     c
097500161207     C*
097600161207     C                   CALL      'TNSD99R'
097700161207     C                   PARM                    TNSD99DS
097800161207     c
097900161207     c* ore teoriche
098000161209     c                   eval      KPIdly_T=d98trc
098100161209     c                   eval      KPIdag  =d98dag
098200161209     c                   eval      KPIdcg  =d98dcg
098300161215     c                   eval      KPIdac  =d98dac
098400161215     c                   eval      KPIdcc  =d98dcc
098500161207
098600161207     c* Richiamo il pgm utilizzando la DS eliminando il num sped
098700161207     c*  e la giacenza
098800161207     c                   clear                   d98nsp
098900161207     c                   clear                   d98dti
099000161207     c                   clear                   d98hti
099100161207     c                   clear                   d98cmc
099200161207     c                   clear                   d98dag
099300161207     c                   clear                   d98dcg
099400161207     c                   eval      d98duc=d98dsp
099500161207     c                   eval      d98dbr=d98dsp
099600170105     c                   eval      D98FDRT='S'
099700161207     c                   clear                   d98dam
099800170524     c* considero i giorni di chiusura se inseriti in partenza
099900170524     c                   if        §ar5mdp='S'
100000170524     c                   eval      d98gc1=§AR5GC1
100100170524     c                   eval      d98gc2=§AR5GC2
100200170524     c                   else
100300170524     c                   clear                   d98gc1
100400170524     c                   clear                   d98gc2
100500170524     c                   endif
100600170524
100700170524     c* la consegna richiesta la considero se inserita in partenza
100800170524     c*  o in arrivo prima del primo tentativo
100900170524     c                   if        d98dcr>0 and (§ar5mdp='S' and
101000170524     c                             §ar5dcr<>%editc(d98dcr:'X'))
101100170525     c**                   clear                   datadcr
101200170525     c**     karb          setll     fiarbf2c
101300170525     c**     karb          reade     fiarbf2c
101400170525     c**                   dow       not %eof(fiarbf2c)
101500170525     c**                   if        %subst(v_arbcvb:1:1)='C'
101600170525     c**                             and v_arbcvb<>'CP'
101700170525     c**                   eval      datadcr=v_arbdtv
101800170525     c**                   leave
101900170525     c**                   endif
102000170524     c
102100170525     c**     karb          reade     fiarbf2c
102200170525     c**                   enddo
102300170525     c*
102400170525     c                   if        d98dcr>dtprimoeve
102500170524     c                   clear                   d98dcr
102600170524     c                   clear                   d98hcr
102700170524     c                   clear                   d98tcr
102800170524     c                   endif
102900170524     c                   endif
103000170525     c* Se il fermo deposito è stato inserito dopo il primo tentativo
103100170525     c*  NON lo considero
103200170525     c                   if        d98ffd='S'   and
103300170525     c                             dtprimoeve>0  and
103400170525     c                             §ar5dfd>*zeros and
103500170525     c                             %int(§ar5dfd)>=dtprimoeve
103600170525     c                   clear                   d98ffd
103700170525     c                   endif
103800170525     c
103900170524     c
104000161207     C                   CALL      'TNSD99R'
104100161207     C                   PARM                    TNSD99DS
104200161207     c
104300161207     c* controllo le ore reali  con le ore teoriche
104400161209     c                   eval      KPIdly_R=d98rrc
104500170523     c* Memorizzo la data prevista consegna
104600170523     c                   eval      KPIdce= d98dce
104700170524     c* Se il fermo deposito è stato inserito dopo il primo tentativo
104800170524     c*  NON lo considero
104900170524     c                   if         d98ffd='S'
105000170525     c                   if        dtprimoeve=0  or
105100170524     c                             §ar5dfd<=*zeros or
105200170525     c                             %int(§ar5dfd)< dtprimoeve
105300170524     c                   clear                   kpidce
105400170524     c                   endif
105500170524     c                   endif
105600161215     c
105700170523     c* se la spedizione è borderizzata in data festiva --> aggiungo 24 ore
105800161215     c*  alle ore reali
105900170105     c* ES 05/1/17 non lo faccio più in quanto  ho modificato allo stesso modo
106000170105     c*  il calcolo della affidabilità tnsd99r
106100170105     c**                 if        arbdbr>0
106200170105     c**                 clear                   wdat8
106300170105     c**                 clear                   tfptfa
106400170105     c**                 z-add     arbdbr        datada
106500170105     c*+                 z-add     arbdbr        dataa
106600170105     c**                 z-add     arbtfp        p_tfp
106700170105     c*+                 z-add     arbtfa        p_tfa
106800170105     c**                 call      'XSRLAV8'
106900170105     c**                 parm                    wdat8
107000170105     c**                 parm                    tfptfa
107100161215     c* se giorno FESTIVO aggiungo 24 ore
107200170105     c**                 if        ggl=0
107300170105     c**                 eval      KPIdly_R=KPIdly_R+24
107400170105     c**                 endif
107500161215
107600170105     c**                 endif
107700161215     c
107800161207     c                   if        kpidly_r<=kpidly_t
107900161207     c                   clear                   kpidly_c
108000161207     c                   else
108100161207     c
108200161207     c                   eval      kpidly_c =kpidly_r-kpidly_t
108300161207     c                   endif
108400161207     c*
108500161213     c* CONSEGNA RICHIESTA IL
108600161212     c* Se data richiesta = data consegna merce
108700161209     c*  sempre OK
108800161212     c**                 if        arbtc1='A' or arbtc2='A'  or
108900161212     c**                           arbtc1='S' or arbtc2='S'
109000161213     c                   if        arbdcr>0 and arbtcr=' '
109100161213     c                   if        arbdcr=arbdcm
109200161209     c                   clear                   kpidly_c
109300161209     c                   endif
109400161213     c* se bolla NON consegnata e data cons richiesta > di oggi
109500161213     c* per ora ON TIME
109600161213     c                   if        arbdcm=0 and arbdcr>=dateu
109700161213     c                   clear                   kpidly_c
109800161213     c                   endif
109900161213     c                   endif
110000161213     c
110100161212     c**                 endif
110200161213
110300161213     c* CONSEGNA RICHIESTA PRIMA DEL
110400161213     c                   if        arbdcr>0 and arbtcr='P'
110500161213     C                   IF        ARBDCM>0 AND ARBDCM<ARBDCR
110600161213     c                   clear                   kpidly_c
110700161213     c                   endif
110800161213     C                   IF        ARBDCM=0
110900161213     c                   clear                   kpidly_c
111000161213     c                   endif
111100161213     c                   endif
111200161213     c* CONSEGNA RICHIESTA DOPO IL
111300161213     c                   if        arbdcr>0 and arbtcr='D'
111400161213     C                   IF        ARBDCM>0 AND ARBDCM>ARBDCR
111500161213     c                   clear                   kpidly_c
111600161213     c                   endif
111700161213     C                   IF        ARBDCM=0
111800161213     c                   clear                   kpidly_c
111900161213     c                   endif
112000161213     c                   endif
112100161207
112200161212     c* se cons anomala RESO o DIROTTAMENTO --> sempre ok
112300161212     c                   if        arbcca='1' or arbcca='2' or
112400161212     c                             arbcca='6'
112500161212     c                   clear                   kpidly_c
112600161212     c                   endif
112700161207     c*
112800161207     c                   ENDSR
112900161103     c*-------------------------------------------------------------------
113000161103     c     IMPdisg       BEGSR
113100161209     c
113200161209     c     kbrv          Klist
113300161209     c                   Kfld                    bltfls
113400161209     c                   Kfld                    bltlna
113500161209     c                   Kfld                    bltnrs
113600161209     c                   Kfld                    bltnsc
113700161103     c
113800161103     c* per ogni collo verifico se in IMP o disguidato
113900161103     c* prendo la data più alta
114000161103     c     kbrv          setll     fnbrv07l
114100161103     c     kbrv          reade     fnbrv07l
114200161103    2c                   dow       not %eof(fnbrv07l)
114300161103    3c                   if        brvatr='D'  or brvnpg=3
114400161103     c* prendo la data della spunta
114500161103     c                   clear                   fnlv53ds
114600161103     C                   MOVEL     BRvNPG        D53NPG
114700161103     C                   Z-ADD     BRvNFV        D53NFV
114800161103     C                   MOVEL     BRvFGS        D53FGS
114900161103     C                   CALL      'FNLV53R'
115000161103     C                   PARM                    fnlv53ds
115100161103    4c                   if        d53err<>*blanks
115200161103    5c                   if        brvdcs>0
115300161103     c                   eval      d53dfv=brvdcs
115400161103     c                   else
115500161103    6c                   if        brvdfs>0
115600161103     c                   eval      d53dfv=brvdfs
115700161103    6c                   endif
115800161103    5c                   endif
115900161103    4c                   endif
116000161103     c
116100161103    4c                   select
116200161103     c* se si tratta di DISGUIDO
116300161103     c                   when      brvatr='D'
116400161103     c
116500161103    5c                   if        d53dfv=DISdata
116600161103     c* Verifico l'ora
116700161103    6c                   if        brvhcs>0 and brvhcs>DISora
116800161103     c                   eval      DISdata=d53dfv
116900161103     c                   eval      DISfgs =brvfgs
117000161103     c                   eval      DISnpg =brvnpg
117100161103     c                   eval      DISora =brvhcs
117200161103   x6c                   else
117300161103    7c                   if        brvhms>0 and brvhms>DISora
117400161103     c                   eval      DISdata=d53dfv
117500161103     c                   eval      DISfgs =brvfgs
117600161103     c                   eval      DISnpg =brvnpg
117700161103     c                   eval      DISora =brvhms
117800161103    7c                   endif
117900161103    6c                   endif
118000161103    5c                   endif
118100161103     c
118200161103    5c                   if        d53dfv>DISdata
118300161103     c                   eval      DISdata=d53dfv
118400161103     c                   eval      DISfgs =brvfgs
118500161103     c                   eval      DISnpg =brvnpg
118600161103     c                   eval      DISora =brvhcs
118700161103     c                   if        disora=0
118800161103     c                   eval      DISora =brvhms
118900161103     c                   endif
119000161103    5c                   endif
119100161103     c
119200161103     c* se si tratta di IMP
119300161103    4c                   when      d53spg='P' and d53dfv>impdata
119400161103     c                   eval      IMPdata=d53dfv
119500161103     c                   eval      IMPfgs =brvfgs
119600161103    4c                   endsl
119700161103    3c                   endif
119800161103
119900161103     c     kbrv          reade     fnbrv07l
120000161103    2c                   enddo
120100161103
120200161103     c* memorizzo la data più alta
120300161103     c* IMP vince sul disguido per stessa data, senza guardare l'ora
120400161103    2c                   if        IMPdata>0 and IMPdata>=IDdata
120500161103     c                   eval      IDdata=IMPdata
120600161103     c                   eval      IDfgs=IMPfgs
120700161103     c                   eval      IDora=0
120800161103     c                   eval      IDnpg='P'
120900161103    2c                   endif
121000161103    2c                   if        (DISdata>0 and DISdata=IDdata
121100161103     c                             and IDnpg<>'P' and DISora>IDora) or
121200161103     c                             (DISdata>0 and DISdata>IDdata)
121300161103     c                   eval      IDdata=DISdata
121400161103     c                   eval      IDfgs=DISfgs
121500161103     c                   eval      IDora=DISora
121600161103     c                   eval      IDnpg=%char(DISnpg)
121700161103    2c                   endif
121800161103     c
121900161103     C                   ENDSR
122000161214     OQSYSPRT   E            fine             02
122100161214     O                                              'Elaborazione esiti consegn'
122200161214     o                                              'a il'
122300161214     o                       dataogg           +  2 '  /  /    '
122400161214     o                       ora_u             +  2 '  :  :  '
122500161214     o                                         +  2 'Bolle scritte :'
122600161214     o                       contab        2   +  2
122700161214**
122800161214OVRPRTF FILE(QSYSPRT) USRDTA('EsitiCons') HOLD(*YES)
