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
007000171019     D* DA 206 A 225 RICHIESTA UNIFICANTE, NON PRESENTE IN QUESTO PGM
007100161207     d
007200161207     D WDAT8           DS
007300161207     D  DATADA                 1      8  0
007400161207     D  DATAA                  9     16  0
007500161207     D  GGL                   17     21  0
007600161207     d
007700161207     D tfptfa          DS
007800161207     D  p_tfp                  1      3  0
007900161207     D  p_tfa                  4      6  0
008000161207     d
008100161207     D                 DS
008200161207     D  arbaas                 1      4  0
008300161207     D  arbmgs                 5      8  0
008400161207     D  arbdsp                 1      8  0
008500110914     d
008600110914     d
008700110617     D  pInFILE        s             10
008800110617     D  pInIDJOB       s             26
008900110617     D  pInPWDXLS      s             15
009000110617     D  pInNOMXLS      s             78
009100111206     D  pInEMLVAR      s             80
009200110617     D  pInEMLDEST     s            121
009300120504     D  pInMRAKE2      s             15    inz
009400110617     D  pOutESITO      s              1
009500161102     d XX              s              3  0
009600110913     d yy              s              3  0
009700110916     d ee              s              3  0
009800110916     d zz              s              3  0
009900161102     d wccm            s              7  0
010000161102     d dateu           s              8  0
010100161214     d dataogg         s              8  0
010200161102     d wdse            s              8  0
010300161214     d memdse          s              8  0
010400170524     d datadcr         s              8  0
010500161214     d CONTAB          s              9  0 INZ
010600161102     d ora_u           s              6  0
010700161227     d conta           s              3  0
010800161227     d evemem          s              3  0
010900161227     d evesalta        s              3  0
011000161102     d alfanrs         s              2
011100161102     d ex88            s              1
011200161102     d wok             s              1
011300161212     d wabi            s              1
011400161102     d wfine           s              1
011500161216     d wesARB          s              1
011600161102     d Indx            s              3  0
011700161102     d oggcli          s              7  0
011800170525     d dtprimoeve      s              8  0
011900161103     d IMPdata         s                   like(d53dfv)
012000161103     d IMPfgs          s                   like(brvfgs)
012100161103     d DISfgs          s                   like(brvfgs)
012200161103     d DISdata         s                   like(d53dfv)
012300161103     d DISnpg          s                   like(brvnpg)
012400161103     d DISora          s                   like(brvhcs)
012500161103     d IDdata          s                   like(d53dfv)
012600161103     d IDora           s                   like(brvhcs)
012700161103     d IDfgs           s                   like(brvfgs)
012800170524     d ktrd            s                   like(ar5trd) inz('GEN')
012900161103     d IDnpg           s              1
013000161209     d wnic            s              1
013100110613     d wlib            s             10    inz('GAITRAGRPS')
013200110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
013300110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
013400110613     d Ora_iso         s               t   timfmt(*iso)
013500110613     d Dataiso         s               d   datfmt(*iso)
013600111013     d Dataeur         s               d   datfmt(*eur)
013700080327     d Datadmy         s               d   datfmt(*dmy)
013800110916
013900110916     d $MaxElem        c                   const(998)
014000080829     d
014100080317      *------------------------------------------------------------------------*
014200080317      *   P R O T O T I P I
014300080317      *------------------------------------------------------------------------*
014400080829      /copy gaitrasrc/srcprotopr,tibs34r
014500110617      /copy gaitrasrc/srcprotopr,trtcm8r1
014600110916      /copy gaitrasrc/srcprotopr,fnlv55r
014700001027     C*---------------------------------------------------------------*
014800001027     C     *ENTRY        PLIST
014900080325     C                   PARM                    kpjba
015000161031     c                   movel     kpjbu         fnlsw7ds
015100170609     C*
015200170609     C* Avvio il controllo sincronia
015300170609     C                   CLEAR                   TIS7VASDS
015400170609     C                   EVAL      i§7VASOPZ = 'SCM'
015500170609     C                   CALL(e)   'TIS7VASR1'
015600170609     C                   PARM                    TIS7VASDS
015700170609     C*
015800170609     C* Verifico esito chiamata
015900170609     C                   if        %error OR o§7VASOK = *off
016000170609     C                   dump(A)
016100170609     C                   seton                                        lr
016200170609     C                   return
016300170609     C                   endif
016400161214     C* ESEGUO OVRPRTF
016500161214     C                   Z-ADD     61            LUNG             15 5
016600161214     C                   MOVEL     *BLANKS       COMMAN           80
016700161214     C                   MOVEA     CMD           COMMAN
016800161214     C                   CALL      'QCMDEXC'
016900161214     C                   PARM                    COMMAN
017000161214     C                   PARM                    LUNG
017100161214     C*
017200161214     C                   Z-ADD     1             C                 3 0
017300161214     C                   OPEN      QSYSPRT
017400170609     C*
017500170609     C* Apertura file DOWNLOAD
017600170609     C                   if        not %open(tivgd00f)
017700170609     C                   open      tivgd00f
017800170609     C                   endif
017900170609     C*
018000170609     C* Inizializzo la transazione DOWNLOAD
018100170609     C                   CLEAR                   TIS7VASDS
018200170609     C                   EVAL      i§7VASOPZ = 'PRG'
018300170609     C                   CALL(e)   'TIS7VASR1'
018400170609     C                   PARM                    TIS7VASDS
018500170609     C*
018600170609     C* Se OK => proseguo
018700170609     C                   if        not %error AND
018800170609     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
018900170609     C                   movel     'S'           wProcedi          1
019000170609     C                   else
019100170609     C                   movel     'N'           wProcedi
019200170609     C                   endif
019300170609     C*
019400170609     C* Se ok a procedere => elaboro
019500170609     C                   if        wProcedi = 'S'
019600080317      /free
0197001610311      if comdsa=0 ;
019800161031        comdsa=comdsd ;
0199001610311      endif  ;
020000161102
020100161102       // Linee partenza selezionate
020200161102          EXSR  PrepaLNP      ;
020300161212
020400161031
0205001610311      if comdsa=comdsd ;
020600161031         *in01 =*on  ;
0207001610311       endif  ;
020800161102
020900161209       zz=1   ;
021000161031
0211001611021      dow lnpsel(zz)>0  ;
021200161102          setll (lnpsel(zz):kaad:kmgd) fnblp31l ;
021300161031          if *in01=*on  ;
021400161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
021500161031          else  ;
021600161102          reade (lnpsel(zz)) fnblp31l ;
021700161031          endif  ;
021800161031
0219001611022         dow not %eof(fnblp31l)   ;
022000161102
022100161102          exsr   Ctrsel    ;
022200161102
022300161102          if Wfine='S'  ;
022400161102          leave  ;
022500161102          endif  ;
022600161102
022700161102          if wok='S'   ;
022800161102          exsr elabora  ;
022900161102          endif  ;
023000161102
023100161102
023200161102          if *in01=*on  ;
023300161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
023400161102          else  ;
023500161102          reade (lnpsel(zz)) fnblp31l ;
023600161102          endif  ;
0237001611022         enddo  ;
023800161031
023900161102          zz=zz+1  ;
024000161031
0241001611021      enddo ;
024200111116
024300110617        // Se scritto almeno un record, invio all'indirizzo e-mail
024400161207        //pInFILE='WFKPI00F'   ;
024500161207        //pInIDJOB=knmeb       ;
024600161207          // Nell'oggetto imposto il primo codice cliente richiesto
024700161207  2     //if  oggcli>0   ;
024800161207        //chain (1:dutkci:oggcli) cnaco00f  ;
024900161207  3     //if not %found(cnaco00f)  ;
025000161207        //  acorag=*all'?'  ;
025100161207  3     //endif  ;
025200111202
025300161207        //  pinEMLVAR='*OBJM* Anomalie Cliente: '+%editc(oggcli:'X')+ '-'+
025400161207        //             %subst(acorag:1:15)  ;
025500161207  x2    //else  ;
025600111202          // Imposto la prima lnp/serie richiesta   ;
025700161207        // pinEMLVAR='*OBJM* Anomalie Lnp/Ser:'+%editc(lnp(1):'X')+' '+
025800161207        //                               nrs(1)      ;
025900161207  2     //endif  ;
026000111206
026100161207        //  pInEMLDEST=%trim(videmail)+'@brt.it' ;
026200161102        //callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
026300161102        //                pInEMLVAR:pInMRAKE2:pOutESITO) ;
026400170609
026500170609     C*
026600170609     C* Chiudo il file di output
026700170609     C                   if        %open(tivgd00f)
026800170609     C                   close     tivgd00f
026900170609     C                   endif
027000170609     C*
027100170609     C* Finalizzo la transazione
027200170609     C                   EVAL      i§7VASOPZ  = 'RLS'
027300170609     C                   EVAL      i§7VASTIP  = '&1'
027400170609     C                   EVAL      i§7VASKSU  = '0BRT0'+%editc(dutpou:'X')
027500170609     C                   EVAL      i§7VASTSC  = 'WW'
027600170609     C                   EVAL      i§7VASSTO  = '?'
027700170609     C                   EVAL      i§7VASSTTO = 'L'
027800170609     C                   EVAL      i§7VASPRG  = o§7VASPRG
027900170609     C                   CALL(e)   'TIS7VASR1'
028000170609     C                   PARM                    TIS7VASDS
028100170609     C*
028200170609     C* Verifico esito chiamata
028300170609     C                   if        %error OR o§7VASOK = *off
028400170609     C                   exsr      EXEERR
028500170609     C                   endif
028600170609     C*
028700170609     C                   endif
028800080829
028900161214        except fine   ;
029000161214
029100080317        *inlr ='1'   ;
029200170609
029300170609
029400170609
029500170609     C*------------------------------------------------------------------------*
029600170609     C* EXEERR - Routine di esecuzione azioni su Errore
029700170609     C*------------------------------------------------------------------------*
029800170609     C     EXEERR        BEGSR
029900170609     C*
030000170609     C                   dump(A)
030100170609     C                   rolbk(e)
030200170609     C*
030300170609     C                   ENDSR
030400170609     C*------------------------------------------------------------------------*
030500170609
030600170609
030700080828
030800110913       //-------------------------------------------------------------------
030900161102        BEGSR   Ctrsel  ;
031000161102
031100161102        Wok='N'  ;
031200161102        Wfine=' '  ;
031300161209        Wnic =' '  ;
031400161216        clear wesARB  ;
031500161227        clear evemem  ;
031600161227        clear evesalta;
031700161227        clear conta   ;
031800161212
031900161212        // Escludo le bolle con CTM 4 e 4F
032000161212        if arbctm ='4 ' or arbctm='4F' ;
032100161212          WOK='N'  ;
032200161212          leavesr  ;
032300161212        endif  ;
032400161216
032500161216        // Escludo le bolle consegnate o non consegnate se richiesto
032600161216        //  verifico la data consegna da ARB se non è dirottata
032700161216        if vidcons<>' '  and arbcca<>'1'  ;
032800161216        chain  (arbaas:arblnp:arbnrs:arbnsp)   fnarb01l     ;
032900161216          if %found(fnarb01l)  ;
033000161216           wesARB='S'  ;
033100161216          else  ;
033200161216           wesARB='N'  ;
033300161216          endif  ;
033400161216        endif  ;
033500161216
033600161216        if wesarb<>'S'   ;
033700161216        if vidcons='S' and arbdcm=0 or vidcons='N' and arbdcm>0 ;
033800161216          WOK='N'  ;
033900161216          leavesr  ;
034000161216        endif  ;
034100161216        else  ;
034200161216        if vidcons='S' and a_arbdcm=0 or vidcons='N' and a_arbdcm>0 ;
034300161216          WOK='N'  ;
034400161216          leavesr  ;
034500161216        endif  ;
034600161216        endif  ;
034700161212
034800161212        // escludo le bolle "figlie"
034900161212        chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl01l  ;
035000161212        if %found(fnlbl01l)  ;
035100161212          WOK='N'  ;
035200161212          leavesr  ;
035300161212        endif  ;
035400161102
0355001611021       if (arbaas*10000)+arbmgs>comdsa ;
035600161102        Wfine='S'  ;
035700161102        leavesr ;
0358001611021       endif  ;
035900161102
036000161102        // controllo cliente e serie
0361001611021       if arbccm>0  ;
036200161102         wccm= arbccm  ;
036300161102        else  ;
036400161102         wccm=arbksc  ;
0365001611021       endif  ;
036600161102
036700161102        xx= 1 ;
036800161212        clear wabi  ;
036900161102
037000161102 3      dow xx<= 20  ;
037100161102 4      if lnp(xx) >0  ;
037200161212 4a     if lnp(xx) = arblnp  ;
037300161102
037400161212        if cli(xx)>0 and wccm<>cli(xx)  ;
037500161212        chain (cli(xx):wccm) fiabk01l  ;
037600161212          if %found(fiabk01l) ;
037700161212           eval wabi='S'  ;
037800161213       //   else   ;
037900161213       //     chain (wccm) fiabk02l  ;
038000161213       //     if %found(fiabk02l) ;
038100161213       //      eval wabi='S'  ;
038200161213       //     endif  ;
038300161212          endif  ;
038400161212        endif  ;
038500161212
038600161212 5      if cli(XX)=0 or wccm= cli(xx) or wabi='S'         ;
038700161102
038800161102        if   nrs(xx)='99'   ;
038900161102         WOK='S'  ;
039000161102          leavesr  ;
039100161102        endif   ;
039200161102
039300161102 6      if   nrs(XX)='SI' and arbnrs>0    ;
039400161102         WOK='S'  ;
039500161102          leavesr  ;
039600161102 x6     else  ;
039700161102
039800161102        alfanrs=%editc(arbnrs:'X')  ;
039900161102 7       if alfanrs= nrs(xx)  ;
040000161102         WOK='S'  ;
040100161102          leavesr  ;
040200161102 7       endif  ;
040300161102 6      endif  ;
040400161102
040500161102 5      endif  ;
040600161102 4      endif  ;
040700161212 4a     endif  ;
040800161102
040900161102        xx=xx+1  ;
041000161102 3      enddo  ;
041100161102
041200161102
041300161102
041400161102        ENDSR  ;
041500161102       //-------------------------------------------------------------------
041600161102       BEGSR  Elabora                ;
041700161207
041800161209       ex88=' '   ;
041900161209       clear wdse;
042000161209       clear                   DISfgs     ;
042100161209       clear                   DISdata    ;
042200161209       clear                   DISnpg     ;
042300161209       clear                   DISora     ;
042400161209       clear                   IMPdata    ;
042500161209       clear                   IMPfgs     ;
042600161209       clear                   IDnpg      ;
042700161209       clear                   IDdata     ;
042800161209       clear                   IDora      ;
042900161209       clear                   IDfgs      ;
043000161209       clear evedt    ;
043100161209       clear evedes   ;
043200161209       clear evedim   ;
043300161209       clear evehim   ;
043400161209
043500161207       clear wfkpi000  ;
043600170525
043700170525       // Lettura eventi
043800170525       exsr leggi_eve  ;
043900161207
044000161209       // calcolo ore reali di consegna tra data sped e data consegna
044100161207       exsr cal_gg    ;
044200161212
044300161216
044400161212       // Se le bolle OK non le voglio --> salto
0445001612120      if vidok <>'N' or  kpidly_c>0  ;
044600161207
044700161212       // se si vogliono gli eventi anche bolle ok elaboro
0448001612120a     if kpidly_c > 0  or vidokeve<>'N' ;
044900161102
045000161102       setll (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
045100161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0452001611021      dow  not %eof(fnblt01l)  ;
045300161214       clear memdse  ;
045400161102
045500161214       // verifico se presente una spunta 88/90
0456001612142      if bltdse>arbdbr or bltdse=0 or bltnpp=88 or bltnpp=90  ;
045700161102       setll  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
045800161102       reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
0459001611023      dow not %eof(fnbrv09l)  ;
046000161214
046100161214       // per testimoniare la presenza del collo metto qualsiasi data
046200161214       // entrata, disguido, arrivo. Quindi tratto la pistola 88 e 90
046300161214       //  allo stesso modo
046400161214
0465001612144       if brvnps=88 or brvnps=90 ;
046600161102         ex88='S'   ;
0467001612145        if  bltdse>0  ;
046800161214           memdse=bltdse;
046900161214x5       else  ;
047000161214          chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0471001612146         if %found(fnart01l) ;
0472001612147         if artdet>0 and artdet<artdam ;
047300161214           memdse=artdet;
047400161214x7        else   ;
0475001612148         if artdam>0  ;
047600161214           memdse=artdam;
0477001612148         endif  ;
0478001612147         endif  ;
0479001612146         endif  ;
0480001612145         endif  ;
048100161214
048200161214 4       endif  ;
048300161214
048400161214        if   memdse>0  ;
048500161214         leave ;
048600161214        endif  ;
048700161214
0488001612144       //if brvnps=90 ;
048900161214        //  chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0490001612145       // if artdam>bltdse  ;
049100161214        //  ex88='S'   ;
049200161214        //  wdse=bltdse;
049300161214        //  leave  ;
0494001612145       // endif  ;
0495001612144       //endif  ;
049600161103
049700161102
0498001611023      reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
049900161102       enddo  ;
0500001611022      endif   ;
050100161209
050200161214       // se era spunta in 88 memorizzo la più recente come data ;
050300161214       if memdse>wdse  ;
050400161214         wdse=memdse  ;
050500161214       endif  ;
050600161214
050700161214       // Verifico se in IMP/Disguido ne basta uno per bolla
050800161214       if idnpg=' '  ;
050900161214         exsr IMPDISG  ;
051000161214       endif  ;
051100161102
051200161102
051300161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0514001611021      enddo  ;
051500161102
051600161209       // verifico se la trazione è ritardo , usando la data arrivo ult collo
051700161216       if wesARB='S'         ;
051800161103       endif  ;
051900161209
0520001612270a     endif  ;
052100161103
052200161102       exsr   Scrivifile  ;
0523001612120      endif  ;
052400161212
052500161102       ENDSR   ;
052600161209       //-------------------------------------------------------------------
052700161209       BEGSR  Leggi_eve              ;
052800161209
052900161209       // Lettura eventi che "servono"
053000161209       clear YY  ;
053100161227       clear indx;
053200161209
053300161209       setll  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
053400161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
053500161209  1    dow  not %eof(fnevb01l)   ;
053600161209
053700161209  2    if %lookup(evbcev:cev)>0   ;
053800161209
053900161227       // Se si tratta di MIC, memorizzo quello con data e ora più alta
054000161227  3    if evbcev='MIC'   ;
054100161227       indx=%lookup(evbcev:evedes)  ;
054200161227  4    if indx>0 ;
054300161227        clear evedt(indx)   ;
054400161227        clear evedes(indx)   ;
054500161227        clear evedim(indx)   ;
054600161227        clear evehim(indx)   ;
054700161227  4    endif  ;
054800161227  3    endif;
054900161227
055000161209       yy=yy+1  ;
055100161227
055200161227  3    if yy<32   ;
055300161209
055400161209        evedt(yy)=evbdev   ;
055500161209        evedes(yy)=evbcev   ;
055600161209        evedim(yy)=evbdtv   ;
055700161209        evehim(yy)=evborv   ;
055800161209
055900161209       // e si tratta di NIC memorizzo anche data e ora immissione
056000161209       // lo elimino se trovo un MIC successivo
056100161209  4     if evbcev='NIC' ;
056200161209         eval wnic='S'   ;
056300161209  4     endif  ;
056400161209  3     endif  ;
056500161209
056600161227  2     endif  ;
056700161209
056800161209        // solo se presente NIC verifico se da eliminare
056900161209   3      if wnic='S' and evbcev='MIC'    ;
057000161209
057100161227        ee=1   ;
057200161227   4    dow ee<=32  ;
057300161227   5    if evedes(ee)='NIC' ;
057400161209
057500161209   6    select  ;
057600161227        when evedt(ee)<>evbdev ;
057700161209
057800161227        when evedim(ee)<evbdtv ;
057900161227        clear evedt(ee)   ;
058000161227        clear evedes(ee)   ;
058100161227        clear evedim(ee)   ;
058200161227        clear evehim(ee)   ;
058300161227        wnic=' '   ;
058400161209
058500161227        when evedim(ee)=evbdtv and evehim(ee)<evborv ;
058600161227        clear evedt(ee)   ;
058700161227        clear evedes(ee)   ;
058800161227        clear evedim(ee)   ;
058900161227        clear evehim(ee)   ;
059000161227        wnic=' '   ;
059100161209        endsl  ;
059200161209
059300161209   5    endif   ;
059400161209
059500161227        ee=ee+1  ;
059600161209   4    enddo  ;
059700161209
059800161209   3      endif  ;
059900161209
060000161209
060100161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
060200161209   1   enddo  ;
060300161209
060400161227       // il MIC lo tengo solo se è l'ultimo degli eventi altrimenti
060500161227       // lo emimino
060600161227       indx=%lookup('MIC':evedes)  ;
060700161227  1    if indx>0 ;
060800161227       yy  =indx+1  ;
060900161227  2    dow yy<=32   ;
061000161227  3     if  evedes(yy)<>*blanks   ;
061100161227         clear evedt(indx)   ;
061200161227         clear evedes(indx)   ;
061300161227         clear evedim(indx)   ;
061400161227         clear evehim(indx)   ;
061500161227         leave   ;
061600161227  3     endif  ;
061700161227
061800161227       yy=yy+1  ;
061900161227  2    enddo  ;
062000161227  1    endif  ;
062100161227
062200161227       // conto anche quanti eventi ci sono per memorizzare solo gli
062300161227       // ultimi 10
062400161227       yy=1  ;
062500161227       clear conta   ;
062600161227       dow yy<=32                ;
062700161227         if  evedes(yy)<>*blanks   ;
062800161227          conta=conta+1  ;
062900161227         endif  ;
063000161227       yy=yy+1  ;
063100161227       enddo  ;
063200161227
063300161227       clear   evesalta  ;
063400161227       if conta>10  ;
063500161227       evesalta=conta-10  ;
063600161227       endif   ;
063700161227
063800161209       ENDSR   ;
063900161102       //-------------------------------------------------------------------
064000161102       BEGSR  Scrivifile             ;
064100111116
064200110913
064300161102       kpiidute=knmeb   ;
064400161102       kpiepo=dutpou  ;
064500110913
064600161102       kpiccm=wccm    ;
064700161102       kpirsm=arbrsm  ;
064800161102
064900161102       kpiaas=arbaas  ;
065000161102       kpilnp=arblnp  ;
065100161102       kpimes=%int(%subst(%editc(arbmgs:'X'):1:2))  ;
065200161102       kpirmn=arbrmn  ;
065300161102       kpirma=arbrma  ;
065400161102       kpinrs=arbnrs  ;
065500161102       kpinsp=arbnsp  ;
065600161102       kpimgs=arbmgs  ;
065700161102       kpilna=arblna  ;
065800161102       kpirsd=arbrsd  ;
065900161102       kpilod=arblod  ;
066000161102       kpiprd=arbprd  ;
066100161102       kpicad=arbcad  ;
066200161215       kpifin=arbfin  ;
066300161102       kpinzd=arbnzd  ;
066400161102       kpiffd=arbffd   ;
066500161102       kpitc1=arbtc1   ;
066600161102       kpitc2=arbtc2   ;
066700161102       kpigc1=arbgc1   ;
066800161102       kpigc2=arbgc2   ;
066900161102       kpincl=arbncl   ;
067000161102       kpipkb=arbpkb   ;
067100161102       kpivlb=arbvlb   ;
067200161102       kpitcr=arbtcr   ;
067300161102       kpidcr=arbdcr   ;
067400161102       kpidcp=arbdcp   ;
067500161102       kpidcm=arbdcm   ;
067600161102       kpihmc=arbhmc   ;
067700161209       kpicca=arbcca   ;
067800161209
067900161216       if arbdcm=0 and wesarb='S' and a_arbdcm>0 and arbcca<>'1';
068000161209       kpidcm=a_arbdcm   ;
068100161209       kpihmc=a_arbhmc   ;
068200161209       kpicca=a_arbcca   ;
068300161209       endif  ;
068400161209
068500161209       if kpicca<>' '  ;
068600161209       tblkey=kpicca   ;
068700161209       chain (1:'7O':tblkey) tabel  ;
068800161209         if %found(tabel00f )  ;
068900161209          kpicca_des=tbluni   ;
069000161209         endif  ;
069100161209       endif  ;
069200161103
069300161103        // pistola ex 88
069400161102        if ex88='S'     ;
069500161102        kpip88='S'      ;
069600161103        kpidse=wdse     ;
069700161102        endif           ;
069800161103
069900161103        //IMP disguido
070000161103        if IDnpg<>' '   ;
070100161103           if IDnpg='P'   ;
070200161103           kpiimpds='P'   ;
070300161103           else  ;
070400161103           kpiimpds='D'   ;
070500161103           endif ;
070600161103        endif ;
070700161209
070800161209       yy=1  ;
070900161227       clear evemem   ;
071000161209
071100170525 1     dow yy<=32               ;
071200161227 2     if  evedes(yy)<>*blanks   ;
071300170524
071400170524       // Verifico se data evento uguale alla data prevista consegna
071500170524 2a    if kpidce>0 ;
071600170524
071700170525 3     if evedt(yy)<=kpidce and kpidcesn<>'S'  ;
071800170524          indx= %lookup(evedes(yy):cev) ;
071900170524 4     if cevt(indx)='E';
072000170524        kpidcesn='S'  ;
072100170524 4     ENDIF;
072200170524       // memorizzo diversamente se c'è un evento particolare
072300170525 4     if cevt(indx)='P' and  evedt(yy) =kpidce ;
072400170524        kpidcesn='P'  ;
072500170524 4     ENDIF;
072600170524
072700170524 3     ENDIF;
072800170524 2a    ENDIF;
072900161227
073000161227       evemem=evemem+1  ;
073100161227
073200161227       // se conta < 10 memorizzo tutti altrimenti salto i primi
073300161227
073400170525 2a    if kpidly_c > 0  or vidokeve<>'N' ;
073500161227 3     if conta <=10 or evemem>evesalta  ;
073600161227 4      select   ;
073700161227         when KPICEV1=*blanks and evedes(yy)<>'MIC';
073800161209         KPICEV1=evedes(yy)  ;
073900161209         KPIDEV1=evedt(yy)  ;
074000161227         when KPICEV2=*blanks and evedes(yy)<>'MIC';
074100161209         KPICEV2=evedes(yy)  ;
074200161209         KPIDEV2=evedt(yy)  ;
074300161227         when KPICEV3=*blanks and evedes(yy)<>'MIC';
074400161209         KPICEV3=evedes(yy)  ;
074500161209         KPIDEV3=evedt(yy)  ;
074600161227         when KPICEV4=*blanks and evedes(yy)<>'MIC';
074700161209         KPICEV4=evedes(yy)  ;
074800161209         KPIDEV4=evedt(yy)  ;
074900161227         when KPICEV5=*blanks  and evedes(yy)<>'MIC';
075000161209         KPICEV5=evedes(yy)  ;
075100161209         KPIDEV5=evedt(yy)  ;
075200161227         when KPICEV6=*blanks  and evedes(yy)<>'MIC';
075300161209         KPICEV6=evedes(yy)  ;
075400161209         KPIDEV6=evedt(yy)  ;
075500161227         when KPICEV7=*blanks  and evedes(yy)<>'MIC';
075600161227         KPICEV7=evedes(yy)  ;
075700161209         KPIDEV7=evedt(yy)  ;
075800161227         when KPICEV8=*blanks  and evedes(yy)<>'MIC';
075900161209         KPICEV8=evedes(yy)  ;
076000161209         KPIDEV8=evedt(yy)  ;
076100161227         when KPICEV9=*blanks  and evedes(yy)<>'MIC';
076200161209         KPICEV9=evedes(yy)  ;
076300161209         KPIDEV9=evedt(yy)  ;
076400161209         when KPICEV0=*blanks  ;
076500161209         KPICEV0=evedes(yy)  ;
076600161209         KPIDEV0=evedt(yy)  ;
076700161227 4       endsl  ;
076800161227 3       endif  ;
076900161227 2       endif  ;
077000170525 2a      endif  ;
077100161209
077200161209         yy=yy+ 1;
077300161209
077400161227 1     enddo  ;
077500170523
077600170523        // Se la data consegna prevista corrisponde alla data cons merce--> ok
077700170526 1      if kpidcm>0 and kpidce>0 and kpidcesn<>'S' ;
077800170526        // utilizzo la data  di consegna merce di arb se esiste per le bolle
077900170526        //  legate
078000170526
078100170526 2        if wesarb='S' and a_arbdcm<=kpidce ;
078200170526            kpidcesn='S' ;
078300170526 x2       else  ;
078400170526 3        if kpidcm<=kpidce ;
078500170523            kpidcesn='S' ;
078600170526 3        ENDIF;
078700170526 2      ENDIF;
078800170526 1      ENDIF;
078900170526 1          if kpidcesn=' '  and kpidce>0 ;
079000170525            kpidcesn='N' ;
079100170526 1          ENDIF;
079200110916
079300161102       write  wfkpi000 ;
079400170609       exsr   WRITIVGD ;
079500110913
079600161214       CONTAB=CONTAB+1   ;
079700110926
079800110913       ENDSR                  ;
079900170609
080000170609
080100170609
080200170609     C*------------------------------------------------------------------------*
080300170609     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
080400170609     C*------------------------------------------------------------------------*
080500170609     C     WRITIVGD      BEGSR
080600170609     C*
080700170609     C                   clear                   tivgd000
080800170609     C                   eval      vgdDTA = %subst(DS_KPI:1:%size(DS_KPI))
080900170609     C                   eval      vgdTIP = '&1'
081000170609     C                   eval      vgdKSU = '0BRT0'+%editc(dutpou:'X')
081100170609     C                   eval      vgdTSC = 'WW'
081200170609     C                   eval      vgdDAT = dateu
081300170609     C                   eval      vgdPRG = o§7VASPRG
081400170609     C                   eval      vgdPGM = procname
081500170609     C                   eval      vgdSTO = '?'
081600170609     C                   write     tivgd000
081700170609     C*
081800170609     C                   ENDSR
081900170609     C*------------------------------------------------------------------------*
082000170609
082100170609
082200170609
082300080327       //-------------------------------------------------------------------
082400080327       BEGSR  *INZSR                 ;
082500080328
082600080328       in(E) *dtaara   ;
082700080328       if    %error or rsut  =*blanks  ;
082800080328       callp TIBS34R (TIBS34DS)        ;
082900080328       in    *dtaara   ;
083000080328       endif                           ;
083100080829
083200110613       // imposto data e ora del giorno
083300110613       dataiso=datasys  ;
083400161214       dataeur=datasys  ;
083500110613       dateu=%dec(dataiso)   ;
083600161214       dataogg=%dec(dataeur)   ;
083700110616
083800110613       ora_iso=ora_sys  ;
083900110613       ora_u=%dec(ora_iso)   ;
084000080829
084100110913       // Se richiesto file pulisco il lancio precedente per filiale
084200161102       setll (dutpou)   wfKPI01l   ;
084300161102       reade (dutpou)   wfKPI01l   ;
084400110616
084500161102       dow  not %eof(wfKPI01l)   ;
084600161102       delete wfKPI000  ;
084700110616
084800161102       reade (dutpou)   wfKPI01l   ;
084900110616       enddo  ;
085000110616
085100161209       clear xx ;
085200161209       setll  (1:'2A') tabel00f ;
085300161209       reade  (1:'2A') tabel00f ;
085400161209
085500161209       dow not %eof(tabel00f ) ;
085600161209       if tblflg=' '  ;
085700161209        ds2a = tbluni   ;
085800161209
085900161209       if   §2aftg='S' or §2atip='N' or §2aftc='A'  ;
086000161209       xx=xx+1  ;
086100161209        cev(xx)=%Subst(tblkey:1:3)  ;
086200170524        if §2antc='S' ;
086300170524          cevt(xx)='E' ;
086400170524        ENDIF;
086500161209       endif  ;
086600161209       endif  ;
086700161209
086800161209       reade  (1:'2A') tabel00f ;
086900161209       enddo  ;
087000161209
087100161209       setll  (1:'2Z') tabel00f ;
087200161209       reade  (1:'2Z') tabel00f ;
087300161209
087400161209       dow not %eof(tabel00f ) ;
087500161209       if tblflg=' '  ;
087600161209        ds2z = tbluni   ;
087700161209
087800161209       if   §2zcep<>' '  ;
087900161209       xx=xx+1  ;
088000161209        cev(xx)=%subst(tblkey:1:3)  ;
088100170523        cevt(xx)='P' ;
088200161209       endif  ;
088300161209       endif  ;
088400161209
088500161209       reade  (1:'2Z') tabel00f ;
088600161209       enddo  ;
088700161209
088800161209       // infine aggiungo fisso il NIC
088900161209       xx=xx+1  ;
089000161209       cev(xx)='NIC' ;
089100161227       xx=xx+1  ;
089200161227       cev(xx)='MIC' ;
089300161209
089400161209       ENDSR     ;
089500161102       //-------------------------------------------------------------------
089600161209       BEGSR  PrepaLNP               ;
089700161102
089800161102       clear  LnpSel  ;
089900161102       xx= 1 ;
090000161102       yy= 1 ;
090100161102
090200161102       dow xx <=20  ;
090300161102        if lnp(xx)>0  ;
090400161102         Indx=%lookup(lnp(xx):LnpSel)  ;
090500161102         if Indx=0  ;
090600161102           eval lnpSel(yy)=lnp(xx)  ;
090700161102           yy=yy+1   ;
090800161102         endif  ;
090900161102
091000161102        endif  ;
091100161102
091200161102        xx=xx+1   ;
091300161102        enddo     ;
091400161102
091500161102        // Memorizzo nel campo oggcli il codice cliente che devo
091600161102        //  mettere nell'ogggetto della mail
091700161212        // clear oggcli  ;
091800161212        // if cli(1)>0   ;
091900161212        //   oggcli=cli(1)  ;
092000161212        // endif  ;
092100161102
092200161102       ENDSR  ;
092300161207
092400161103
092500161103      /end-free
092600161103
092700161207     c*-------------------------------------------------------------------
092800161207     c     Cal_gg        BEGSR
092900161207     c
093000161209     c     karb          klist
093100161209     c                   kfld                    arbaas
093200161209     c                   kfld                    arblnp
093300161209     c                   kfld                    arbnrs
093400161209     c                   kfld                    arbnsp
093500170524     c     kar5          klist
093600170524     c                   kfld                    arbaas
093700170524     c                   kfld                    arblnp
093800170524     c                   kfld                    arbnrs
093900170524     c                   kfld                    arbnsp
094000170524     c                   kfld                    ktrd
094100161209
094200161216     c* chain solo se non l'ho già fatto prima
094300161216     c                   if        wesarb=' '
094400161209     c     karb          chain     fnarb01l
094500170105     c                   if        %found(fnarb01l)
094600170105     c                   eval      wesARB='S'
094700170105     c                   else
094800170105     c                   eval      wesARB='N'
094900170105     c                   endif
095000161216     c                   endif
095100170524
095200170524     c     kar5          chain     fiar501l
095300170524     c                   if        %found(fiar501l)
095400170524     c                   eval      dar5gen=ar5uni
095500170524     c                   else
095600170524     c
095700170524     c                   clear                   dar5gen
095800170524     c                   endif
095900170525
096000170525     c* prendo il primo evento pieno "NO MIC/NIC"
096100170525     c                   clear                   dtprimoeve
096200170525     c                   eval      yy=1
096300170525     c                   dow       yy<=10 and dtprimoeve=0
096400170525     c                   if        evedes(yy)<>*blanks and
096500170525     c                             evedes(yy)<>'MIC' and
096600170525     c                             evedes(yy)<>'NIC'
096700170525     c                   EVAL      DTPRIMOEVE=EVEDT(YY)
096800170525     c                   ENDIF
096900170525     c                   ADD       1             YY
097000170525     c                   ENDDO
097100170525
097200161216
097300161207     c                   clear                   tnsd99ds
097400161207     C                   MOVEL     ' '           D98TLA
097500161216     c                   if        wesARB='S'
097600161207     C                   MOVEL     'A'           D98TBO
097700161209     c                   else
097800161209     C                   MOVEL     'P'           D98TBO
097900161209     c                   endif
098000161207     C                   MOVEL     ARBAAS        D98AAS
098100161207     C                   MOVEL     ARBLNP        D98LNP
098200161207     C                   MOVEL     ARBNRS        D98NRS
098300161207     C                   MOVEL     ARBNSP        D98NSP
098400161207     c
098500161207     c                   if        arbtsp='D'
098600161207     c                   eval      I98TSP_FOR='C'
098700161207     c                   endif
098800170524     c
098900161207     C*
099000161207     C                   CALL      'TNSD99R'
099100161207     C                   PARM                    TNSD99DS
099200161207     c
099300161207     c* ore teoriche
099400161209     c                   eval      KPIdly_T=d98trc
099500161209     c                   eval      KPIdag  =d98dag
099600161209     c                   eval      KPIdcg  =d98dcg
099700161215     c                   eval      KPIdac  =d98dac
099800161215     c                   eval      KPIdcc  =d98dcc
099900161207
100000161207     c* Richiamo il pgm utilizzando la DS eliminando il num sped
100100161207     c*  e la giacenza
100200161207     c                   clear                   d98nsp
100300161207     c                   clear                   d98dti
100400161207     c                   clear                   d98hti
100500161207     c                   clear                   d98cmc
100600161207     c                   clear                   d98dag
100700161207     c                   clear                   d98dcg
100800161207     c                   eval      d98duc=d98dsp
100900161207     c                   eval      d98dbr=d98dsp
101000170105     c                   eval      D98FDRT='S'
101100161207     c                   clear                   d98dam
101200170524     c* considero i giorni di chiusura se inseriti in partenza
101300170524     c                   if        §ar5mdp='S'
101400170524     c                   eval      d98gc1=§AR5GC1
101500170524     c                   eval      d98gc2=§AR5GC2
101600170524     c                   else
101700170524     c                   clear                   d98gc1
101800170524     c                   clear                   d98gc2
101900170524     c                   endif
102000170524
102100170524     c* la consegna richiesta la considero se inserita in partenza
102200170524     c*  o in arrivo prima del primo tentativo
102300170524     c                   if        d98dcr>0 and (§ar5mdp='S' and
102400170524     c                             §ar5dcr<>%editc(d98dcr:'X'))
102500170525     c**                   clear                   datadcr
102600170525     c**     karb          setll     fiarbf2c
102700170525     c**     karb          reade     fiarbf2c
102800170525     c**                   dow       not %eof(fiarbf2c)
102900170525     c**                   if        %subst(v_arbcvb:1:1)='C'
103000170525     c**                             and v_arbcvb<>'CP'
103100170525     c**                   eval      datadcr=v_arbdtv
103200170525     c**                   leave
103300170525     c**                   endif
103400170524     c
103500170525     c**     karb          reade     fiarbf2c
103600170525     c**                   enddo
103700170525     c*
103800170525     c                   if        d98dcr>dtprimoeve
103900170524     c                   clear                   d98dcr
104000170524     c                   clear                   d98hcr
104100170524     c                   clear                   d98tcr
104200170524     c                   endif
104300170524     c                   endif
104400170525     c* Se il fermo deposito è stato inserito dopo il primo tentativo
104500170525     c*  NON lo considero
104600170525     c                   if        d98ffd='S'   and
104700170525     c                             dtprimoeve>0  and
104800170525     c                             §ar5dfd>*zeros and
104900170525     c                             %int(§ar5dfd)>=dtprimoeve
105000170525     c                   clear                   d98ffd
105100170525     c                   endif
105200170525     c
105300170524     c
105400161207     C                   CALL      'TNSD99R'
105500161207     C                   PARM                    TNSD99DS
105600161207     c
105700161207     c* controllo le ore reali  con le ore teoriche
105800161209     c                   eval      KPIdly_R=d98rrc
105900170523     c* Memorizzo la data prevista consegna
106000170523     c                   eval      KPIdce= d98dce
106100170524     c* Se il fermo deposito è stato inserito dopo il primo tentativo
106200170524     c*  NON lo considero
106300170524     c                   if         d98ffd='S'
106400170525     c                   if        dtprimoeve=0  or
106500170524     c                             §ar5dfd<=*zeros or
106600170525     c                             %int(§ar5dfd)< dtprimoeve
106700170524     c                   clear                   kpidce
106800170524     c                   endif
106900170524     c                   endif
107000161215     c
107100170523     c* se la spedizione è borderizzata in data festiva --> aggiungo 24 ore
107200161215     c*  alle ore reali
107300170105     c* ES 05/1/17 non lo faccio più in quanto  ho modificato allo stesso modo
107400170105     c*  il calcolo della affidabilità tnsd99r
107500170105     c**                 if        arbdbr>0
107600170105     c**                 clear                   wdat8
107700170105     c**                 clear                   tfptfa
107800170105     c**                 z-add     arbdbr        datada
107900170105     c*+                 z-add     arbdbr        dataa
108000170105     c**                 z-add     arbtfp        p_tfp
108100170105     c*+                 z-add     arbtfa        p_tfa
108200170105     c**                 call      'XSRLAV8'
108300170105     c**                 parm                    wdat8
108400170105     c**                 parm                    tfptfa
108500161215     c* se giorno FESTIVO aggiungo 24 ore
108600170105     c**                 if        ggl=0
108700170105     c**                 eval      KPIdly_R=KPIdly_R+24
108800170105     c**                 endif
108900161215
109000170105     c**                 endif
109100161215     c
109200161207     c                   if        kpidly_r<=kpidly_t
109300161207     c                   clear                   kpidly_c
109400161207     c                   else
109500161207     c
109600161207     c                   eval      kpidly_c =kpidly_r-kpidly_t
109700161207     c                   endif
109800161207     c*
109900161213     c* CONSEGNA RICHIESTA IL
110000161212     c* Se data richiesta = data consegna merce
110100161209     c*  sempre OK
110200161212     c**                 if        arbtc1='A' or arbtc2='A'  or
110300161212     c**                           arbtc1='S' or arbtc2='S'
110400161213     c                   if        arbdcr>0 and arbtcr=' '
110500161213     c                   if        arbdcr=arbdcm
110600161209     c                   clear                   kpidly_c
110700161209     c                   endif
110800161213     c* se bolla NON consegnata e data cons richiesta > di oggi
110900161213     c* per ora ON TIME
111000161213     c                   if        arbdcm=0 and arbdcr>=dateu
111100161213     c                   clear                   kpidly_c
111200161213     c                   endif
111300161213     c                   endif
111400161213     c
111500161212     c**                 endif
111600161213
111700161213     c* CONSEGNA RICHIESTA PRIMA DEL
111800161213     c                   if        arbdcr>0 and arbtcr='P'
111900161213     C                   IF        ARBDCM>0 AND ARBDCM<ARBDCR
112000161213     c                   clear                   kpidly_c
112100161213     c                   endif
112200161213     C                   IF        ARBDCM=0
112300161213     c                   clear                   kpidly_c
112400161213     c                   endif
112500161213     c                   endif
112600161213     c* CONSEGNA RICHIESTA DOPO IL
112700161213     c                   if        arbdcr>0 and arbtcr='D'
112800161213     C                   IF        ARBDCM>0 AND ARBDCM>ARBDCR
112900161213     c                   clear                   kpidly_c
113000161213     c                   endif
113100161213     C                   IF        ARBDCM=0
113200161213     c                   clear                   kpidly_c
113300161213     c                   endif
113400161213     c                   endif
113500161207
113600161212     c* se cons anomala RESO o DIROTTAMENTO --> sempre ok
113700161212     c                   if        arbcca='1' or arbcca='2' or
113800161212     c                             arbcca='6'
113900161212     c                   clear                   kpidly_c
114000161212     c                   endif
114100161207     c*
114200161207     c                   ENDSR
114300161103     c*-------------------------------------------------------------------
114400161103     c     IMPdisg       BEGSR
114500161209     c
114600161209     c     kbrv          Klist
114700161209     c                   Kfld                    bltfls
114800161209     c                   Kfld                    bltlna
114900161209     c                   Kfld                    bltnrs
115000161209     c                   Kfld                    bltnsc
115100161103     c
115200161103     c* per ogni collo verifico se in IMP o disguidato
115300161103     c* prendo la data più alta
115400161103     c     kbrv          setll     fnbrv07l
115500161103     c     kbrv          reade     fnbrv07l
115600161103    2c                   dow       not %eof(fnbrv07l)
115700171018
115800171018     c*se si tratta di disguido ignoro se cat 5 e spuntato dal 2 livello
115900171018     c                   if        brvatr='D' and brvnpg=5
116000171018     c                             and brvfle=arbtfp
116100171018     c                   clear                   brvatr
116200171018     c                   endif
116300171018     c
116400161103    3c                   if        brvatr='D'  or brvnpg=3
116500161103     c* prendo la data della spunta
116600161103     c                   clear                   fnlv53ds
116700161103     C                   MOVEL     BRvNPG        D53NPG
116800161103     C                   Z-ADD     BRvNFV        D53NFV
116900161103     C                   MOVEL     BRvFGS        D53FGS
117000161103     C                   CALL      'FNLV53R'
117100161103     C                   PARM                    fnlv53ds
117200161103    4c                   if        d53err<>*blanks
117300161103    5c                   if        brvdcs>0
117400161103     c                   eval      d53dfv=brvdcs
117500161103     c                   else
117600161103    6c                   if        brvdfs>0
117700161103     c                   eval      d53dfv=brvdfs
117800161103    6c                   endif
117900161103    5c                   endif
118000161103    4c                   endif
118100161103     c
118200161103    4c                   select
118300161103     c* se si tratta di DISGUIDO
118400161103     c                   when      brvatr='D'
118500161103     c
118600161103    5c                   if        d53dfv=DISdata
118700161103     c* Verifico l'ora
118800161103    6c                   if        brvhcs>0 and brvhcs>DISora
118900161103     c                   eval      DISdata=d53dfv
119000161103     c                   eval      DISfgs =brvfgs
119100161103     c                   eval      DISnpg =brvnpg
119200161103     c                   eval      DISora =brvhcs
119300161103   x6c                   else
119400161103    7c                   if        brvhms>0 and brvhms>DISora
119500161103     c                   eval      DISdata=d53dfv
119600161103     c                   eval      DISfgs =brvfgs
119700161103     c                   eval      DISnpg =brvnpg
119800161103     c                   eval      DISora =brvhms
119900161103    7c                   endif
120000161103    6c                   endif
120100161103    5c                   endif
120200161103     c
120300161103    5c                   if        d53dfv>DISdata
120400161103     c                   eval      DISdata=d53dfv
120500161103     c                   eval      DISfgs =brvfgs
120600161103     c                   eval      DISnpg =brvnpg
120700161103     c                   eval      DISora =brvhcs
120800161103     c                   if        disora=0
120900161103     c                   eval      DISora =brvhms
121000161103     c                   endif
121100161103    5c                   endif
121200161103     c
121300161103     c* se si tratta di IMP
121400161103    4c                   when      d53spg='P' and d53dfv>impdata
121500161103     c                   eval      IMPdata=d53dfv
121600161103     c                   eval      IMPfgs =brvfgs
121700161103    4c                   endsl
121800161103    3c                   endif
121900161103
122000161103     c     kbrv          reade     fnbrv07l
122100161103    2c                   enddo
122200161103
122300161103     c* memorizzo la data più alta
122400161103     c* IMP vince sul disguido per stessa data, senza guardare l'ora
122500161103    2c                   if        IMPdata>0 and IMPdata>=IDdata
122600161103     c                   eval      IDdata=IMPdata
122700161103     c                   eval      IDfgs=IMPfgs
122800161103     c                   eval      IDora=0
122900161103     c                   eval      IDnpg='P'
123000161103    2c                   endif
123100161103    2c                   if        (DISdata>0 and DISdata=IDdata
123200161103     c                             and IDnpg<>'P' and DISora>IDora) or
123300161103     c                             (DISdata>0 and DISdata>IDdata)
123400161103     c                   eval      IDdata=DISdata
123500161103     c                   eval      IDfgs=DISfgs
123600161103     c                   eval      IDora=DISora
123700161103     c                   eval      IDnpg=%char(DISnpg)
123800161103    2c                   endif
123900161103     c
124000161103     C                   ENDSR
124100161214     OQSYSPRT   E            fine             02
124200161214     O                                              'Elaborazione esiti consegn'
124300161214     o                                              'a il'
124400161214     o                       dataogg           +  2 '  /  /    '
124500161214     o                       ora_u             +  2 '  :  :  '
124600161214     o                                         +  2 'Bolle scritte :'
124700161214     o                       contab        2   +  2
124800161214**
124900161214OVRPRTF FILE(QSYSPRT) USRDTA('EsitiCons') HOLD(*YES)
