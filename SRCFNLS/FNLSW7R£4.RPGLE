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
015000170609     C*
015100170609     C* Avvio il controllo sincronia
015200170609     C                   CLEAR                   TIS7VASDS
015300170609     C                   EVAL      i§7VASOPZ = 'SCM'
015400170609     C                   CALL(e)   'TIS7VASR1'
015500170609     C                   PARM                    TIS7VASDS
015600170609     C*
015700170609     C* Verifico esito chiamata
015800170609     C                   if        %error OR o§7VASOK = *off
015900170609     C                   dump(A)
016000170609     C                   seton                                        lr
016100170609     C                   return
016200170609     C                   endif
016300161214     C* ESEGUO OVRPRTF
016400161214     C                   Z-ADD     61            LUNG             15 5
016500161214     C                   MOVEL     *BLANKS       COMMAN           80
016600161214     C                   MOVEA     CMD           COMMAN
016700161214     C                   CALL      'QCMDEXC'
016800161214     C                   PARM                    COMMAN
016900161214     C                   PARM                    LUNG
017000161214     C*
017100161214     C                   Z-ADD     1             C                 3 0
017200161214     C                   OPEN      QSYSPRT
017300170609     C*
017400170609     C* Apertura file DOWNLOAD
017500170609     C                   if        not %open(tivgd00f)
017600170609     C                   open      tivgd00f
017700170609     C                   endif
017800170609     C*
017900170609     C* Inizializzo la transazione DOWNLOAD
018000170609     C                   CLEAR                   TIS7VASDS
018100170609     C                   EVAL      i§7VASOPZ = 'PRG'
018200170609     C                   CALL(e)   'TIS7VASR1'
018300170609     C                   PARM                    TIS7VASDS
018400170609     C*
018500170609     C* Se OK => proseguo
018600170609     C                   if        not %error AND
018700170609     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
018800170609     C                   movel     'S'           wProcedi          1
018900170609     C                   else
019000170609     C                   movel     'N'           wProcedi
019100170609     C                   endif
019200170609     C*
019300170609     C* Se ok a procedere => elaboro
019400170609     C                   if        wProcedi = 'S'
019500080317      /free
0196001610311      if comdsa=0 ;
019700161031        comdsa=comdsd ;
0198001610311      endif  ;
019900161102
020000161102       // Linee partenza selezionate
020100161102          EXSR  PrepaLNP      ;
020200161212
020300161031
0204001610311      if comdsa=comdsd ;
020500161031         *in01 =*on  ;
0206001610311       endif  ;
020700161102
020800161209       zz=1   ;
020900161031
0210001611021      dow lnpsel(zz)>0  ;
021100161102          setll (lnpsel(zz):kaad:kmgd) fnblp31l ;
021200161031          if *in01=*on  ;
021300161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
021400161031          else  ;
021500161102          reade (lnpsel(zz)) fnblp31l ;
021600161031          endif  ;
021700161031
0218001611022         dow not %eof(fnblp31l)   ;
021900161102
022000161102          exsr   Ctrsel    ;
022100161102
022200161102          if Wfine='S'  ;
022300161102          leave  ;
022400161102          endif  ;
022500161102
022600161102          if wok='S'   ;
022700161102          exsr elabora  ;
022800161102          endif  ;
022900161102
023000161102
023100161102          if *in01=*on  ;
023200161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
023300161102          else  ;
023400161102          reade (lnpsel(zz)) fnblp31l ;
023500161102          endif  ;
0236001611022         enddo  ;
023700161031
023800161102          zz=zz+1  ;
023900161031
0240001611021      enddo ;
024100111116
024200110617        // Se scritto almeno un record, invio all'indirizzo e-mail
024300161207        //pInFILE='WFKPI00F'   ;
024400161207        //pInIDJOB=knmeb       ;
024500161207          // Nell'oggetto imposto il primo codice cliente richiesto
024600161207  2     //if  oggcli>0   ;
024700161207        //chain (1:dutkci:oggcli) cnaco00f  ;
024800161207  3     //if not %found(cnaco00f)  ;
024900161207        //  acorag=*all'?'  ;
025000161207  3     //endif  ;
025100111202
025200161207        //  pinEMLVAR='*OBJM* Anomalie Cliente: '+%editc(oggcli:'X')+ '-'+
025300161207        //             %subst(acorag:1:15)  ;
025400161207  x2    //else  ;
025500111202          // Imposto la prima lnp/serie richiesta   ;
025600161207        // pinEMLVAR='*OBJM* Anomalie Lnp/Ser:'+%editc(lnp(1):'X')+' '+
025700161207        //                               nrs(1)      ;
025800161207  2     //endif  ;
025900111206
026000161207        //  pInEMLDEST=%trim(videmail)+'@brt.it' ;
026100161102        //callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
026200161102        //                pInEMLVAR:pInMRAKE2:pOutESITO) ;
026300170609
026400170609     C*
026500170609     C* Chiudo il file di output
026600170609     C                   if        %open(tivgd00f)
026700170609     C                   close     tivgd00f
026800170609     C                   endif
026900170609     C*
027000170609     C* Finalizzo la transazione
027100170609     C                   EVAL      i§7VASOPZ  = 'RLS'
027200170609     C                   EVAL      i§7VASTIP  = '&1'
027300170609     C                   EVAL      i§7VASKSU  = '0BRT0'+%editc(dutpou:'X')
027400170609     C                   EVAL      i§7VASTSC  = 'WW'
027500170609     C                   EVAL      i§7VASSTO  = '?'
027600170609     C                   EVAL      i§7VASSTTO = 'L'
027700170609     C                   EVAL      i§7VASPRG  = o§7VASPRG
027800170609     C                   CALL(e)   'TIS7VASR1'
027900170609     C                   PARM                    TIS7VASDS
028000170609     C*
028100170609     C* Verifico esito chiamata
028200170609     C                   if        %error OR o§7VASOK = *off
028300170609     C                   exsr      EXEERR
028400170609     C                   endif
028500170609     C*
028600170609     C                   endif
028700080829
028800161214        except fine   ;
028900161214
029000080317        *inlr ='1'   ;
029100170609
029200170609
029300170609
029400170609     C*------------------------------------------------------------------------*
029500170609     C* EXEERR - Routine di esecuzione azioni su Errore
029600170609     C*------------------------------------------------------------------------*
029700170609     C     EXEERR        BEGSR
029800170609     C*
029900170609     C                   dump(A)
030000170609     C                   rolbk(e)
030100170609     C*
030200170609     C                   ENDSR
030300170609     C*------------------------------------------------------------------------*
030400170609
030500170609
030600080828
030700110913       //-------------------------------------------------------------------
030800161102        BEGSR   Ctrsel  ;
030900161102
031000161102        Wok='N'  ;
031100161102        Wfine=' '  ;
031200161209        Wnic =' '  ;
031300161216        clear wesARB  ;
031400161227        clear evemem  ;
031500161227        clear evesalta;
031600161227        clear conta   ;
031700161212
031800161212        // Escludo le bolle con CTM 4 e 4F
031900161212        if arbctm ='4 ' or arbctm='4F' ;
032000161212          WOK='N'  ;
032100161212          leavesr  ;
032200161212        endif  ;
032300161216
032400161216        // Escludo le bolle consegnate o non consegnate se richiesto
032500161216        //  verifico la data consegna da ARB se non è dirottata
032600161216        if vidcons<>' '  and arbcca<>'1'  ;
032700161216        chain  (arbaas:arblnp:arbnrs:arbnsp)   fnarb01l     ;
032800161216          if %found(fnarb01l)  ;
032900161216           wesARB='S'  ;
033000161216          else  ;
033100161216           wesARB='N'  ;
033200161216          endif  ;
033300161216        endif  ;
033400161216
033500161216        if wesarb<>'S'   ;
033600161216        if vidcons='S' and arbdcm=0 or vidcons='N' and arbdcm>0 ;
033700161216          WOK='N'  ;
033800161216          leavesr  ;
033900161216        endif  ;
034000161216        else  ;
034100161216        if vidcons='S' and a_arbdcm=0 or vidcons='N' and a_arbdcm>0 ;
034200161216          WOK='N'  ;
034300161216          leavesr  ;
034400161216        endif  ;
034500161216        endif  ;
034600161212
034700161212        // escludo le bolle "figlie"
034800161212        chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl01l  ;
034900161212        if %found(fnlbl01l)  ;
035000161212          WOK='N'  ;
035100161212          leavesr  ;
035200161212        endif  ;
035300161102
0354001611021       if (arbaas*10000)+arbmgs>comdsa ;
035500161102        Wfine='S'  ;
035600161102        leavesr ;
0357001611021       endif  ;
035800161102
035900161102        // controllo cliente e serie
0360001611021       if arbccm>0  ;
036100161102         wccm= arbccm  ;
036200161102        else  ;
036300161102         wccm=arbksc  ;
0364001611021       endif  ;
036500161102
036600161102        xx= 1 ;
036700161212        clear wabi  ;
036800161102
036900161102 3      dow xx<= 20  ;
037000161102 4      if lnp(xx) >0  ;
037100161212 4a     if lnp(xx) = arblnp  ;
037200161102
037300161212        if cli(xx)>0 and wccm<>cli(xx)  ;
037400161212        chain (cli(xx):wccm) fiabk01l  ;
037500161212          if %found(fiabk01l) ;
037600161212           eval wabi='S'  ;
037700161213       //   else   ;
037800161213       //     chain (wccm) fiabk02l  ;
037900161213       //     if %found(fiabk02l) ;
038000161213       //      eval wabi='S'  ;
038100161213       //     endif  ;
038200161212          endif  ;
038300161212        endif  ;
038400161212
038500161212 5      if cli(XX)=0 or wccm= cli(xx) or wabi='S'         ;
038600161102
038700161102        if   nrs(xx)='99'   ;
038800161102         WOK='S'  ;
038900161102          leavesr  ;
039000161102        endif   ;
039100161102
039200161102 6      if   nrs(XX)='SI' and arbnrs>0    ;
039300161102         WOK='S'  ;
039400161102          leavesr  ;
039500161102 x6     else  ;
039600161102
039700161102        alfanrs=%editc(arbnrs:'X')  ;
039800161102 7       if alfanrs= nrs(xx)  ;
039900161102         WOK='S'  ;
040000161102          leavesr  ;
040100161102 7       endif  ;
040200161102 6      endif  ;
040300161102
040400161102 5      endif  ;
040500161102 4      endif  ;
040600161212 4a     endif  ;
040700161102
040800161102        xx=xx+1  ;
040900161102 3      enddo  ;
041000161102
041100161102
041200161102
041300161102        ENDSR  ;
041400161102       //-------------------------------------------------------------------
041500161102       BEGSR  Elabora                ;
041600161207
041700161209       ex88=' '   ;
041800161209       clear wdse;
041900161209       clear                   DISfgs     ;
042000161209       clear                   DISdata    ;
042100161209       clear                   DISnpg     ;
042200161209       clear                   DISora     ;
042300161209       clear                   IMPdata    ;
042400161209       clear                   IMPfgs     ;
042500161209       clear                   IDnpg      ;
042600161209       clear                   IDdata     ;
042700161209       clear                   IDora      ;
042800161209       clear                   IDfgs      ;
042900161209       clear evedt    ;
043000161209       clear evedes   ;
043100161209       clear evedim   ;
043200161209       clear evehim   ;
043300161209
043400161207       clear wfkpi000  ;
043500170525
043600170525       // Lettura eventi
043700170525       exsr leggi_eve  ;
043800161207
043900161209       // calcolo ore reali di consegna tra data sped e data consegna
044000161207       exsr cal_gg    ;
044100161212
044200161216
044300161212       // Se le bolle OK non le voglio --> salto
0444001612120      if vidok <>'N' or  kpidly_c>0  ;
044500161207
044600161212       // se si vogliono gli eventi anche bolle ok elaboro
0447001612120a     if kpidly_c > 0  or vidokeve<>'N' ;
044800161102
044900161102       setll (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
045000161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0451001611021      dow  not %eof(fnblt01l)  ;
045200161214       clear memdse  ;
045300161102
045400161214       // verifico se presente una spunta 88/90
0455001612142      if bltdse>arbdbr or bltdse=0 or bltnpp=88 or bltnpp=90  ;
045600161102       setll  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
045700161102       reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
0458001611023      dow not %eof(fnbrv09l)  ;
045900161214
046000161214       // per testimoniare la presenza del collo metto qualsiasi data
046100161214       // entrata, disguido, arrivo. Quindi tratto la pistola 88 e 90
046200161214       //  allo stesso modo
046300161214
0464001612144       if brvnps=88 or brvnps=90 ;
046500161102         ex88='S'   ;
0466001612145        if  bltdse>0  ;
046700161214           memdse=bltdse;
046800161214x5       else  ;
046900161214          chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0470001612146         if %found(fnart01l) ;
0471001612147         if artdet>0 and artdet<artdam ;
047200161214           memdse=artdet;
047300161214x7        else   ;
0474001612148         if artdam>0  ;
047500161214           memdse=artdam;
0476001612148         endif  ;
0477001612147         endif  ;
0478001612146         endif  ;
0479001612145         endif  ;
048000161214
048100161214 4       endif  ;
048200161214
048300161214        if   memdse>0  ;
048400161214         leave ;
048500161214        endif  ;
048600161214
0487001612144       //if brvnps=90 ;
048800161214        //  chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0489001612145       // if artdam>bltdse  ;
049000161214        //  ex88='S'   ;
049100161214        //  wdse=bltdse;
049200161214        //  leave  ;
0493001612145       // endif  ;
0494001612144       //endif  ;
049500161103
049600161102
0497001611023      reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
049800161102       enddo  ;
0499001611022      endif   ;
050000161209
050100161214       // se era spunta in 88 memorizzo la più recente come data ;
050200161214       if memdse>wdse  ;
050300161214         wdse=memdse  ;
050400161214       endif  ;
050500161214
050600161214       // Verifico se in IMP/Disguido ne basta uno per bolla
050700161214       if idnpg=' '  ;
050800161214         exsr IMPDISG  ;
050900161214       endif  ;
051000161102
051100161102
051200161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0513001611021      enddo  ;
051400161102
051500161209       // verifico se la trazione è ritardo , usando la data arrivo ult collo
051600161216       if wesARB='S'         ;
051700161103       endif  ;
051800161209
0519001612270a     endif  ;
052000161103
052100161102       exsr   Scrivifile  ;
0522001612120      endif  ;
052300161212
052400161102       ENDSR   ;
052500161209       //-------------------------------------------------------------------
052600161209       BEGSR  Leggi_eve              ;
052700161209
052800161209       // Lettura eventi che "servono"
052900161209       clear YY  ;
053000161227       clear indx;
053100161209
053200161209       setll  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
053300161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
053400161209  1    dow  not %eof(fnevb01l)   ;
053500161209
053600161209  2    if %lookup(evbcev:cev)>0   ;
053700161209
053800161227       // Se si tratta di MIC, memorizzo quello con data e ora più alta
053900161227  3    if evbcev='MIC'   ;
054000161227       indx=%lookup(evbcev:evedes)  ;
054100161227  4    if indx>0 ;
054200161227        clear evedt(indx)   ;
054300161227        clear evedes(indx)   ;
054400161227        clear evedim(indx)   ;
054500161227        clear evehim(indx)   ;
054600161227  4    endif  ;
054700161227  3    endif;
054800161227
054900161209       yy=yy+1  ;
055000161227
055100161227  3    if yy<32   ;
055200161209
055300161209        evedt(yy)=evbdev   ;
055400161209        evedes(yy)=evbcev   ;
055500161209        evedim(yy)=evbdtv   ;
055600161209        evehim(yy)=evborv   ;
055700161209
055800161209       // e si tratta di NIC memorizzo anche data e ora immissione
055900161209       // lo elimino se trovo un MIC successivo
056000161209  4     if evbcev='NIC' ;
056100161209         eval wnic='S'   ;
056200161209  4     endif  ;
056300161209  3     endif  ;
056400161209
056500161227  2     endif  ;
056600161209
056700161209        // solo se presente NIC verifico se da eliminare
056800161209   3      if wnic='S' and evbcev='MIC'    ;
056900161209
057000161227        ee=1   ;
057100161227   4    dow ee<=32  ;
057200161227   5    if evedes(ee)='NIC' ;
057300161209
057400161209   6    select  ;
057500161227        when evedt(ee)<>evbdev ;
057600161209
057700161227        when evedim(ee)<evbdtv ;
057800161227        clear evedt(ee)   ;
057900161227        clear evedes(ee)   ;
058000161227        clear evedim(ee)   ;
058100161227        clear evehim(ee)   ;
058200161227        wnic=' '   ;
058300161209
058400161227        when evedim(ee)=evbdtv and evehim(ee)<evborv ;
058500161227        clear evedt(ee)   ;
058600161227        clear evedes(ee)   ;
058700161227        clear evedim(ee)   ;
058800161227        clear evehim(ee)   ;
058900161227        wnic=' '   ;
059000161209        endsl  ;
059100161209
059200161209   5    endif   ;
059300161209
059400161227        ee=ee+1  ;
059500161209   4    enddo  ;
059600161209
059700161209   3      endif  ;
059800161209
059900161209
060000161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
060100161209   1   enddo  ;
060200161209
060300161227       // il MIC lo tengo solo se è l'ultimo degli eventi altrimenti
060400161227       // lo emimino
060500161227       indx=%lookup('MIC':evedes)  ;
060600161227  1    if indx>0 ;
060700161227       yy  =indx+1  ;
060800161227  2    dow yy<=32   ;
060900161227  3     if  evedes(yy)<>*blanks   ;
061000161227         clear evedt(indx)   ;
061100161227         clear evedes(indx)   ;
061200161227         clear evedim(indx)   ;
061300161227         clear evehim(indx)   ;
061400161227         leave   ;
061500161227  3     endif  ;
061600161227
061700161227       yy=yy+1  ;
061800161227  2    enddo  ;
061900161227  1    endif  ;
062000161227
062100161227       // conto anche quanti eventi ci sono per memorizzare solo gli
062200161227       // ultimi 10
062300161227       yy=1  ;
062400161227       clear conta   ;
062500161227       dow yy<=32                ;
062600161227         if  evedes(yy)<>*blanks   ;
062700161227          conta=conta+1  ;
062800161227         endif  ;
062900161227       yy=yy+1  ;
063000161227       enddo  ;
063100161227
063200161227       clear   evesalta  ;
063300161227       if conta>10  ;
063400161227       evesalta=conta-10  ;
063500161227       endif   ;
063600161227
063700161209       ENDSR   ;
063800161102       //-------------------------------------------------------------------
063900161102       BEGSR  Scrivifile             ;
064000111116
064100110913
064200161102       kpiidute=knmeb   ;
064300161102       kpiepo=dutpou  ;
064400110913
064500161102       kpiccm=wccm    ;
064600161102       kpirsm=arbrsm  ;
064700161102
064800161102       kpiaas=arbaas  ;
064900161102       kpilnp=arblnp  ;
065000161102       kpimes=%int(%subst(%editc(arbmgs:'X'):1:2))  ;
065100161102       kpirmn=arbrmn  ;
065200161102       kpirma=arbrma  ;
065300161102       kpinrs=arbnrs  ;
065400161102       kpinsp=arbnsp  ;
065500161102       kpimgs=arbmgs  ;
065600161102       kpilna=arblna  ;
065700161102       kpirsd=arbrsd  ;
065800161102       kpilod=arblod  ;
065900161102       kpiprd=arbprd  ;
066000161102       kpicad=arbcad  ;
066100161215       kpifin=arbfin  ;
066200161102       kpinzd=arbnzd  ;
066300161102       kpiffd=arbffd   ;
066400161102       kpitc1=arbtc1   ;
066500161102       kpitc2=arbtc2   ;
066600161102       kpigc1=arbgc1   ;
066700161102       kpigc2=arbgc2   ;
066800161102       kpincl=arbncl   ;
066900161102       kpipkb=arbpkb   ;
067000161102       kpivlb=arbvlb   ;
067100161102       kpitcr=arbtcr   ;
067200161102       kpidcr=arbdcr   ;
067300161102       kpidcp=arbdcp   ;
067400161102       kpidcm=arbdcm   ;
067500161102       kpihmc=arbhmc   ;
067600161209       kpicca=arbcca   ;
067700161209
067800161216       if arbdcm=0 and wesarb='S' and a_arbdcm>0 and arbcca<>'1';
067900161209       kpidcm=a_arbdcm   ;
068000161209       kpihmc=a_arbhmc   ;
068100161209       kpicca=a_arbcca   ;
068200161209       endif  ;
068300161209
068400161209       if kpicca<>' '  ;
068500161209       tblkey=kpicca   ;
068600161209       chain (1:'7O':tblkey) tabel  ;
068700161209         if %found(tabel00f )  ;
068800161209          kpicca_des=tbluni   ;
068900161209         endif  ;
069000161209       endif  ;
069100161103
069200161103        // pistola ex 88
069300161102        if ex88='S'     ;
069400161102        kpip88='S'      ;
069500161103        kpidse=wdse     ;
069600161102        endif           ;
069700161103
069800161103        //IMP disguido
069900161103        if IDnpg<>' '   ;
070000161103           if IDnpg='P'   ;
070100161103           kpiimpds='P'   ;
070200161103           else  ;
070300161103           kpiimpds='D'   ;
070400161103           endif ;
070500161103        endif ;
070600161209
070700161209       yy=1  ;
070800161227       clear evemem   ;
070900161209
071000170525 1     dow yy<=32               ;
071100161227 2     if  evedes(yy)<>*blanks   ;
071200170524
071300170524       // Verifico se data evento uguale alla data prevista consegna
071400170524 2a    if kpidce>0 ;
071500170524
071600170525 3     if evedt(yy)<=kpidce and kpidcesn<>'S'  ;
071700170524          indx= %lookup(evedes(yy):cev) ;
071800170524 4     if cevt(indx)='E';
071900170524        kpidcesn='S'  ;
072000170524 4     ENDIF;
072100170524       // memorizzo diversamente se c'è un evento particolare
072200170525 4     if cevt(indx)='P' and  evedt(yy) =kpidce ;
072300170524        kpidcesn='P'  ;
072400170524 4     ENDIF;
072500170524
072600170524 3     ENDIF;
072700170524 2a    ENDIF;
072800161227
072900161227       evemem=evemem+1  ;
073000161227
073100161227       // se conta < 10 memorizzo tutti altrimenti salto i primi
073200161227
073300170525 2a    if kpidly_c > 0  or vidokeve<>'N' ;
073400161227 3     if conta <=10 or evemem>evesalta  ;
073500161227 4      select   ;
073600161227         when KPICEV1=*blanks and evedes(yy)<>'MIC';
073700161209         KPICEV1=evedes(yy)  ;
073800161209         KPIDEV1=evedt(yy)  ;
073900161227         when KPICEV2=*blanks and evedes(yy)<>'MIC';
074000161209         KPICEV2=evedes(yy)  ;
074100161209         KPIDEV2=evedt(yy)  ;
074200161227         when KPICEV3=*blanks and evedes(yy)<>'MIC';
074300161209         KPICEV3=evedes(yy)  ;
074400161209         KPIDEV3=evedt(yy)  ;
074500161227         when KPICEV4=*blanks and evedes(yy)<>'MIC';
074600161209         KPICEV4=evedes(yy)  ;
074700161209         KPIDEV4=evedt(yy)  ;
074800161227         when KPICEV5=*blanks  and evedes(yy)<>'MIC';
074900161209         KPICEV5=evedes(yy)  ;
075000161209         KPIDEV5=evedt(yy)  ;
075100161227         when KPICEV6=*blanks  and evedes(yy)<>'MIC';
075200161209         KPICEV6=evedes(yy)  ;
075300161209         KPIDEV6=evedt(yy)  ;
075400161227         when KPICEV7=*blanks  and evedes(yy)<>'MIC';
075500161227         KPICEV7=evedes(yy)  ;
075600161209         KPIDEV7=evedt(yy)  ;
075700161227         when KPICEV8=*blanks  and evedes(yy)<>'MIC';
075800161209         KPICEV8=evedes(yy)  ;
075900161209         KPIDEV8=evedt(yy)  ;
076000161227         when KPICEV9=*blanks  and evedes(yy)<>'MIC';
076100161209         KPICEV9=evedes(yy)  ;
076200161209         KPIDEV9=evedt(yy)  ;
076300161209         when KPICEV0=*blanks  ;
076400161209         KPICEV0=evedes(yy)  ;
076500161209         KPIDEV0=evedt(yy)  ;
076600161227 4       endsl  ;
076700161227 3       endif  ;
076800161227 2       endif  ;
076900170525 2a      endif  ;
077000161209
077100161209         yy=yy+ 1;
077200161209
077300161227 1     enddo  ;
077400170523
077500170523        // Se la data consegna prevista corrisponde alla data cons merce--> ok
077600170526 1      if kpidcm>0 and kpidce>0 and kpidcesn<>'S' ;
077700170526        // utilizzo la data  di consegna merce di arb se esiste per le bolle
077800170526        //  legate
077900170526
078000170526 2        if wesarb='S' and a_arbdcm<=kpidce ;
078100170526            kpidcesn='S' ;
078200170526 x2       else  ;
078300170526 3        if kpidcm<=kpidce ;
078400170523            kpidcesn='S' ;
078500170526 3        ENDIF;
078600170526 2      ENDIF;
078700170526 1      ENDIF;
078800170526 1          if kpidcesn=' '  and kpidce>0 ;
078900170525            kpidcesn='N' ;
079000170526 1          ENDIF;
079100110916
079200161102       write  wfkpi000 ;
079300170609       exsr   WRITIVGD ;
079400110913
079500161214       CONTAB=CONTAB+1   ;
079600110926
079700110913       ENDSR                  ;
079800170609
079900170609
080000170609
080100170609     C*------------------------------------------------------------------------*
080200170609     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
080300170609     C*------------------------------------------------------------------------*
080400170609     C     WRITIVGD      BEGSR
080500170609     C*
080600170609     C                   clear                   tivgd000
080700170609     C                   eval      vgdDTA = %subst(DS_KPI:1:%size(DS_KPI))
080800170609     C                   eval      vgdTIP = '&1'
080900170609     C                   eval      vgdKSU = '0BRT0'+%editc(dutpou:'X')
081000170609     C                   eval      vgdTSC = 'WW'
081100170609     C                   eval      vgdDAT = dateu
081200170609     C                   eval      vgdPRG = o§7VASPRG
081300170609     C                   eval      vgdPGM = procname
081400170609     C                   eval      vgdSTO = '?'
081500170609     C                   write     tivgd000
081600170609     C*
081700170609     C                   ENDSR
081800170609     C*------------------------------------------------------------------------*
081900170609
082000170609
082100170609
082200080327       //-------------------------------------------------------------------
082300080327       BEGSR  *INZSR                 ;
082400080328
082500080328       in(E) *dtaara   ;
082600080328       if    %error or rsut  =*blanks  ;
082700080328       callp TIBS34R (TIBS34DS)        ;
082800080328       in    *dtaara   ;
082900080328       endif                           ;
083000080829
083100110613       // imposto data e ora del giorno
083200110613       dataiso=datasys  ;
083300161214       dataeur=datasys  ;
083400110613       dateu=%dec(dataiso)   ;
083500161214       dataogg=%dec(dataeur)   ;
083600110616
083700110613       ora_iso=ora_sys  ;
083800110613       ora_u=%dec(ora_iso)   ;
083900080829
084000110913       // Se richiesto file pulisco il lancio precedente per filiale
084100161102       setll (dutpou)   wfKPI01l   ;
084200161102       reade (dutpou)   wfKPI01l   ;
084300110616
084400161102       dow  not %eof(wfKPI01l)   ;
084500161102       delete wfKPI000  ;
084600110616
084700161102       reade (dutpou)   wfKPI01l   ;
084800110616       enddo  ;
084900110616
085000161209       clear xx ;
085100161209       setll  (1:'2A') tabel00f ;
085200161209       reade  (1:'2A') tabel00f ;
085300161209
085400161209       dow not %eof(tabel00f ) ;
085500161209       if tblflg=' '  ;
085600161209        ds2a = tbluni   ;
085700161209
085800161209       if   §2aftg='S' or §2atip='N' or §2aftc='A'  ;
085900161209       xx=xx+1  ;
086000161209        cev(xx)=%Subst(tblkey:1:3)  ;
086100170524        if §2antc='S' ;
086200170524          cevt(xx)='E' ;
086300170524        ENDIF;
086400161209       endif  ;
086500161209       endif  ;
086600161209
086700161209       reade  (1:'2A') tabel00f ;
086800161209       enddo  ;
086900161209
087000161209       setll  (1:'2Z') tabel00f ;
087100161209       reade  (1:'2Z') tabel00f ;
087200161209
087300161209       dow not %eof(tabel00f ) ;
087400161209       if tblflg=' '  ;
087500161209        ds2z = tbluni   ;
087600161209
087700161209       if   §2zcep<>' '  ;
087800161209       xx=xx+1  ;
087900161209        cev(xx)=%subst(tblkey:1:3)  ;
088000170523        cevt(xx)='P' ;
088100161209       endif  ;
088200161209       endif  ;
088300161209
088400161209       reade  (1:'2Z') tabel00f ;
088500161209       enddo  ;
088600161209
088700161209       // infine aggiungo fisso il NIC
088800161209       xx=xx+1  ;
088900161209       cev(xx)='NIC' ;
089000161227       xx=xx+1  ;
089100161227       cev(xx)='MIC' ;
089200161209
089300161209       ENDSR     ;
089400161102       //-------------------------------------------------------------------
089500161209       BEGSR  PrepaLNP               ;
089600161102
089700161102       clear  LnpSel  ;
089800161102       xx= 1 ;
089900161102       yy= 1 ;
090000161102
090100161102       dow xx <=20  ;
090200161102        if lnp(xx)>0  ;
090300161102         Indx=%lookup(lnp(xx):LnpSel)  ;
090400161102         if Indx=0  ;
090500161102           eval lnpSel(yy)=lnp(xx)  ;
090600161102           yy=yy+1   ;
090700161102         endif  ;
090800161102
090900161102        endif  ;
091000161102
091100161102        xx=xx+1   ;
091200161102        enddo     ;
091300161102
091400161102        // Memorizzo nel campo oggcli il codice cliente che devo
091500161102        //  mettere nell'ogggetto della mail
091600161212        // clear oggcli  ;
091700161212        // if cli(1)>0   ;
091800161212        //   oggcli=cli(1)  ;
091900161212        // endif  ;
092000161102
092100161102       ENDSR  ;
092200161207
092300161103
092400161103      /end-free
092500161103
092600161207     c*-------------------------------------------------------------------
092700161207     c     Cal_gg        BEGSR
092800161207     c
092900161209     c     karb          klist
093000161209     c                   kfld                    arbaas
093100161209     c                   kfld                    arblnp
093200161209     c                   kfld                    arbnrs
093300161209     c                   kfld                    arbnsp
093400170524     c     kar5          klist
093500170524     c                   kfld                    arbaas
093600170524     c                   kfld                    arblnp
093700170524     c                   kfld                    arbnrs
093800170524     c                   kfld                    arbnsp
093900170524     c                   kfld                    ktrd
094000161209
094100161216     c* chain solo se non l'ho già fatto prima
094200161216     c                   if        wesarb=' '
094300161209     c     karb          chain     fnarb01l
094400170105     c                   if        %found(fnarb01l)
094500170105     c                   eval      wesARB='S'
094600170105     c                   else
094700170105     c                   eval      wesARB='N'
094800170105     c                   endif
094900161216     c                   endif
095000170524
095100170524     c     kar5          chain     fiar501l
095200170524     c                   if        %found(fiar501l)
095300170524     c                   eval      dar5gen=ar5uni
095400170524     c                   else
095500170524     c
095600170524     c                   clear                   dar5gen
095700170524     c                   endif
095800170525
095900170525     c* prendo il primo evento pieno "NO MIC/NIC"
096000170525     c                   clear                   dtprimoeve
096100170525     c                   eval      yy=1
096200170525     c                   dow       yy<=10 and dtprimoeve=0
096300170525     c                   if        evedes(yy)<>*blanks and
096400170525     c                             evedes(yy)<>'MIC' and
096500170525     c                             evedes(yy)<>'NIC'
096600170525     c                   EVAL      DTPRIMOEVE=EVEDT(YY)
096700170525     c                   ENDIF
096800170525     c                   ADD       1             YY
096900170525     c                   ENDDO
097000170525
097100161216
097200161207     c                   clear                   tnsd99ds
097300161207     C                   MOVEL     ' '           D98TLA
097400161216     c                   if        wesARB='S'
097500161207     C                   MOVEL     'A'           D98TBO
097600161209     c                   else
097700161209     C                   MOVEL     'P'           D98TBO
097800161209     c                   endif
097900161207     C                   MOVEL     ARBAAS        D98AAS
098000161207     C                   MOVEL     ARBLNP        D98LNP
098100161207     C                   MOVEL     ARBNRS        D98NRS
098200161207     C                   MOVEL     ARBNSP        D98NSP
098300161207     c
098400161207     c                   if        arbtsp='D'
098500161207     c                   eval      I98TSP_FOR='C'
098600161207     c                   endif
098700170524     c
098800161207     C*
098900161207     C                   CALL      'TNSD99R'
099000161207     C                   PARM                    TNSD99DS
099100161207     c
099200161207     c* ore teoriche
099300161209     c                   eval      KPIdly_T=d98trc
099400161209     c                   eval      KPIdag  =d98dag
099500161209     c                   eval      KPIdcg  =d98dcg
099600161215     c                   eval      KPIdac  =d98dac
099700161215     c                   eval      KPIdcc  =d98dcc
099800161207
099900161207     c* Richiamo il pgm utilizzando la DS eliminando il num sped
100000161207     c*  e la giacenza
100100161207     c                   clear                   d98nsp
100200161207     c                   clear                   d98dti
100300161207     c                   clear                   d98hti
100400161207     c                   clear                   d98cmc
100500161207     c                   clear                   d98dag
100600161207     c                   clear                   d98dcg
100700161207     c                   eval      d98duc=d98dsp
100800161207     c                   eval      d98dbr=d98dsp
100900170105     c                   eval      D98FDRT='S'
101000161207     c                   clear                   d98dam
101100170524     c* considero i giorni di chiusura se inseriti in partenza
101200170524     c                   if        §ar5mdp='S'
101300170524     c                   eval      d98gc1=§AR5GC1
101400170524     c                   eval      d98gc2=§AR5GC2
101500170524     c                   else
101600170524     c                   clear                   d98gc1
101700170524     c                   clear                   d98gc2
101800170524     c                   endif
101900170524
102000170524     c* la consegna richiesta la considero se inserita in partenza
102100170524     c*  o in arrivo prima del primo tentativo
102200170524     c                   if        d98dcr>0 and (§ar5mdp='S' and
102300170524     c                             §ar5dcr<>%editc(d98dcr:'X'))
102400170525     c**                   clear                   datadcr
102500170525     c**     karb          setll     fiarbf2c
102600170525     c**     karb          reade     fiarbf2c
102700170525     c**                   dow       not %eof(fiarbf2c)
102800170525     c**                   if        %subst(v_arbcvb:1:1)='C'
102900170525     c**                             and v_arbcvb<>'CP'
103000170525     c**                   eval      datadcr=v_arbdtv
103100170525     c**                   leave
103200170525     c**                   endif
103300170524     c
103400170525     c**     karb          reade     fiarbf2c
103500170525     c**                   enddo
103600170525     c*
103700170525     c                   if        d98dcr>dtprimoeve
103800170524     c                   clear                   d98dcr
103900170524     c                   clear                   d98hcr
104000170524     c                   clear                   d98tcr
104100170524     c                   endif
104200170524     c                   endif
104300170525     c* Se il fermo deposito è stato inserito dopo il primo tentativo
104400170525     c*  NON lo considero
104500170525     c                   if        d98ffd='S'   and
104600170525     c                             dtprimoeve>0  and
104700170525     c                             §ar5dfd>*zeros and
104800170525     c                             %int(§ar5dfd)>=dtprimoeve
104900170525     c                   clear                   d98ffd
105000170525     c                   endif
105100170525     c
105200170524     c
105300161207     C                   CALL      'TNSD99R'
105400161207     C                   PARM                    TNSD99DS
105500161207     c
105600161207     c* controllo le ore reali  con le ore teoriche
105700161209     c                   eval      KPIdly_R=d98rrc
105800170523     c* Memorizzo la data prevista consegna
105900170523     c                   eval      KPIdce= d98dce
106000170524     c* Se il fermo deposito è stato inserito dopo il primo tentativo
106100170524     c*  NON lo considero
106200170524     c                   if         d98ffd='S'
106300170525     c                   if        dtprimoeve=0  or
106400170524     c                             §ar5dfd<=*zeros or
106500170525     c                             %int(§ar5dfd)< dtprimoeve
106600170524     c                   clear                   kpidce
106700170524     c                   endif
106800170524     c                   endif
106900161215     c
107000170523     c* se la spedizione è borderizzata in data festiva --> aggiungo 24 ore
107100161215     c*  alle ore reali
107200170105     c* ES 05/1/17 non lo faccio più in quanto  ho modificato allo stesso modo
107300170105     c*  il calcolo della affidabilità tnsd99r
107400170105     c**                 if        arbdbr>0
107500170105     c**                 clear                   wdat8
107600170105     c**                 clear                   tfptfa
107700170105     c**                 z-add     arbdbr        datada
107800170105     c*+                 z-add     arbdbr        dataa
107900170105     c**                 z-add     arbtfp        p_tfp
108000170105     c*+                 z-add     arbtfa        p_tfa
108100170105     c**                 call      'XSRLAV8'
108200170105     c**                 parm                    wdat8
108300170105     c**                 parm                    tfptfa
108400161215     c* se giorno FESTIVO aggiungo 24 ore
108500170105     c**                 if        ggl=0
108600170105     c**                 eval      KPIdly_R=KPIdly_R+24
108700170105     c**                 endif
108800161215
108900170105     c**                 endif
109000161215     c
109100161207     c                   if        kpidly_r<=kpidly_t
109200161207     c                   clear                   kpidly_c
109300161207     c                   else
109400161207     c
109500161207     c                   eval      kpidly_c =kpidly_r-kpidly_t
109600161207     c                   endif
109700161207     c*
109800161213     c* CONSEGNA RICHIESTA IL
109900161212     c* Se data richiesta = data consegna merce
110000161209     c*  sempre OK
110100161212     c**                 if        arbtc1='A' or arbtc2='A'  or
110200161212     c**                           arbtc1='S' or arbtc2='S'
110300161213     c                   if        arbdcr>0 and arbtcr=' '
110400161213     c                   if        arbdcr=arbdcm
110500161209     c                   clear                   kpidly_c
110600161209     c                   endif
110700161213     c* se bolla NON consegnata e data cons richiesta > di oggi
110800161213     c* per ora ON TIME
110900161213     c                   if        arbdcm=0 and arbdcr>=dateu
111000161213     c                   clear                   kpidly_c
111100161213     c                   endif
111200161213     c                   endif
111300161213     c
111400161212     c**                 endif
111500161213
111600161213     c* CONSEGNA RICHIESTA PRIMA DEL
111700161213     c                   if        arbdcr>0 and arbtcr='P'
111800161213     C                   IF        ARBDCM>0 AND ARBDCM<ARBDCR
111900161213     c                   clear                   kpidly_c
112000161213     c                   endif
112100161213     C                   IF        ARBDCM=0
112200161213     c                   clear                   kpidly_c
112300161213     c                   endif
112400161213     c                   endif
112500161213     c* CONSEGNA RICHIESTA DOPO IL
112600161213     c                   if        arbdcr>0 and arbtcr='D'
112700161213     C                   IF        ARBDCM>0 AND ARBDCM>ARBDCR
112800161213     c                   clear                   kpidly_c
112900161213     c                   endif
113000161213     C                   IF        ARBDCM=0
113100161213     c                   clear                   kpidly_c
113200161213     c                   endif
113300161213     c                   endif
113400161207
113500161212     c* se cons anomala RESO o DIROTTAMENTO --> sempre ok
113600161212     c                   if        arbcca='1' or arbcca='2' or
113700161212     c                             arbcca='6'
113800161212     c                   clear                   kpidly_c
113900161212     c                   endif
114000161207     c*
114100161207     c                   ENDSR
114200161103     c*-------------------------------------------------------------------
114300161103     c     IMPdisg       BEGSR
114400161209     c
114500161209     c     kbrv          Klist
114600161209     c                   Kfld                    bltfls
114700161209     c                   Kfld                    bltlna
114800161209     c                   Kfld                    bltnrs
114900161209     c                   Kfld                    bltnsc
115000161103     c
115100161103     c* per ogni collo verifico se in IMP o disguidato
115200161103     c* prendo la data più alta
115300161103     c     kbrv          setll     fnbrv07l
115400161103     c     kbrv          reade     fnbrv07l
115500161103    2c                   dow       not %eof(fnbrv07l)
115600161103    3c                   if        brvatr='D'  or brvnpg=3
115700161103     c* prendo la data della spunta
115800161103     c                   clear                   fnlv53ds
115900161103     C                   MOVEL     BRvNPG        D53NPG
116000161103     C                   Z-ADD     BRvNFV        D53NFV
116100161103     C                   MOVEL     BRvFGS        D53FGS
116200161103     C                   CALL      'FNLV53R'
116300161103     C                   PARM                    fnlv53ds
116400161103    4c                   if        d53err<>*blanks
116500161103    5c                   if        brvdcs>0
116600161103     c                   eval      d53dfv=brvdcs
116700161103     c                   else
116800161103    6c                   if        brvdfs>0
116900161103     c                   eval      d53dfv=brvdfs
117000161103    6c                   endif
117100161103    5c                   endif
117200161103    4c                   endif
117300161103     c
117400161103    4c                   select
117500161103     c* se si tratta di DISGUIDO
117600161103     c                   when      brvatr='D'
117700161103     c
117800161103    5c                   if        d53dfv=DISdata
117900161103     c* Verifico l'ora
118000161103    6c                   if        brvhcs>0 and brvhcs>DISora
118100161103     c                   eval      DISdata=d53dfv
118200161103     c                   eval      DISfgs =brvfgs
118300161103     c                   eval      DISnpg =brvnpg
118400161103     c                   eval      DISora =brvhcs
118500161103   x6c                   else
118600161103    7c                   if        brvhms>0 and brvhms>DISora
118700161103     c                   eval      DISdata=d53dfv
118800161103     c                   eval      DISfgs =brvfgs
118900161103     c                   eval      DISnpg =brvnpg
119000161103     c                   eval      DISora =brvhms
119100161103    7c                   endif
119200161103    6c                   endif
119300161103    5c                   endif
119400161103     c
119500161103    5c                   if        d53dfv>DISdata
119600161103     c                   eval      DISdata=d53dfv
119700161103     c                   eval      DISfgs =brvfgs
119800161103     c                   eval      DISnpg =brvnpg
119900161103     c                   eval      DISora =brvhcs
120000161103     c                   if        disora=0
120100161103     c                   eval      DISora =brvhms
120200161103     c                   endif
120300161103    5c                   endif
120400161103     c
120500161103     c* se si tratta di IMP
120600161103    4c                   when      d53spg='P' and d53dfv>impdata
120700161103     c                   eval      IMPdata=d53dfv
120800161103     c                   eval      IMPfgs =brvfgs
120900161103    4c                   endsl
121000161103    3c                   endif
121100161103
121200161103     c     kbrv          reade     fnbrv07l
121300161103    2c                   enddo
121400161103
121500161103     c* memorizzo la data più alta
121600161103     c* IMP vince sul disguido per stessa data, senza guardare l'ora
121700161103    2c                   if        IMPdata>0 and IMPdata>=IDdata
121800161103     c                   eval      IDdata=IMPdata
121900161103     c                   eval      IDfgs=IMPfgs
122000161103     c                   eval      IDora=0
122100161103     c                   eval      IDnpg='P'
122200161103    2c                   endif
122300161103    2c                   if        (DISdata>0 and DISdata=IDdata
122400161103     c                             and IDnpg<>'P' and DISora>IDora) or
122500161103     c                             (DISdata>0 and DISdata>IDdata)
122600161103     c                   eval      IDdata=DISdata
122700161103     c                   eval      IDfgs=DISfgs
122800161103     c                   eval      IDora=DISora
122900161103     c                   eval      IDnpg=%char(DISnpg)
123000161103    2c                   endif
123100161103     c
123200161103     C                   ENDSR
123300161214     OQSYSPRT   E            fine             02
123400161214     O                                              'Elaborazione esiti consegn'
123500161214     o                                              'a il'
123600161214     o                       dataogg           +  2 '  /  /    '
123700161214     o                       ora_u             +  2 '  :  :  '
123800161214     o                                         +  2 'Bolle scritte :'
123900161214     o                       contab        2   +  2
124000161214**
124100161214OVRPRTF FILE(QSYSPRT) USRDTA('EsitiCons') HOLD(*YES)
