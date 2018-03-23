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
002100171219     F**TIVGD00F  O    E             DISK    USROPN COMMIT
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
010700171218     d conta           s              3  0
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
011900171219     d stainvio        s             48
012000161103     d IMPdata         s                   like(d53dfv)
012100161103     d IMPfgs          s                   like(brvfgs)
012200161103     d DISfgs          s                   like(brvfgs)
012300161103     d DISdata         s                   like(d53dfv)
012400161103     d DISnpg          s                   like(brvnpg)
012500161103     d DISora          s                   like(brvhcs)
012600161103     d IDdata          s                   like(d53dfv)
012700161103     d IDora           s                   like(brvhcs)
012800161103     d IDfgs           s                   like(brvfgs)
012900170524     d ktrd            s                   like(ar5trd) inz('GEN')
013000161103     d IDnpg           s              1
013100161209     d wnic            s              1
013200110613     d wlib            s             10    inz('GAITRAGRPS')
013300110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
013400110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
013500110613     d Ora_iso         s               t   timfmt(*iso)
013600110613     d Dataiso         s               d   datfmt(*iso)
013700111013     d Dataeur         s               d   datfmt(*eur)
013800080327     d Datadmy         s               d   datfmt(*dmy)
013900110916
014000110916     d $MaxElem        c                   const(998)
014100171218     d xlsE            c                   const('Inviato per e-mail')
014200171219     d xlsF            c                   const('Excel posto+
014300171219     d                                      nella cartella disco N/BARTMAIL')
014400080829     d
014500080317      *------------------------------------------------------------------------*
014600080317      *   P R O T O T I P I
014700080317      *------------------------------------------------------------------------*
014800080829      /copy gaitrasrc/srcprotopr,tibs34r
014900110617      /copy gaitrasrc/srcprotopr,trtcm8r1
015000110916      /copy gaitrasrc/srcprotopr,fnlv55r
015100001027     C*---------------------------------------------------------------*
015200001027     C     *ENTRY        PLIST
015300080325     C                   PARM                    kpjba
015400161031     c                   movel     kpjbu         fnlsw7ds
015500170609     C*
015600170609     C* Avvio il controllo sincronia
015700171219     C**                 CLEAR                   TIS7VASDS
015800171219     C**                 EVAL      i§7VASOPZ = 'SCM'
015900171219     C**                 CALL(e)   'TIS7VASR1'
016000171219     C**                 PARM                    TIS7VASDS
016100170609     C*
016200170609     C* Verifico esito chiamata
016300171219     C***                if        %error OR o§7VASOK = *off
016400171219     C**                 dump(A)
016500171219     C**                 seton                                        lr
016600171219     C**                 return
016700171219     C**                 endif
016800161214     C* ESEGUO OVRPRTF
016900161214     C                   Z-ADD     61            LUNG             15 5
017000161214     C                   MOVEL     *BLANKS       COMMAN           80
017100161214     C                   MOVEA     CMD           COMMAN
017200161214     C                   CALL      'QCMDEXC'
017300161214     C                   PARM                    COMMAN
017400161214     C                   PARM                    LUNG
017500161214     C*
017600161214     C                   Z-ADD     1             C                 3 0
017700161214     C                   OPEN      QSYSPRT
017800170609     C*
017900170609     C* Apertura file DOWNLOAD
018000171219     C***                if        not %open(tivgd00f)
018100171219     C***                open      tivgd00f
018200171219     C***                endif
018300171219     C***
018400170609     C* Inizializzo la transazione DOWNLOAD
018500171219     C***                CLEAR                   TIS7VASDS
018600171219     C***                EVAL      i§7VASOPZ = 'PRG'
018700171219     C***                CALL(e)   'TIS7VASR1'
018800171219     C***                PARM                    TIS7VASDS
018900170609     C*
019000170609     C* Se OK => proseguo
019100171219     C***                if        not %error AND
019200171219     C***                          o§7VASOK = *on AND o§7VASPRG <> *blanks
019300171219     C***                movel     'S'           wProcedi          1
019400171219     C***                else
019500171219     C***                movel     'N'           wProcedi
019600171219     C***                endif
019700170609     C*
019800170609     C* Se ok a procedere => elaboro
019900171219     C***                if        wProcedi = 'S'
020000080317      /free
0201001610311      if comdsa=0 ;
020200161031        comdsa=comdsd ;
0203001610311      endif  ;
020400161102
020500161102       // Linee partenza selezionate
020600161102          EXSR  PrepaLNP      ;
020700161212
020800161031
0209001610311      if comdsa=comdsd ;
021000161031         *in01 =*on  ;
0211001610311       endif  ;
021200161102
021300161209       zz=1   ;
021400161031
0215001611021      dow lnpsel(zz)>0  ;
021600161102          setll (lnpsel(zz):kaad:kmgd) fnblp31l ;
021700161031          if *in01=*on  ;
021800161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
021900161031          else  ;
022000161102          reade (lnpsel(zz)) fnblp31l ;
022100161031          endif  ;
022200161031
0223001611022         dow not %eof(fnblp31l)   ;
022400161102
022500161102          exsr   Ctrsel    ;
022600161102
022700161102          if Wfine='S'  ;
022800161102          leave  ;
022900161102          endif  ;
023000161102
023100161102          if wok='S'   ;
023200161102          exsr elabora  ;
023300161102          endif  ;
023400161102
023500161102
023600161102          if *in01=*on  ;
023700161102          reade (lnpsel(zz):kaad:kmgd) fnblp31l ;
023800161102          else  ;
023900161102          reade (lnpsel(zz)) fnblp31l ;
024000161102          endif  ;
0241001611022         enddo  ;
024200161031
024300161102          zz=zz+1  ;
024400171218
024500171218          if zz>20 ;
024600171218          leave ;
024700171218          endif     ;
0248001611021      enddo ;
024900170609     C*
025000170609     C* Chiudo il file di output
025100171219     C***                if        %open(tivgd00f)
025200171219     C***                close     tivgd00f
025300171219     C***                endif
025400170609     C*
025500170609     C* Finalizzo la transazione
025600171219     c***                if        contab<3000
025700171219     C***                EVAL      i§7VASOPZ  = 'RLS'
025800171219     C***                EVAL      i§7VASTIP  = '&1'
025900171219     C***                EVAL      i§7VASKSU  = '0BRT0'+%editc(dutpou:'X')
026000171219     C***                EVAL      i§7VASTSC  = 'WW'
026100171219     C***                EVAL      i§7VASSTO  = '?'
026200171219     C***                EVAL      i§7VASSTTO = 'L'
026300171219     C***                EVAL      i§7VASPRG  = o§7VASPRG
026400171219     C***                CALL(e)   'TIS7VASR1'
026500171219     C***                PARM                    TIS7VASDS
026600170609     C*
026700170609     C* Verifico esito chiamata
026800171219     C***                if        %error OR o§7VASOK = *off
026900171219     C***                exsr      EXEERR
027000171219     C***                endif
027100171218
027200171218        stainvio=xlsE  ;
027300171218     c*
027400171219     c***                else
027500171218     c* oltre vado sul disco n
027600171218        stainvio=xlsF  ;
027700171218
027800171218        pInFILE='WFKPI00F'   ;
027900171218        pInIDJOB=knmeb       ;
028000171218          // Nell'oggetto imposto il primo codice cliente richiesto
028100171218  2     if  oggcli>0   ;
028200171218        chain (1:dutkci:oggcli) cnaco00f  ;
028300171218  3     if not %found(cnaco00f)  ;
028400171218          acorag=*all'?'  ;
028500171218  3     endif  ;
028600171218
028700171219          pinEMLVAR='*OBJM* K.P.I Cliente: '+%editc(oggcli:'X')+ '-'+
028800171218                     %subst(acorag:1:15)  ;
028900171218  x2    else  ;
029000171218          // Imposto la prima lnp/serie richiesta   ;
029100171219         pinEMLVAR='*OBJM* K.P.I. Lnp/Ser:'+%editc(lnp(1):'X')+' '+
029200171218                                       nrs(1)      ;
029300171218  2     endif  ;
029400171218
029500171218        //  pInEMLDEST=%trim(videmail)+'@brt.it' ;
029600171218
029700171218            pInMRAKE2='XLS_F'  ;
029800171218
029900171218        callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
030000171218                        pInEMLVAR:pInMRAKE2:pOutESITO) ;
030100171219        //endif;
030200171218
030300170609     C*
030400171219     C***                endif
030500080829
030600161214        except fine   ;
030700161214
030800080317        *inlr ='1'   ;
030900170609
031000170609     C*------------------------------------------------------------------------*
031100170609     C* EXEERR - Routine di esecuzione azioni su Errore
031200170609     C*------------------------------------------------------------------------*
031300171219     C***  EXEERR        BEGSR
031400170609     C*
031500171219     C***                dump(A)
031600171219     C***                rolbk(e)
031700170609     C*
031800171219     C***                ENDSR
031900170609     C*------------------------------------------------------------------------*
032000080828
032100110913       //-------------------------------------------------------------------
032200161102        BEGSR   Ctrsel  ;
032300161102
032400161102        Wok='N'  ;
032500161102        Wfine=' '  ;
032600161209        Wnic =' '  ;
032700161216        clear wesARB  ;
032800161227        clear evemem  ;
032900161227        clear evesalta;
033000161227        clear conta   ;
033100161212
033200161212        // Escludo le bolle con CTM 4 e 4F
033300161212        if arbctm ='4 ' or arbctm='4F' ;
033400161212          WOK='N'  ;
033500161212          leavesr  ;
033600161212        endif  ;
033700161216
033800161216        // Escludo le bolle consegnate o non consegnate se richiesto
033900161216        //  verifico la data consegna da ARB se non è dirottata
034000161216        if vidcons<>' '  and arbcca<>'1'  ;
034100161216        chain  (arbaas:arblnp:arbnrs:arbnsp)   fnarb01l     ;
034200161216          if %found(fnarb01l)  ;
034300161216           wesARB='S'  ;
034400161216          else  ;
034500161216           wesARB='N'  ;
034600161216          endif  ;
034700161216        endif  ;
034800161216
034900161216        if wesarb<>'S'   ;
035000161216        if vidcons='S' and arbdcm=0 or vidcons='N' and arbdcm>0 ;
035100161216          WOK='N'  ;
035200161216          leavesr  ;
035300161216        endif  ;
035400161216        else  ;
035500161216        if vidcons='S' and a_arbdcm=0 or vidcons='N' and a_arbdcm>0 ;
035600161216          WOK='N'  ;
035700161216          leavesr  ;
035800161216        endif  ;
035900161216        endif  ;
036000161212
036100161212        // escludo le bolle "figlie"
036200161212        chain (arbaas:arblnp:arbnrs:arbnsp) fnlbl01l  ;
036300161212        if %found(fnlbl01l)  ;
036400161212          WOK='N'  ;
036500161212          leavesr  ;
036600161212        endif  ;
036700161102
0368001611021       if (arbaas*10000)+arbmgs>comdsa ;
036900161102        Wfine='S'  ;
037000161102        leavesr ;
0371001611021       endif  ;
037200161102
037300161102        // controllo cliente e serie
0374001611021       if arbccm>0  ;
037500161102         wccm= arbccm  ;
037600161102        else  ;
037700161102         wccm=arbksc  ;
0378001611021       endif  ;
037900161102
038000161102        xx= 1 ;
038100161212        clear wabi  ;
038200161102
038300161102 3      dow xx<= 20  ;
038400161102 4      if lnp(xx) >0  ;
038500161212 4a     if lnp(xx) = arblnp  ;
038600161102
038700161212        if cli(xx)>0 and wccm<>cli(xx)  ;
038800161212        chain (cli(xx):wccm) fiabk01l  ;
038900161212          if %found(fiabk01l) ;
039000161212           eval wabi='S'  ;
039100161213       //   else   ;
039200161213       //     chain (wccm) fiabk02l  ;
039300161213       //     if %found(fiabk02l) ;
039400161213       //      eval wabi='S'  ;
039500161213       //     endif  ;
039600161212          endif  ;
039700161212        endif  ;
039800161212
039900161212 5      if cli(XX)=0 or wccm= cli(xx) or wabi='S'         ;
040000161102
040100161102        if   nrs(xx)='99'   ;
040200161102         WOK='S'  ;
040300161102          leavesr  ;
040400161102        endif   ;
040500161102
040600161102 6      if   nrs(XX)='SI' and arbnrs>0    ;
040700161102         WOK='S'  ;
040800161102          leavesr  ;
040900161102 x6     else  ;
041000161102
041100161102        alfanrs=%editc(arbnrs:'X')  ;
041200161102 7       if alfanrs= nrs(xx)  ;
041300161102         WOK='S'  ;
041400161102          leavesr  ;
041500161102 7       endif  ;
041600161102 6      endif  ;
041700161102
041800161102 5      endif  ;
041900161102 4      endif  ;
042000161212 4a     endif  ;
042100161102
042200161102        xx=xx+1  ;
042300161102 3      enddo  ;
042400161102
042500161102
042600161102
042700161102        ENDSR  ;
042800161102       //-------------------------------------------------------------------
042900161102       BEGSR  Elabora                ;
043000161207
043100161209       ex88=' '   ;
043200161209       clear wdse;
043300161209       clear                   DISfgs     ;
043400161209       clear                   DISdata    ;
043500161209       clear                   DISnpg     ;
043600161209       clear                   DISora     ;
043700161209       clear                   IMPdata    ;
043800161209       clear                   IMPfgs     ;
043900161209       clear                   IDnpg      ;
044000161209       clear                   IDdata     ;
044100161209       clear                   IDora      ;
044200161209       clear                   IDfgs      ;
044300161209       clear evedt    ;
044400161209       clear evedes   ;
044500161209       clear evedim   ;
044600161209       clear evehim   ;
044700161209
044800161207       clear wfkpi000  ;
044900170525
045000170525       // Lettura eventi
045100170525       exsr leggi_eve  ;
045200161207
045300161209       // calcolo ore reali di consegna tra data sped e data consegna
045400161207       exsr cal_gg    ;
045500161212
045600161216
045700161212       // Se le bolle OK non le voglio --> salto
0458001612120      if vidok <>'N' or  kpidly_c>0  ;
045900161207
046000161212       // se si vogliono gli eventi anche bolle ok elaboro
0461001612120a     if kpidly_c > 0  or vidokeve<>'N' ;
046200161102
046300161102       setll (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
046400161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0465001611021      dow  not %eof(fnblt01l)  ;
046600161214       clear memdse  ;
046700161102
046800161214       // verifico se presente una spunta 88/90
0469001612142      if bltdse>arbdbr or bltdse=0 or bltnpp=88 or bltnpp=90  ;
047000161102       setll  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
047100161102       reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
0472001611023      dow not %eof(fnbrv09l)  ;
047300161214
047400161214       // per testimoniare la presenza del collo metto qualsiasi data
047500161214       // entrata, disguido, arrivo. Quindi tratto la pistola 88 e 90
047600161214       //  allo stesso modo
047700161214
0478001612144       if brvnps=88 or brvnps=90 ;
047900161102         ex88='S'   ;
0480001612145        if  bltdse>0  ;
048100161214           memdse=bltdse;
048200161214x5       else  ;
048300161214          chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0484001612146         if %found(fnart01l) ;
0485001612147         if artdet>0 and artdet<artdam ;
048600161214           memdse=artdet;
048700161214x7        else   ;
0488001612148         if artdam>0  ;
048900161214           memdse=artdam;
0490001612148         endif  ;
0491001612147         endif  ;
0492001612146         endif  ;
0493001612145         endif  ;
049400161214
049500161214 4       endif  ;
049600161214
049700161214        if   memdse>0  ;
049800161214         leave ;
049900161214        endif  ;
050000161214
0501001612144       //if brvnps=90 ;
050200161214        //  chain (arbaas:arblnp:arbnrs:arbnsp:bltnsc) fnart01l  ;
0503001612145       // if artdam>bltdse  ;
050400161214        //  ex88='S'   ;
050500161214        //  wdse=bltdse;
050600161214        //  leave  ;
0507001612145       // endif  ;
0508001612144       //endif  ;
050900161103
051000161102
0511001611023      reade  (1 : bltfls:bltlna:bltnrs:bltnsc) fnbrv09l  ;
051200161102       enddo  ;
0513001611022      endif   ;
051400161209
051500161214       // se era spunta in 88 memorizzo la più recente come data ;
051600161214       if memdse>wdse  ;
051700161214         wdse=memdse  ;
051800161214       endif  ;
051900161214
052000161214       // Verifico se in IMP/Disguido ne basta uno per bolla
052100161214       if idnpg=' '  ;
052200161214         exsr IMPDISG  ;
052300161214       endif  ;
052400161102
052500161102
052600161102       reade (arbaas:arblnp:arbnrs:arbnsp) fnblt01l  ;
0527001611021      enddo  ;
052800161102
052900161209       // verifico se la trazione è ritardo , usando la data arrivo ult collo
053000161216       if wesARB='S'         ;
053100161103       endif  ;
053200161209
0533001612270a     endif  ;
053400161103
053500161102       exsr   Scrivifile  ;
0536001612120      endif  ;
053700161212
053800161102       ENDSR   ;
053900161209       //-------------------------------------------------------------------
054000161209       BEGSR  Leggi_eve              ;
054100161209
054200161209       // Lettura eventi che "servono"
054300161209       clear YY  ;
054400161227       clear indx;
054500161209
054600161209       setll  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
054700161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
054800161209  1    dow  not %eof(fnevb01l)   ;
054900161209
055000161209  2    if %lookup(evbcev:cev)>0   ;
055100161209
055200161227       // Se si tratta di MIC, memorizzo quello con data e ora più alta
055300161227  3    if evbcev='MIC'   ;
055400161227       indx=%lookup(evbcev:evedes)  ;
055500161227  4    if indx>0 ;
055600161227        clear evedt(indx)   ;
055700161227        clear evedes(indx)   ;
055800161227        clear evedim(indx)   ;
055900161227        clear evehim(indx)   ;
056000161227  4    endif  ;
056100161227  3    endif;
056200161227
056300161209       yy=yy+1  ;
056400161227
056500161227  3    if yy<32   ;
056600161209
056700161209        evedt(yy)=evbdev   ;
056800161209        evedes(yy)=evbcev   ;
056900161209        evedim(yy)=evbdtv   ;
057000161209        evehim(yy)=evborv   ;
057100161209
057200161209       // e si tratta di NIC memorizzo anche data e ora immissione
057300161209       // lo elimino se trovo un MIC successivo
057400161209  4     if evbcev='NIC' ;
057500161209         eval wnic='S'   ;
057600161209  4     endif  ;
057700161209  3     endif  ;
057800161209
057900161227  2     endif  ;
058000161209
058100161209        // solo se presente NIC verifico se da eliminare
058200161209   3      if wnic='S' and evbcev='MIC'    ;
058300161209
058400161227        ee=1   ;
058500161227   4    dow ee<=32  ;
058600161227   5    if evedes(ee)='NIC' ;
058700161209
058800161209   6    select  ;
058900161227        when evedt(ee)<>evbdev ;
059000161209
059100161227        when evedim(ee)<evbdtv ;
059200161227        clear evedt(ee)   ;
059300161227        clear evedes(ee)   ;
059400161227        clear evedim(ee)   ;
059500161227        clear evehim(ee)   ;
059600161227        wnic=' '   ;
059700161209
059800161227        when evedim(ee)=evbdtv and evehim(ee)<evborv ;
059900161227        clear evedt(ee)   ;
060000161227        clear evedes(ee)   ;
060100161227        clear evedim(ee)   ;
060200161227        clear evehim(ee)   ;
060300161227        wnic=' '   ;
060400161209        endsl  ;
060500161209
060600161209   5    endif   ;
060700161209
060800161227        ee=ee+1  ;
060900161209   4    enddo  ;
061000161209
061100161209   3      endif  ;
061200161209
061300161209
061400161209       reade  (arbaas:arblnp:arbnrs:arbnsp  ) fnevb01l ;
061500161209   1   enddo  ;
061600161209
061700161227       // il MIC lo tengo solo se è l'ultimo degli eventi altrimenti
061800161227       // lo emimino
061900161227       indx=%lookup('MIC':evedes)  ;
062000161227  1    if indx>0 ;
062100161227       yy  =indx+1  ;
062200161227  2    dow yy<=32   ;
062300161227  3     if  evedes(yy)<>*blanks   ;
062400161227         clear evedt(indx)   ;
062500161227         clear evedes(indx)   ;
062600161227         clear evedim(indx)   ;
062700161227         clear evehim(indx)   ;
062800161227         leave   ;
062900161227  3     endif  ;
063000161227
063100161227       yy=yy+1  ;
063200161227  2    enddo  ;
063300161227  1    endif  ;
063400161227
063500161227       // conto anche quanti eventi ci sono per memorizzare solo gli
063600161227       // ultimi 10
063700161227       yy=1  ;
063800161227       clear conta   ;
063900161227       dow yy<=32                ;
064000161227         if  evedes(yy)<>*blanks   ;
064100161227          conta=conta+1  ;
064200161227         endif  ;
064300161227       yy=yy+1  ;
064400161227       enddo  ;
064500161227
064600161227       clear   evesalta  ;
064700161227       if conta>10  ;
064800161227       evesalta=conta-10  ;
064900161227       endif   ;
065000161227
065100161209       ENDSR   ;
065200161102       //-------------------------------------------------------------------
065300161102       BEGSR  Scrivifile             ;
065400111116
065500110913
065600161102       kpiidute=knmeb   ;
065700161102       kpiepo=dutpou  ;
065800110913
065900161102       kpiccm=wccm    ;
066000161102       kpirsm=arbrsm  ;
066100161102
066200161102       kpiaas=arbaas  ;
066300161102       kpilnp=arblnp  ;
066400161102       kpimes=%int(%subst(%editc(arbmgs:'X'):1:2))  ;
066500161102       kpirmn=arbrmn  ;
066600161102       kpirma=arbrma  ;
066700161102       kpinrs=arbnrs  ;
066800161102       kpinsp=arbnsp  ;
066900161102       kpimgs=arbmgs  ;
067000161102       kpilna=arblna  ;
067100161102       kpirsd=arbrsd  ;
067200161102       kpilod=arblod  ;
067300161102       kpiprd=arbprd  ;
067400161102       kpicad=arbcad  ;
067500161215       kpifin=arbfin  ;
067600161102       kpinzd=arbnzd  ;
067700161102       kpiffd=arbffd   ;
067800161102       kpitc1=arbtc1   ;
067900161102       kpitc2=arbtc2   ;
068000161102       kpigc1=arbgc1   ;
068100161102       kpigc2=arbgc2   ;
068200161102       kpincl=arbncl   ;
068300161102       kpipkb=arbpkb   ;
068400161102       kpivlb=arbvlb   ;
068500161102       kpitcr=arbtcr   ;
068600161102       kpidcr=arbdcr   ;
068700161102       kpidcp=arbdcp   ;
068800161102       kpidcm=arbdcm   ;
068900161102       kpihmc=arbhmc   ;
069000161209       kpicca=arbcca   ;
069100161209
069200161216       if arbdcm=0 and wesarb='S' and a_arbdcm>0 and arbcca<>'1';
069300161209       kpidcm=a_arbdcm   ;
069400161209       kpihmc=a_arbhmc   ;
069500161209       kpicca=a_arbcca   ;
069600161209       endif  ;
069700161209
069800161209       if kpicca<>' '  ;
069900161209       tblkey=kpicca   ;
070000161209       chain (1:'7O':tblkey) tabel  ;
070100161209         if %found(tabel00f )  ;
070200161209          kpicca_des=tbluni   ;
070300161209         endif  ;
070400161209       endif  ;
070500161103
070600161103        // pistola ex 88
070700161102        if ex88='S'     ;
070800161102        kpip88='S'      ;
070900161103        kpidse=wdse     ;
071000161102        endif           ;
071100161103
071200161103        //IMP disguido
071300161103        if IDnpg<>' '   ;
071400161103           if IDnpg='P'   ;
071500161103           kpiimpds='P'   ;
071600161103           else  ;
071700161103           kpiimpds='D'   ;
071800161103           endif ;
071900161103        endif ;
072000161209
072100161209       yy=1  ;
072200161227       clear evemem   ;
072300161209
072400170525 1     dow yy<=32               ;
072500161227 2     if  evedes(yy)<>*blanks   ;
072600170524
072700170524       // Verifico se data evento uguale alla data prevista consegna
072800170524 2a    if kpidce>0 ;
072900170524
073000170525 3     if evedt(yy)<=kpidce and kpidcesn<>'S'  ;
073100170524          indx= %lookup(evedes(yy):cev) ;
073200170524 4     if cevt(indx)='E';
073300170524        kpidcesn='S'  ;
073400170524 4     ENDIF;
073500170524       // memorizzo diversamente se c'è un evento particolare
073600170525 4     if cevt(indx)='P' and  evedt(yy) =kpidce ;
073700170524        kpidcesn='P'  ;
073800170524 4     ENDIF;
073900170524
074000170524 3     ENDIF;
074100170524 2a    ENDIF;
074200161227
074300161227       evemem=evemem+1  ;
074400161227
074500161227       // se conta < 10 memorizzo tutti altrimenti salto i primi
074600161227
074700170525 2a    if kpidly_c > 0  or vidokeve<>'N' ;
074800161227 3     if conta <=10 or evemem>evesalta  ;
074900161227 4      select   ;
075000161227         when KPICEV1=*blanks and evedes(yy)<>'MIC';
075100161209         KPICEV1=evedes(yy)  ;
075200161209         KPIDEV1=evedt(yy)  ;
075300161227         when KPICEV2=*blanks and evedes(yy)<>'MIC';
075400161209         KPICEV2=evedes(yy)  ;
075500161209         KPIDEV2=evedt(yy)  ;
075600161227         when KPICEV3=*blanks and evedes(yy)<>'MIC';
075700161209         KPICEV3=evedes(yy)  ;
075800161209         KPIDEV3=evedt(yy)  ;
075900161227         when KPICEV4=*blanks and evedes(yy)<>'MIC';
076000161209         KPICEV4=evedes(yy)  ;
076100161209         KPIDEV4=evedt(yy)  ;
076200161227         when KPICEV5=*blanks  and evedes(yy)<>'MIC';
076300161209         KPICEV5=evedes(yy)  ;
076400161209         KPIDEV5=evedt(yy)  ;
076500161227         when KPICEV6=*blanks  and evedes(yy)<>'MIC';
076600161209         KPICEV6=evedes(yy)  ;
076700161209         KPIDEV6=evedt(yy)  ;
076800161227         when KPICEV7=*blanks  and evedes(yy)<>'MIC';
076900161227         KPICEV7=evedes(yy)  ;
077000161209         KPIDEV7=evedt(yy)  ;
077100161227         when KPICEV8=*blanks  and evedes(yy)<>'MIC';
077200161209         KPICEV8=evedes(yy)  ;
077300161209         KPIDEV8=evedt(yy)  ;
077400161227         when KPICEV9=*blanks  and evedes(yy)<>'MIC';
077500161209         KPICEV9=evedes(yy)  ;
077600161209         KPIDEV9=evedt(yy)  ;
077700161209         when KPICEV0=*blanks  ;
077800161209         KPICEV0=evedes(yy)  ;
077900161209         KPIDEV0=evedt(yy)  ;
078000161227 4       endsl  ;
078100161227 3       endif  ;
078200161227 2       endif  ;
078300170525 2a      endif  ;
078400161209
078500161209         yy=yy+ 1;
078600161209
078700161227 1     enddo  ;
078800170523
078900170523        // Se la data consegna prevista corrisponde alla data cons merce--> ok
079000170526 1      if kpidcm>0 and kpidce>0 and kpidcesn<>'S' ;
079100170526        // utilizzo la data  di consegna merce di arb se esiste per le bolle
079200170526        //  legate
079300170526
079400170526 2        if wesarb='S' and a_arbdcm<=kpidce ;
079500170526            kpidcesn='S' ;
079600170526 x2       else  ;
079700170526 3        if kpidcm<=kpidce ;
079800170523            kpidcesn='S' ;
079900170526 3        ENDIF;
080000170526 2      ENDIF;
080100170526 1      ENDIF;
080200170526 1          if kpidcesn=' '  and kpidce>0 ;
080300170525            kpidcesn='N' ;
080400170526 1          ENDIF;
080500110916
080600161102       write  wfkpi000 ;
080700171219       //exsr   WRITIVGD ;
080800110913
080900161214       CONTAB=CONTAB+1   ;
081000110926
081100110913       ENDSR                  ;
081200170609
081300170609     C*------------------------------------------------------------------------*
081400170609     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
081500170609     C*------------------------------------------------------------------------*
081600171219     C***  WRITIVGD      BEGSR
081700171219     C***
081800171219     C***                clear                   tivgd000
081900171219     C***                eval      vgdDTA = %subst(DS_KPI:1:%size(DS_KPI))
082000171219     C***                eval      vgdTIP = '&1'
082100171219     C***                eval      vgdKSU = '0BRT0'+%editc(dutpou:'X')
082200171219     C***                eval      vgdTSC = 'WW'
082300171219     C***                eval      vgdDAT = dateu
082400171219     C***                eval      vgdPRG = o§7VASPRG
082500171219     C***                eval      vgdPGM = procname
082600171219     C***                eval      vgdSTO = '?'
082700171219     C***                write     tivgd000
082800170609     C*
082900171219     C***                ENDSR
083000170609     C*------------------------------------------------------------------------*
083100170609
083200170609
083300170609
083400080327       //-------------------------------------------------------------------
083500080327       BEGSR  *INZSR                 ;
083600080328
083700080328       in(E) *dtaara   ;
083800080328       if    %error or rsut  =*blanks  ;
083900080328       callp TIBS34R (TIBS34DS)        ;
084000080328       in    *dtaara   ;
084100080328       endif                           ;
084200080829
084300110613       // imposto data e ora del giorno
084400110613       dataiso=datasys  ;
084500161214       dataeur=datasys  ;
084600110613       dateu=%dec(dataiso)   ;
084700161214       dataogg=%dec(dataeur)   ;
084800110616
084900110613       ora_iso=ora_sys  ;
085000110613       ora_u=%dec(ora_iso)   ;
085100080829
085200110913       // Se richiesto file pulisco il lancio precedente per filiale
085300161102       setll (dutpou)   wfKPI01l   ;
085400161102       reade (dutpou)   wfKPI01l   ;
085500110616
085600161102       dow  not %eof(wfKPI01l)   ;
085700161102       delete wfKPI000  ;
085800110616
085900161102       reade (dutpou)   wfKPI01l   ;
086000110616       enddo  ;
086100110616
086200161209       clear xx ;
086300161209       setll  (1:'2A') tabel00f ;
086400161209       reade  (1:'2A') tabel00f ;
086500161209
086600161209       dow not %eof(tabel00f ) ;
086700161209       if tblflg=' '  ;
086800161209        ds2a = tbluni   ;
086900161209
087000161209       if   §2aftg='S' or §2atip='N' or §2aftc='A'  ;
087100161209       xx=xx+1  ;
087200161209        cev(xx)=%Subst(tblkey:1:3)  ;
087300170524        if §2antc='S' ;
087400170524          cevt(xx)='E' ;
087500170524        ENDIF;
087600161209       endif  ;
087700161209       endif  ;
087800161209
087900161209       reade  (1:'2A') tabel00f ;
088000161209       enddo  ;
088100161209
088200161209       setll  (1:'2Z') tabel00f ;
088300161209       reade  (1:'2Z') tabel00f ;
088400161209
088500161209       dow not %eof(tabel00f ) ;
088600161209       if tblflg=' '  ;
088700161209        ds2z = tbluni   ;
088800161209
088900161209       if   §2zcep<>' '  ;
089000161209       xx=xx+1  ;
089100161209        cev(xx)=%subst(tblkey:1:3)  ;
089200170523        cevt(xx)='P' ;
089300161209       endif  ;
089400161209       endif  ;
089500161209
089600161209       reade  (1:'2Z') tabel00f ;
089700161209       enddo  ;
089800161209
089900161209       // infine aggiungo fisso il NIC
090000161209       xx=xx+1  ;
090100161209       cev(xx)='NIC' ;
090200161227       xx=xx+1  ;
090300161227       cev(xx)='MIC' ;
090400161209
090500161209       ENDSR     ;
090600161102       //-------------------------------------------------------------------
090700161209       BEGSR  PrepaLNP               ;
090800161102
090900161102       clear  LnpSel  ;
091000161102       xx= 1 ;
091100161102       yy= 1 ;
091200161102
091300161102       dow xx <=20  ;
091400161102        if lnp(xx)>0  ;
091500161102         Indx=%lookup(lnp(xx):LnpSel)  ;
091600161102         if Indx=0  ;
091700161102           eval lnpSel(yy)=lnp(xx)  ;
091800161102           yy=yy+1   ;
091900161102         endif  ;
092000161102
092100161102        endif  ;
092200161102
092300161102        xx=xx+1   ;
092400161102        enddo     ;
092500161102
092600161102        // Memorizzo nel campo oggcli il codice cliente che devo
092700161102        //  mettere nell'ogggetto della mail
092800171219           clear oggcli  ;
092900171219           if cli(1)>0   ;
093000171219             oggcli=cli(1)  ;
093100171219           endif  ;
093200161102
093300161102       ENDSR  ;
093400161207
093500161103
093600161103      /end-free
093700161103
093800161207     c*-------------------------------------------------------------------
093900161207     c     Cal_gg        BEGSR
094000161207     c
094100161209     c     karb          klist
094200161209     c                   kfld                    arbaas
094300161209     c                   kfld                    arblnp
094400161209     c                   kfld                    arbnrs
094500161209     c                   kfld                    arbnsp
094600170524     c     kar5          klist
094700170524     c                   kfld                    arbaas
094800170524     c                   kfld                    arblnp
094900170524     c                   kfld                    arbnrs
095000170524     c                   kfld                    arbnsp
095100170524     c                   kfld                    ktrd
095200161209
095300161216     c* chain solo se non l'ho già fatto prima
095400161216     c                   if        wesarb=' '
095500161209     c     karb          chain     fnarb01l
095600170105     c                   if        %found(fnarb01l)
095700170105     c                   eval      wesARB='S'
095800170105     c                   else
095900170105     c                   eval      wesARB='N'
096000170105     c                   endif
096100161216     c                   endif
096200170524
096300170524     c     kar5          chain     fiar501l
096400170524     c                   if        %found(fiar501l)
096500170524     c                   eval      dar5gen=ar5uni
096600170524     c                   else
096700170524     c
096800170524     c                   clear                   dar5gen
096900170524     c                   endif
097000170525
097100170525     c* prendo il primo evento pieno "NO MIC/NIC"
097200170525     c                   clear                   dtprimoeve
097300170525     c                   eval      yy=1
097400170525     c                   dow       yy<=10 and dtprimoeve=0
097500170525     c                   if        evedes(yy)<>*blanks and
097600170525     c                             evedes(yy)<>'MIC' and
097700170525     c                             evedes(yy)<>'NIC'
097800170525     c                   EVAL      DTPRIMOEVE=EVEDT(YY)
097900170525     c                   ENDIF
098000170525     c                   ADD       1             YY
098100170525     c                   ENDDO
098200170525
098300161216
098400161207     c                   clear                   tnsd99ds
098500161207     C                   MOVEL     ' '           D98TLA
098600161216     c                   if        wesARB='S'
098700161207     C                   MOVEL     'A'           D98TBO
098800161209     c                   else
098900161209     C                   MOVEL     'P'           D98TBO
099000161209     c                   endif
099100161207     C                   MOVEL     ARBAAS        D98AAS
099200161207     C                   MOVEL     ARBLNP        D98LNP
099300161207     C                   MOVEL     ARBNRS        D98NRS
099400161207     C                   MOVEL     ARBNSP        D98NSP
099500161207     c
099600161207     c                   if        arbtsp='D'
099700161207     c                   eval      I98TSP_FOR='C'
099800161207     c                   endif
099900170524     c
100000161207     C*
100100161207     C                   CALL      'TNSD99R'
100200161207     C                   PARM                    TNSD99DS
100300161207     c
100400161207     c* ore teoriche
100500161209     c                   eval      KPIdly_T=d98trc
100600161209     c                   eval      KPIdag  =d98dag
100700161209     c                   eval      KPIdcg  =d98dcg
100800161215     c                   eval      KPIdac  =d98dac
100900161215     c                   eval      KPIdcc  =d98dcc
101000161207
101100161207     c* Richiamo il pgm utilizzando la DS eliminando il num sped
101200161207     c*  e la giacenza
101300161207     c                   clear                   d98nsp
101400161207     c                   clear                   d98dti
101500161207     c                   clear                   d98hti
101600161207     c                   clear                   d98cmc
101700161207     c                   clear                   d98dag
101800161207     c                   clear                   d98dcg
101900161207     c                   eval      d98duc=d98dsp
102000161207     c                   eval      d98dbr=d98dsp
102100170105     c                   eval      D98FDRT='S'
102200161207     c                   clear                   d98dam
102300170524     c* considero i giorni di chiusura se inseriti in partenza
102400170524     c                   if        §ar5mdp='S'
102500170524     c                   eval      d98gc1=§AR5GC1
102600170524     c                   eval      d98gc2=§AR5GC2
102700170524     c                   else
102800170524     c                   clear                   d98gc1
102900170524     c                   clear                   d98gc2
103000170524     c                   endif
103100170524
103200170524     c* la consegna richiesta la considero se inserita in partenza
103300170524     c*  o in arrivo prima del primo tentativo
103400170524     c                   if        d98dcr>0 and (§ar5mdp='S' and
103500170524     c                             §ar5dcr<>%editc(d98dcr:'X'))
103600170525     c**                   clear                   datadcr
103700170525     c**     karb          setll     fiarbf2c
103800170525     c**     karb          reade     fiarbf2c
103900170525     c**                   dow       not %eof(fiarbf2c)
104000170525     c**                   if        %subst(v_arbcvb:1:1)='C'
104100170525     c**                             and v_arbcvb<>'CP'
104200170525     c**                   eval      datadcr=v_arbdtv
104300170525     c**                   leave
104400170525     c**                   endif
104500170524     c
104600170525     c**     karb          reade     fiarbf2c
104700170525     c**                   enddo
104800170525     c*
104900170525     c                   if        d98dcr>dtprimoeve
105000170524     c                   clear                   d98dcr
105100170524     c                   clear                   d98hcr
105200170524     c                   clear                   d98tcr
105300170524     c                   endif
105400170524     c                   endif
105500170525     c* Se il fermo deposito è stato inserito dopo il primo tentativo
105600170525     c*  NON lo considero
105700170525     c                   if        d98ffd='S'   and
105800170525     c                             dtprimoeve>0  and
105900170525     c                             §ar5dfd>*zeros and
106000170525     c                             %int(§ar5dfd)>=dtprimoeve
106100170525     c                   clear                   d98ffd
106200170525     c                   endif
106300170525     c
106400170524     c
106500161207     C                   CALL      'TNSD99R'
106600161207     C                   PARM                    TNSD99DS
106700161207     c
106800161207     c* controllo le ore reali  con le ore teoriche
106900161209     c                   eval      KPIdly_R=d98rrc
107000170523     c* Memorizzo la data prevista consegna
107100170523     c                   eval      KPIdce= d98dce
107200170524     c* Se il fermo deposito è stato inserito dopo il primo tentativo
107300170524     c*  NON lo considero
107400170524     c                   if         d98ffd='S'
107500170525     c                   if        dtprimoeve=0  or
107600170524     c                             §ar5dfd<=*zeros or
107700170525     c                             %int(§ar5dfd)< dtprimoeve
107800170524     c                   clear                   kpidce
107900170524     c                   endif
108000170524     c                   endif
108100161215     c
108200170523     c* se la spedizione è borderizzata in data festiva --> aggiungo 24 ore
108300161215     c*  alle ore reali
108400170105     c* ES 05/1/17 non lo faccio più in quanto  ho modificato allo stesso modo
108500170105     c*  il calcolo della affidabilità tnsd99r
108600170105     c**                 if        arbdbr>0
108700170105     c**                 clear                   wdat8
108800170105     c**                 clear                   tfptfa
108900170105     c**                 z-add     arbdbr        datada
109000170105     c*+                 z-add     arbdbr        dataa
109100170105     c**                 z-add     arbtfp        p_tfp
109200170105     c*+                 z-add     arbtfa        p_tfa
109300170105     c**                 call      'XSRLAV8'
109400170105     c**                 parm                    wdat8
109500170105     c**                 parm                    tfptfa
109600161215     c* se giorno FESTIVO aggiungo 24 ore
109700170105     c**                 if        ggl=0
109800170105     c**                 eval      KPIdly_R=KPIdly_R+24
109900170105     c**                 endif
110000161215
110100170105     c**                 endif
110200161215     c
110300161207     c                   if        kpidly_r<=kpidly_t
110400161207     c                   clear                   kpidly_c
110500161207     c                   else
110600161207     c
110700161207     c                   eval      kpidly_c =kpidly_r-kpidly_t
110800161207     c                   endif
110900161207     c*
111000161213     c* CONSEGNA RICHIESTA IL
111100161212     c* Se data richiesta = data consegna merce
111200161209     c*  sempre OK
111300161212     c**                 if        arbtc1='A' or arbtc2='A'  or
111400161212     c**                           arbtc1='S' or arbtc2='S'
111500161213     c                   if        arbdcr>0 and arbtcr=' '
111600161213     c                   if        arbdcr=arbdcm
111700161209     c                   clear                   kpidly_c
111800161209     c                   endif
111900161213     c* se bolla NON consegnata e data cons richiesta > di oggi
112000161213     c* per ora ON TIME
112100161213     c                   if        arbdcm=0 and arbdcr>=dateu
112200161213     c                   clear                   kpidly_c
112300161213     c                   endif
112400161213     c                   endif
112500161213     c
112600161212     c**                 endif
112700161213
112800161213     c* CONSEGNA RICHIESTA PRIMA DEL
112900161213     c                   if        arbdcr>0 and arbtcr='P'
113000161213     C                   IF        ARBDCM>0 AND ARBDCM<ARBDCR
113100161213     c                   clear                   kpidly_c
113200161213     c                   endif
113300161213     C                   IF        ARBDCM=0
113400161213     c                   clear                   kpidly_c
113500161213     c                   endif
113600161213     c                   endif
113700161213     c* CONSEGNA RICHIESTA DOPO IL
113800161213     c                   if        arbdcr>0 and arbtcr='D'
113900161213     C                   IF        ARBDCM>0 AND ARBDCM>ARBDCR
114000161213     c                   clear                   kpidly_c
114100161213     c                   endif
114200161213     C                   IF        ARBDCM=0
114300161213     c                   clear                   kpidly_c
114400161213     c                   endif
114500161213     c                   endif
114600161207
114700161212     c* se cons anomala RESO o DIROTTAMENTO --> sempre ok
114800161212     c                   if        arbcca='1' or arbcca='2' or
114900161212     c                             arbcca='6'
115000161212     c                   clear                   kpidly_c
115100161212     c                   endif
115200161207     c*
115300161207     c                   ENDSR
115400161103     c*-------------------------------------------------------------------
115500161103     c     IMPdisg       BEGSR
115600161209     c
115700161209     c     kbrv          Klist
115800161209     c                   Kfld                    bltfls
115900161209     c                   Kfld                    bltlna
116000161209     c                   Kfld                    bltnrs
116100161209     c                   Kfld                    bltnsc
116200161103     c
116300161103     c* per ogni collo verifico se in IMP o disguidato
116400161103     c* prendo la data più alta
116500161103     c     kbrv          setll     fnbrv07l
116600161103     c     kbrv          reade     fnbrv07l
116700161103    2c                   dow       not %eof(fnbrv07l)
116800171018
116900171018     c*se si tratta di disguido ignoro se cat 5 e spuntato dal 2 livello
117000171018     c                   if        brvatr='D' and brvnpg=5
117100171018     c                             and brvfle=arbtfp
117200171018     c                   clear                   brvatr
117300171018     c                   endif
117400171018     c
117500161103    3c                   if        brvatr='D'  or brvnpg=3
117600161103     c* prendo la data della spunta
117700161103     c                   clear                   fnlv53ds
117800161103     C                   MOVEL     BRvNPG        D53NPG
117900161103     C                   Z-ADD     BRvNFV        D53NFV
118000161103     C                   MOVEL     BRvFGS        D53FGS
118100161103     C                   CALL      'FNLV53R'
118200161103     C                   PARM                    fnlv53ds
118300161103    4c                   if        d53err<>*blanks
118400161103    5c                   if        brvdcs>0
118500161103     c                   eval      d53dfv=brvdcs
118600161103     c                   else
118700161103    6c                   if        brvdfs>0
118800161103     c                   eval      d53dfv=brvdfs
118900161103    6c                   endif
119000161103    5c                   endif
119100161103    4c                   endif
119200161103     c
119300161103    4c                   select
119400161103     c* se si tratta di DISGUIDO
119500161103     c                   when      brvatr='D'
119600161103     c
119700161103    5c                   if        d53dfv=DISdata
119800161103     c* Verifico l'ora
119900161103    6c                   if        brvhcs>0 and brvhcs>DISora
120000161103     c                   eval      DISdata=d53dfv
120100161103     c                   eval      DISfgs =brvfgs
120200161103     c                   eval      DISnpg =brvnpg
120300161103     c                   eval      DISora =brvhcs
120400161103   x6c                   else
120500161103    7c                   if        brvhms>0 and brvhms>DISora
120600161103     c                   eval      DISdata=d53dfv
120700161103     c                   eval      DISfgs =brvfgs
120800161103     c                   eval      DISnpg =brvnpg
120900161103     c                   eval      DISora =brvhms
121000161103    7c                   endif
121100161103    6c                   endif
121200161103    5c                   endif
121300161103     c
121400161103    5c                   if        d53dfv>DISdata
121500161103     c                   eval      DISdata=d53dfv
121600161103     c                   eval      DISfgs =brvfgs
121700161103     c                   eval      DISnpg =brvnpg
121800161103     c                   eval      DISora =brvhcs
121900161103     c                   if        disora=0
122000161103     c                   eval      DISora =brvhms
122100161103     c                   endif
122200161103    5c                   endif
122300161103     c
122400161103     c* se si tratta di IMP
122500161103    4c                   when      d53spg='P' and d53dfv>impdata
122600161103     c                   eval      IMPdata=d53dfv
122700161103     c                   eval      IMPfgs =brvfgs
122800161103    4c                   endsl
122900161103    3c                   endif
123000161103
123100161103     c     kbrv          reade     fnbrv07l
123200161103    2c                   enddo
123300161103
123400161103     c* memorizzo la data più alta
123500161103     c* IMP vince sul disguido per stessa data, senza guardare l'ora
123600161103    2c                   if        IMPdata>0 and IMPdata>=IDdata
123700161103     c                   eval      IDdata=IMPdata
123800161103     c                   eval      IDfgs=IMPfgs
123900161103     c                   eval      IDora=0
124000161103     c                   eval      IDnpg='P'
124100161103    2c                   endif
124200161103    2c                   if        (DISdata>0 and DISdata=IDdata
124300161103     c                             and IDnpg<>'P' and DISora>IDora) or
124400161103     c                             (DISdata>0 and DISdata>IDdata)
124500161103     c                   eval      IDdata=DISdata
124600161103     c                   eval      IDfgs=DISfgs
124700161103     c                   eval      IDora=DISora
124800161103     c                   eval      IDnpg=%char(DISnpg)
124900161103    2c                   endif
125000161103     c
125100161103     C                   ENDSR
125200161214     OQSYSPRT   E            fine             02
125300161214     O                                              'Elaborazione esiti consegn'
125400161214     o                                              'a il'
125500161214     o                       dataogg           +  2 '  /  /    '
125600161214     o                       ora_u             +  2 '  :  :  '
125700161214     o                                         +  2 'Bolle scritte :'
125800161214     o                       contab        2   +  2
125900171218     o                       stainvio          +  2
126000161214**
126100161214OVRPRTF FILE(QSYSPRT) USRDTA('EsitiCons') HOLD(*YES)
