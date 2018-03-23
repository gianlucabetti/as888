000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200160919     H* FNLRU39R *---------------------------------------------------*
000300160919     H*     elaborazione dati per file  FIART00F
000400000000     H*--------------------------------------------------------------*
000500120618     fFNart01L  if   e           k Disk
000600090225     f                                     prefix(old:3)
000700090225     fFNBLt01L  if   e           k Disk
000800120618     fFnBLp01L  if   e           k Disk    prefix(arb:3)
000900161104     fFnarb01L  if   e           k Disk
001000090226     fazorg01l  if   e           k Disk
001100120618     ffiar501l  if   e           k Disk
001200120618     ffnbrv07l  if   e           k Disk
001300160920     ffnbrv05l  if   e           k Disk    rename(fnbrv000:fnbrv005)
001400120618     ffnevb04l  if   e           k Disk
001500051012     F
001600060215     d
001700051012     D
001800120618     d ktrd            s                   like(ar5trd) inz('GEN')
001900120618     d kcev            s                   like(evbcev) inz('P  ')
002000150618     d w_fnart         s              1
002100150618     d w_fnblt         s              1
002200160920     d knpg            s              1  0 inz(1)
002300150618     d keybolla        s             16
002400150618     d savbolla        s                   like(keybolla)
002500120618     d dar5gen       e ds
002600120618     d og143         e ds
002700120618     d fnlv53ds      e ds
002800120618     d fnlv55ds      e ds
002900170126     d FiartDs       e ds                  extname(fiart00f)
003000160919     d fnlru39ds     e ds
003100161116     d dataiso         s               d   datfmt(*ISO)  inz(*sys)
003200051012     C
003300150618     c     *entry        plist
003400160919     c                   parm                    fnlru39ds
003500150618     c                   parm                    FiartDs
003600161116
003700060217      *
003800051019     c
003900150618    1C     i38tla        IFNE      'C'
004000150618
004100150618     c                   clear                   fiartds
004200150618
004300150618     c* chain la testata bolla solo se diversa da quella elaborata precedentemente
004400150618     c                   eval      keybolla=%editc(i38aas:'X')
004500150618     c                             +%editc(i38lnp:'X')
004600150618     c                             +%editc(i38nrs:'X') + %editc(i38nsp:'X')
004700150618    2c                   if        keybolla<>savbolla
004800161104     c* preferisco chainare FNARB perchè i dati di consegna sono prima su ARB e poi su BLP
004900161104     c     kbolla        chain     fnarb01l
005000161104    3c                   if        not %found(fnarb01l)
005100161104     c     kbolla        chain     fnblp01l
005200161104    3c                   endif
005300150618     c                   eval      savbolla=keybolla
005400150618    2c                   endif
005500160914
005600150618     c* Dati da dettaglio colli in partenza
005700150618     c                   clear                   w_fnblt
005800150618     c                   clear                   w_fnart
005900150618     c     kdett         chain     fnblt01l
006000150618     c                   if        not %found(fnblt01l)
006100150618     c                   eval      w_fnblt='N'
006200150618     c                   endif
006300150618     c     kdett         chain     fnart01l
006400150618     c                   if        not %found(fnart01l)
006500150618     c                   eval      w_fnart='N'
006600150618     c                   endif
006700150618     c                   if        w_fnblt=*blanks
006800150618     c                   exsr      sr_dablt
006900150618     c                   endif
007000150618     c* Dati da dettaglio colli in arrivo
007100150618     c                   if        w_fnart=*blanks
007200150618     c                   exsr      sr_daart
007300150618     c                   endif
007400150618     c* chain su spunte e fiar5 per le data e ora mancanti
007500150618     c                   exsr      altredate
007600150618
007700150618    1c                   endif
007800150618
007900150618     C     I38tla        IFNE      ' '
008000150618     c                   eval      d53tla='C'
008100150618     c                   call      'FNLV53R'
008200150618     c                   parm                    fnlv53ds
008300150618     c
008400150618     c                   eval      d55tla='C'
008500150618     c                   call      'FNLV55R'
008600150618     c                   parm                    fnlv55ds
008700150618
008800150618
008900150618     C                   SETON                                        LR
009000150618     C                   ELSE
009100150618     C                   SETON                                        RT
009200150618     C                   ENDIF
009300150618     c
009400051012     c***
009500150618     c*------------------------------------------------------------------
009600150618     c     sr_DaBlt      BEGSR
009700150618     c                   eval      artaas=bltaas
009800150618     c                   eval      artlnp=bltlnp
009900150618     c                   eval      artnrs=bltnrs
010000150618     c                   eval      artnsp=bltnsp
010100150618     c                   eval      artlna=bltlna
010200150618     c                   eval      artfls=bltfls
010300150618     c                   eval      artnsc=bltnsc
010400150618     c                   eval      artvuc=bltvuc
010500150618     c                   eval      artfvc=bltfvc
010600150618     c                   eval      artpuc=bltpuc
010700150618     c                   eval      artfpc=bltfpc
010800150618     c                   eval      artdfv=bltdfv
010900150618     c                   eval      artnpp=bltnpp
011000161104     c                   if        bltnpp=87
011100161104     c                   eval      artx88='S'
011200161104     c                   endif
011300150618     c                   eval      artnfv=bltnfv
011400150618     c                   eval      artdse=bltdse
011500150618     c                   eval      artnpe=bltnpe
011600150618     c                   eval      artdet=bltdet
011700150618     c                   eval      artdam=bltdam
011800150618    3c                   if        artlna=arbtfa  and artdam>0
011900150618     c                   eval      artdamf=bltdam
012000150618     c                   endif
012100161116     c* se il terminal è diverso MA si tratta di bolla dirottata o resa
012200161116     c*  è meglio aggiornare anche DAMF
012300161116     c                   if        artlna<>arbtfa and arbcca='1' and artdcm>0
012400161116     c                   eval      artdamf=bltdam
012500161116     c                   endif
012600170322     c* Per i RESI ogni bolla visualizza il suo dettaglio colli
012700170322     c***                if        artlna<>arbtfa and (arbcca='2' or arbcca='6')
012800170322     c***                eval      artdamf=bltdam
012900170322     c***                endif
013000150618     c
013100150618     c                   eval      artdut=bltdut
013200150618     c                   eval      artdcm=bltdcm
013300150618     c                   eval      artcan=bltcan
013400150618    4c                   if        artcan<>' '
013500150618     c                   eval      artfta='P'
013600150618    4c                   endif
013700150618     c                   eval      artcca=bltfl1
013800150618     c                   eval      arttda=bltfl2
013900150618     c                   eval      artftr=bltftr
014000150618     c* Se bltftr=C significa trasmesso al cliente e quindi imposto
014100150618     c*  la data trasmissione al cliente
014200150618    4c                   if        bltftr='C'
014300150618     c                   eval      artdtc=artdcm
014400150618    4c                   endif
014500150618     c* Verifico se si tratta di bolla export
014600150618     c     bltlna        chain     azorg01l
014700150618    4c                   if        %found(azorg01l)
014800150618     c                   movel     orgde3        og143
014900150618     c                   else
015000150618     c                   clear                   og143
015100150618    4c                   endif
015200150618     c
015300150618     c* Se non c'e' il record in arrivo
015400150618    4C                   IF        w_fnart='N'
015500150618     c* Se non trovo bolla in arb e la bolla è già stata trasmessa e
015600150618     c*  consegnata, significa che è stata pulita da fnarb
015700161031    5c                   if        arbdcm>0 and arbft1<>' '
015800150618     c                   eval      artbga='C'
015900150618     c                   eval      artabn='S'
016000150618    6c                   if        §ogntw='EEX' or §ogntw='FED' or
016100150618     c                             §ogntw='DPD'
016200150618     c                   eval      artdmf=artdam
016300150618    6c                   endif
016400150618    5c                   endif
016500161031
016600161031    5c                   if        arbdcm=0 and arbft1<>' '
016700161031     c                   eval      artbga='D'
016800161031     c                   eval      artabn='S'
016900161031     c                   endif
017000150618     c
017100150618   x4c                   else
017200150618     c                   eval      artbga='S'
017300150618     c
017400150618    5c                   if        artvuc=0 and oldvuc>0
017500150618     c                   eval      artvuc=oldvuc
017600150618     c                   eval      artfvc=oldfvc
017700150618    5c                   endif
017800150618    5c                   if        artpuc=0 and oldpuc>0
017900150618     c                   eval      artpuc=oldpuc
018000150618     c                   eval      artfpc=oldfpc
018100150618    5c                   endif
018200150618    c
018300150618    4c                   endif
018400150618     c                   endsr
018500150618     c*------------------------------------------------------------------
018600150618     c     sr_DaArt      BEGSR
018700150618     c* i campi seguenti li imposto solo se non ho trovato dettaglio colli
018800150618     c* bolla part.
018900150618     c                   if        w_fnblt='N'
019000150618     c                   eval      artbga='A'
019100150618     c                   eval      artaas=oldaas
019200150618     c                   eval      artlnp=oldlnp
019300150618     c                   eval      artnrs=oldnrs
019400150618     c                   eval      artnsp=oldnsp
019500150618     c                   eval      artlna=oldlna
019600150618     c                   eval      artfls=oldfls
019700150618     c                   eval      artnsc=oldnsc
019800150618     c                   eval      artvuc=oldvuc
019900150618     c                   eval      artfvc=oldfvc
020000150618     c                   eval      artpuc=oldpuc
020100150618     c                   eval      artfpc=oldfpc
020200150618     c                   endif
020300150618     c                   eval      artdfv=olddfv
020400150618     c                   eval      artnpp=oldnpp
020500150618     c                   eval      artnfv=oldnfv
020600150618     c                   eval      artflp=oldflp
020700150618     c                   eval      artabn=oldabn
020800161116     c                   if        oldcan<>' '  and artfta=' '
020900160922     c                   eval      artcan=oldcan
021000150618     c                   eval      artfta='A'
021100150618     c                   endif
021200150618     c                   eval      artdet=olddet
021300150618     c                   eval      artpet=oldpet
021400150618     c                   eval      artdut=olddut
021500150618     c                   eval      artput=oldput
021600161103     c                   eval      artdam=olddam
021700161122     c* se pistola 85 imposto anche data arrivo finale
021800161122    3c                   if        (artlna=arbtfa  and artdam>0 )
021900161122     c                             or (olddam>0 and oldnap=85)
022000150618     c                   eval      artdamf=olddam
022100160914     c                   eval      artnapf=oldnap
022200161122     c* se pistola 85 imposto anche dia e diaf
022300161122     c                   if        oldnap=85
022400161122     c                   eval      artdia=olddam
022500161122     c                   eval      artdiaf=olddam
022600161122     c                   eval      artflnap=artlna
022700161122     c                   endif
022800161122     c
022900161116     c                   else
023000161116     c                   clear                   artdamf
023100161116     c                   clear                   artnapf
023200150618     c                   endif
023300150618     c
023400150618     c                   eval      artnap=oldnap
023500150618     c                   eval      arttda=oldfl2
023600150618     c                   eval      artdab=olddab
023700161103     c                   select
023800161103     c* se dirottata visualizzo quello che c'è in BLT
023900161103     c                   when      arbcca= '1'
024000161103
024100161103     c* se resa visualizzo ART se BLT è vuota  altrimenti BLT
024200161103     c                   when      arbcca= '2'  and artdcm=0
024300161103     c                   eval      artdcm=olddcm
024400161103     c                   when      arbcca= '6'  and artdcm=0
024500161103     c                   eval      artdcm=olddcm
024600161103     c                   other
024700161103     c                   eval      artdcm=olddcm
024800161103     c                   endsl
024900150618     c                   eval      artcca=oldfl1
025000150618     c* Verifico se si tratta di bolla export
025100150618     c     oldlna        chain     azorg01l
025200150618    4c                   if        %found(azorg01l)
025300150618     c                   movel     orgde3        og143
025400150618     c                   else
025500150618     c                   clear                   og143
025600150618    4c                   endif
025700150618     c                   if        §ogntw='EEX' or §ogntw='FED' or
025800150618     c                             §ogntw='DPD'
025900150618     c                   eval      artdmf=olddtr
026000150618    5c                   endif
026100150618     c                   endsr
026200051012     c*------------------------------------------------------------------
026300120618     c     Altredate     BEGSR
026400160919     c                   clear                   Wora              4 0
026500160919     c                   clear                   Wdata             8 0
026600161031     c                   clear                   WNOSPU            1
026700120618     c*
026800161116     c* se bolla < 1 anno  --> non faccio nulla
026900161116     c                   if        (arbaas*10000)+arbmgs<annomeno
027000161116     c                   eval      wNOSPU='N'
027100161116     c                   else
027200161031     c* se non ci sono spunte memorizzo
027300161031     c     kbrv          chain     fnbrv07l
027400161031   0ac                   if        not %found(fnbrv07l) or brvnvr='*'
027500161031     c                   eval      wNOSPU='N'
027600161031     c                   endif
027700161116     c                   endif
027800161031     c
027900161031     c* la lettura del fiar5 la faccio solo se non ci sono più spunte
028000161031    0c                   if        arbncl=1  and wnospu='N'
028100160919     c
028200120618     c     kar5          chain     fiar501l
028300120618    1c                   if        %found(fiar501l)
028400120618     c                   eval      dar5gen=ar5uni
028500120618    2c                   if        §AR5ARRDT>*zeros
028600160914     c* solo se spedizione di un collo
028700160919     c                   movel     §AR5ARRDT     artdiaf
028800120618     c                   movel     §AR5ARRHM     arthamf
028900120618     c* se la linea finale di arrivo = al terminal --> aggiorno anche HAM
029000120618    3c                   if        artlna=arbtfa  and artdam>0
029100160919     c                   movel     §AR5ARRDT     artdia
029200160919     c                   movel     §AR5ARRHM     artham
029300120618    3c                   endif
029400160914
029500120618    2c                   endif
029600120618    1c                   endif
029700160919    0c                   endif
029800160914     c*
029900160914     c* Se DAMF =0
030000160914    1c                   if        arbtfa<>artlna and artdamf=0
030100161031     c                             and artdam>0  and wnospu=' '
030200160914     c     kbrv          setll     fnbrv07l
030300160914     c     kbrv          reade     fnbrv07l
030400160914   1ac                   dow       not %eof(fnbrv07l)
030500160914    2c                   if        brvdcs>0
030600160914     c* prendo il terminal di arrivo della spunta
030700160914     c                   clear                   fnlv55ds
030800160914     c
030900160914    3c                   if        brvpes<>artlna
031000160914     c                   movel     '6'           d55tpt
031100160914     c                   movel     artlna        d55lin
031200160914     c                   movel     brvdcs        d55drf
031300160914     C                   CALL      'FNLV55R'
031400160914     C                   PARM                    fnlv55ds
031500160914    3c                   endif
031600160914     c
031700160914    3c                   if        brvpes=artlna or brvpes=d55tfa
031800160914     c                   clear                   fnlv53ds
031900160914     C                   eval      D53NFV=brvnfv
032000160914     C                   MOVEL     brvNPG        D53NPG
032100160914     C                   MOVEL     brvFGS        D53FGS
032200160914     C                   CALL      'FNLV53R'
032300160914     C                   PARM                    fnlv53ds
032400160914
032500160914     c* utilizzo la spunta, per le bolle locali, solo se fatta in ambito "arrivo"
032600160914    4c                   select
032700160914     c                   when       arblnp=arblna and artlna=arblna
032800160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
032900160914     c                   when      arbtfp=arbtfa and artlna=arbtfa
033000160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
033100160914     c                   when      arbtfp=arbtfa and d55tfa=arbtfa
033200160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
033300160914   x4c                   other
033400160914     c
033500160914     c* memorizzo data più recente di spunta
033600160914    5c                   if        d53err<>' '
033700160914     c                   eval      d53dfv=brvdcs
033800160914    5c                   endif
033900160914     c
034000160914    5c                   if        d53dfv<artdamf or artdamf=0
034100160914     c                   eval      artdamf=d53dfv
034200160914     c                   eval      artnapf=brvnps
034300160919
034400160919     c                   exsr      CalcIMMISS
034500160919     c                   eval      artdiaf=wdata
034600160919     c                   eval      arthamf=wora
034700160914     c
034800160914   x5c                   else
034900160919     c                   exsr      CalcIMMISS
035000160919
035100160919    6c                   if        (wdata < artdiaf) or
035200160919     c                             (wdata = artdiaf and wora<arthamf)
035300160914     c                   eval      arthamf=wora
035400160919     c                   eval      artdiaf=wdata
035500160914    6c                   endif
035600160914     c
035700160914    5c                   endif
035800160914
035900160914    4c                   endsl
036000160914    3c                   endif
036100160914     c
036200160914    2c                   endif
036300160914     c
036400160914     c     kbrv          reade     fnbrv07l
036500160914   1ac                   enddo
036600160914     c
036700160914    1c                   endif
036800120618     c*
036900160919     c* leggo per aggiornare ARTHAM/ARTDIA/ARTFLNAP
037000161031    1c                   if        artdam>0 and artdia=0 and wnospu=' '
037100120618     c     kbrv          setll     fnbrv07l
037200120618     c     kbrv          reade     fnbrv07l
037300160914   1ac                   dow       not %eof(fnbrv07l)
037400160922    2c                   if        (brvnps=artnap or w_fnart='N')
037500160922     c                             and brvdcs>0
037600120618     c* controllo corrispondenza data
037700120618     c                   clear                   fnlv53ds
037800120618     C                   eval      D53NFV=brvnfv
037900120618     C                   MOVEL     brvNPG        D53NPG
038000120618     C                   MOVEL     brvFGS        D53FGS
038100120618     C                   CALL      'FNLV53R'
038200120618     C                   PARM                    fnlv53ds
038300120618    3c                   if        d53dfv=artdam
038400120618     c
038500120618    4c                   if        brvfgs<>arbtfa and brvfgs<>arblna
038600120618     c* prendo il terminal di arrivo della spunta
038700120618     c                   clear                   fnlv55ds
038800120618     c                   movel     'A'           d55tpt
038900120618     c                   movel     brvfgs        d55lin
039000120618     c                   movel     d53dfv        d55drf
039100120618     C                   CALL      'FNLV55R'
039200120618     C                   PARM                    fnlv55ds
039300120618    4c                   endif
039400120618     c
039500120618    4c                   if        brvfgs=arbtfa or brvfgs=arblna or
039600120618     c                             d55tfa=arbtfa
039700160919     c                   exsr      CalcIMMISS
039800160914     c
039900160919    5c                   if        artdia=0  or (wdata<artdia) or
040000160919     c                             (wdata=artdia and wora<artham)
040100160914     c                   eval      artham=wora
040200160919     c                   eval      artdia=wdata
040300160919     c                   eval      artflnap=brvpes
040400160922     c                   if        w_fnart='N'
040500160922     C                   eval      artnap=brvnps
040600160922     c                   endif
040700160922     c
040800160919    5c                   endif
040900120618     c
041000120618    4c                   endif
041100120618    3c                   endif
041200120618    2c                   endif
041300120618     c
041400120618     c     kbrv          reade     fnbrv07l
041500120618   1ac                   enddo
041600120618     c
041700160920     c* se arbtfa=artlna --> imposto anche le date all'arrivo finale
041800160920    2c                   if        arbtfa=artlna
041900160920     c                   eval      arthamf=artham
042000160920     c                   eval      artdiaf=artdia
042100160920    2c                   endif
042200160920    1c                   endif
042300120618     c
042400120618     c* Se ARTHSE vuoto --> leggo da spunte
042500161031    1c                   if        artdie=0 and artdse>0 and wnospu=' '
042600120618     c     kbrv          setll     fnbrv07l
042700120618     c     kbrv          reade     fnbrv07l
042800160914   1ac                   dow       not %eof(fnbrv07l)
042900120618    2c                   if        brvnps=artnpe and brvdcs>0
043000120618     c* controllo corrispondenza data
043100120618     c                   clear                   fnlv53ds
043200120618     C                   eval      D53NFV=brvnfv
043300120618     C                   MOVEL     brvNPG        D53NPG
043400120618     C                   MOVEL     brvFGS        D53FGS
043500120618     C                   CALL      'FNLV53R'
043600120618     C                   PARM                    fnlv53ds
043700120618    3c                   if        d53dfv=artdse
043800120618     c
043900120618    4c                   if        brvfgs<>arbtfp and brvfgs<>arblnp
044000120618     c* prendo il terminal di arrivo della spunta
044100120618     c                   clear                   fnlv55ds
044200120618     c                   movel     'P'           d55tpt
044300120618     c                   movel     brvfgs        d55lin
044400120618     c                   movel     d53dfv        d55drf
044500120618     C                   CALL      'FNLV55R'
044600120618     C                   PARM                    fnlv55ds
044700120618    4c                   endif
044800120618     c
044900120618    4c                   if        brvfgs=arbtfp or brvfgs=arblnp or
045000120618     c                             d55tfp=arbtfp
045100160919     c                   exsr      CalcIMMISS
045200160914
045300160919    5c                   if        artdie=0 or (wdata<artdie) or
045400160919     c                             (artdie=wdata and wora<arthse)
045500160914     c                   eval      arthse=wora
045600160919     c                   eval      artdie=wdata
045700160914    5c                   endif
045800120618     c
045900120618    4c                   endif
046000120618    3c                   endif
046100120618    2c                   endif
046200120618     c
046300120618     c     kbrv          reade     fnbrv07l
046400120618   1ac                   enddo
046500120618     c
046600120618    1c                   endif
046700120618     c
046800120618     c* ORA CONSEGNA MERCE
046900120618     C                   if        artdcm>0
047000120618     c
047100120618     c                   if        arthcm=0
047200120618     c                   movel     arbhmc        arthcm
047300120618    1c                   endif
047400120618     c
047500120618     c* se negli eventi c'e' consegna parziale --> imposto per il dettaglio con data diversa
047600120618     c     kevb          setll     fnevb04l
047700120618     c     kevb          reade     fnevb04l
047800120618     c                   dow       not  %eof(fnevb04l)
047900120618     c*
048000120618     c* se la data consegna sul dett colli corrrisponde alla data evento
048100120618     c*  come orario imposto quello dell'evento
048200120618     c                   if        artdcm=evbdev
048300120618     c                   movel     evbhev        arthcm
048400120618     c                   leave
048500120618     c                   endif
048600120618     c
048700120618     c     kevb          reade     fnevb04l
048800120618     c                   enddo
048900120618     c                   endif
049000160920
049100160920     c* Per bolle export, se ci sono le spunte, cerco spunta manifest
049200160920    1c                   if        §ogntw='EEX' or §ogntw='FED' or
049300161031     c                             §ogntw='DPD'  and wnospu=' '
049400160920    2c                   if        artdmf>0
049500160920     c     kbrv5         setll     fnbrv05l
049600160920     c     kbrv5         reade     fnbrv05l
049700160920    3c                   dow       not%eof(fnbrv05l)
049800160920     c* controllo corrispondenza data
049900160920     c                   clear                   fnlv53ds
050000160920     C                   eval      D53NFV=brvnfv
050100160920     C                   MOVEL     brvNPG        D53NPG
050200160920     C                   MOVEL     brvFGS        D53FGS
050300160920     C                   CALL      'FNLV53R'
050400160920     C                   PARM                    fnlv53ds
050500160920    4c                   if        d53dfv=artdmf and d53def='S'
050600160920     c                   eval      artnpdmf=brvnps
050700160920    4c                   endif
050800160920
050900160920     c     kbrv5         reade     fnbrv05l
051000160920    3c                   enddo
051100160920    2c                   endif
051200160920    1c                   endif
051300120618     c
051400120618     c                   ENDSR
051500160920     C**************************************************************************
051600160919     c     calcIMMISS    BEGSR
051700160919    6c                   if        ((brvdfs<brvdcs) or
051800160919     c                             (brvdfs=brvdcs and brvhms<brvhcs))
051900160919     c                   movel     brvhms        wora
052000160919     c                   movel     brvdfs        wdata
052100160919   x6c                   else
052200160919     c                   movel     brvhcs        wora
052300160919     c                   movel     brvdcs        wdata
052400160919    6c                   endif
052500160919     c                   ENDSR
052600150618     C**************************************************************************
052700150618     c     *inzsr        begsr
052800150618     c     Kbolla        Klist
052900150618     c                   Kfld                    i38aas
053000150618     c                   Kfld                    i38lnp
053100150618     c                   Kfld                    i38nrs
053200150618     c                   Kfld                    i38nsp
053300150618     c     Kdett         Klist
053400150618     c                   Kfld                    i38aas
053500150618     c                   Kfld                    i38lnp
053600150618     c                   Kfld                    i38nrs
053700150618     c                   Kfld                    i38nsp
053800150618     c                   Kfld                    i38nsc
053900150618     c     Kar5          Klist
054000150618     c                   Kfld                    artaas
054100150618     c                   Kfld                    artlnp
054200150618     c                   Kfld                    artnrs
054300150618     c                   Kfld                    artnsp
054400150618     c                   Kfld                    ktrd
054500150618     c     Kbrv          Klist
054600150618     c                   Kfld                    artfls
054700150618     c                   Kfld                    artlna
054800150618     c                   Kfld                    artnrs
054900150618     c                   Kfld                    artnsc
055000160920     c     Kbrv5         Klist
055100160920     c                   Kfld                    knpg
055200160920     c                   Kfld                    artfls
055300160920     c                   Kfld                    artlna
055400160920     c                   Kfld                    artnrs
055500160920     c                   Kfld                    artnsc
055600150618     c     KEVB          Klist
055700150618     c                   Kfld                    artaas
055800150618     c                   Kfld                    artlnp
055900150618     c                   Kfld                    artnrs
056000150618     c                   Kfld                    artnsp
056100150618     c                   Kfld                    kcev
056200161116     c
056300161116     c                   subdur    1:*y          dataiso
056400161116     c                   movel     dataiso       annomeno          8 0
056500161116     c
056600150618     c                   ENDSR
