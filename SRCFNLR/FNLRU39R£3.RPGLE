000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200160919     H* FNLRU39R *---------------------------------------------------*
000300160919     H*     elaborazione dati per file  FIART00F
000400000000     H*--------------------------------------------------------------*
000500120618     fFNart01L  if   e           k Disk
000600090225     f                                     prefix(old:3)
000700090225     fFNBLt01L  if   e           k Disk
000800120618     fFnBLp01L  if   e           k Disk    prefix(arb:3)
000900090226     fazorg01l  if   e           k Disk
001000120618     ffiar501l  if   e           k Disk
001100120618     ffnbrv07l  if   e           k Disk
001200160920     ffnbrv05l  if   e           k Disk    rename(fnbrv000:fnbrv005)
001300120618     ffnevb04l  if   e           k Disk
001400051012     F
001500060215     d
001600051012     D
001700120618     d ktrd            s                   like(ar5trd) inz('GEN')
001800120618     d kcev            s                   like(evbcev) inz('P  ')
001900150618     d w_fnart         s              1
002000150618     d w_fnblt         s              1
002100160920     d knpg            s              1  0 inz(1)
002200150618     d keybolla        s             16
002300150618     d savbolla        s                   like(keybolla)
002400120618     d dar5gen       e ds
002500120618     d og143         e ds
002600120618     d fnlv53ds      e ds
002700120618     d fnlv55ds      e ds
002800160919     d FiartDs       e ds                  extname(fiart50f)
002900160919     d fnlru39ds     e ds
003000051012     C
003100150618     c     *entry        plist
003200160919     c                   parm                    fnlru39ds
003300150618     c                   parm                    FiartDs
003400060217      *
003500051019     c
003600150618    1C     i38tla        IFNE      'C'
003700150618
003800150618     c                   clear                   fiartds
003900150618
004000150618     c* chain la testata bolla solo se diversa da quella elaborata precedentemente
004100150618     c                   eval      keybolla=%editc(i38aas:'X')
004200150618     c                             +%editc(i38lnp:'X')
004300150618     c                             +%editc(i38nrs:'X') + %editc(i38nsp:'X')
004400150618    2c                   if        keybolla<>savbolla
004500150618     c     kbolla        chain     fnblp01l
004600150618     c                   eval      savbolla=keybolla
004700150618    2c                   endif
004800160914
004900150618     c* Dati da dettaglio colli in partenza
005000150618     c                   clear                   w_fnblt
005100150618     c                   clear                   w_fnart
005200150618     c     kdett         chain     fnblt01l
005300150618     c                   if        not %found(fnblt01l)
005400150618     c                   eval      w_fnblt='N'
005500150618     c                   endif
005600150618     c     kdett         chain     fnart01l
005700150618     c                   if        not %found(fnart01l)
005800150618     c                   eval      w_fnart='N'
005900150618     c                   endif
006000150618     c                   if        w_fnblt=*blanks
006100150618     c                   exsr      sr_dablt
006200150618     c                   endif
006300150618     c* Dati da dettaglio colli in arrivo
006400150618     c                   if        w_fnart=*blanks
006500150618     c                   exsr      sr_daart
006600150618     c                   endif
006700150618     c* chain su spunte e fiar5 per le data e ora mancanti
006800150618     c                   exsr      altredate
006900150618
007000150618    1c                   endif
007100150618
007200150618     C     I38tla        IFNE      ' '
007300150618     c                   eval      d53tla='C'
007400150618     c                   call      'FNLV53R'
007500150618     c                   parm                    fnlv53ds
007600150618     c
007700150618     c                   eval      d55tla='C'
007800150618     c                   call      'FNLV55R'
007900150618     c                   parm                    fnlv55ds
008000150618
008100150618
008200150618     C                   SETON                                        LR
008300150618     C                   ELSE
008400150618     C                   SETON                                        RT
008500150618     C                   ENDIF
008600150618     c
008700051012     c***
008800150618     c*------------------------------------------------------------------
008900150618     c     sr_DaBlt      BEGSR
009000150618     c                   eval      artaas=bltaas
009100150618     c                   eval      artlnp=bltlnp
009200150618     c                   eval      artnrs=bltnrs
009300150618     c                   eval      artnsp=bltnsp
009400150618     c                   eval      artlna=bltlna
009500150618     c                   eval      artfls=bltfls
009600150618     c                   eval      artnsc=bltnsc
009700150618     c                   eval      artvuc=bltvuc
009800150618     c                   eval      artfvc=bltfvc
009900150618     c                   eval      artpuc=bltpuc
010000150618     c                   eval      artfpc=bltfpc
010100150618     c                   eval      artdfv=bltdfv
010200150618     c                   eval      artnpp=bltnpp
010300150618     c                   eval      artnfv=bltnfv
010400150618     c                   eval      artdse=bltdse
010500150618     c                   eval      artnpe=bltnpe
010600150618     c                   eval      artdet=bltdet
010700150618     c                   eval      artdam=bltdam
010800150618    3c                   if        artlna=arbtfa  and artdam>0
010900150618     c                   eval      artdamf=bltdam
011000150618     c                   endif
011100150618     c
011200150618     c                   eval      artdut=bltdut
011300150618     c                   eval      artdcm=bltdcm
011400150618     c                   eval      artcan=bltcan
011500150618    4c                   if        artcan<>' '
011600150618     c                   eval      artfta='P'
011700150618    4c                   endif
011800150618     c                   eval      artcca=bltfl1
011900150618     c                   eval      arttda=bltfl2
012000150618     c                   eval      artftr=bltftr
012100150618     c* Se bltftr=C significa trasmesso al cliente e quindi imposto
012200150618     c*  la data trasmissione al cliente
012300150618    4c                   if        bltftr='C'
012400150618     c                   eval      artdtc=artdcm
012500150618    4c                   endif
012600150618     c* Verifico se si tratta di bolla export
012700150618     c     bltlna        chain     azorg01l
012800150618    4c                   if        %found(azorg01l)
012900150618     c                   movel     orgde3        og143
013000150618     c                   else
013100150618     c                   clear                   og143
013200150618    4c                   endif
013300150618     c
013400150618     c* Se non c'e' il record in arrivo
013500150618    4C                   IF        w_fnart='N'
013600150618     c* Se non trovo bolla in arb e la bolla è già stata trasmessa e
013700150618     c*  consegnata, significa che è stata pulita da fnarb
013800161031    5c                   if        arbdcm>0 and arbft1<>' '
013900150618     c                   eval      artbga='C'
014000150618     c                   eval      artabn='S'
014100150618    6c                   if        §ogntw='EEX' or §ogntw='FED' or
014200150618     c                             §ogntw='DPD'
014300150618     c                   eval      artdmf=artdam
014400150618    6c                   endif
014500150618    5c                   endif
014600161031
014700161031    5c                   if        arbdcm=0 and arbft1<>' '
014800161031     c                   eval      artbga='D'
014900161031     c                   eval      artabn='S'
015000161031     c                   endif
015100150618     c
015200150618   x4c                   else
015300150618     c                   eval      artbga='S'
015400150618     c
015500150618    5c                   if        artvuc=0 and oldvuc>0
015600150618     c                   eval      artvuc=oldvuc
015700150618     c                   eval      artfvc=oldfvc
015800150618    5c                   endif
015900150618    5c                   if        artpuc=0 and oldpuc>0
016000150618     c                   eval      artpuc=oldpuc
016100150618     c                   eval      artfpc=oldfpc
016200150618    5c                   endif
016300150618    c
016400150618    4c                   endif
016500150618     c                   endsr
016600150618     c*------------------------------------------------------------------
016700150618     c     sr_DaArt      BEGSR
016800150618     c* i campi seguenti li imposto solo se non ho trovato dettaglio colli
016900150618     c* bolla part.
017000150618     c                   if        w_fnblt='N'
017100150618     c                   eval      artbga='A'
017200150618     c                   eval      artaas=oldaas
017300150618     c                   eval      artlnp=oldlnp
017400150618     c                   eval      artnrs=oldnrs
017500150618     c                   eval      artnsp=oldnsp
017600150618     c                   eval      artlna=oldlna
017700150618     c                   eval      artfls=oldfls
017800150618     c                   eval      artnsc=oldnsc
017900150618     c                   eval      artvuc=oldvuc
018000150618     c                   eval      artfvc=oldfvc
018100150618     c                   eval      artpuc=oldpuc
018200150618     c                   eval      artfpc=oldfpc
018300150618     c                   endif
018400150618     c                   eval      artdfv=olddfv
018500150618     c                   eval      artnpp=oldnpp
018600150618     c                   eval      artnfv=oldnfv
018700150618     c                   eval      artflp=oldflp
018800150618     c                   eval      artabn=oldabn
018900160922     c                   if        artcan<>' '  and artfta=' '
019000160922     c                   eval      artcan=oldcan
019100150618     c                   eval      artfta='A'
019200150618     c                   endif
019300150618     c                   eval      artdet=olddet
019400150618     c                   eval      artpet=oldpet
019500150618     c                   eval      artdut=olddut
019600150618     c                   eval      artput=oldput
019700150618     c                   eval      artdam=olddam
019800150618    3c                   if        artlna=arbtfa  and artdam>0
019900150618     c                   eval      artdamf=olddam
020000160914     c                   eval      artnapf=oldnap
020100150618     c                   endif
020200150618     c
020300150618     c                   eval      artnap=oldnap
020400150618     c                   eval      arttda=oldfl2
020500150618     c                   eval      artdab=olddab
020600150618     c                   eval      artdcm=olddcm
020700150618     c                   eval      artcca=oldfl1
020800150618     c* Verifico se si tratta di bolla export
020900150618     c     oldlna        chain     azorg01l
021000150618    4c                   if        %found(azorg01l)
021100150618     c                   movel     orgde3        og143
021200150618     c                   else
021300150618     c                   clear                   og143
021400150618    4c                   endif
021500150618     c                   if        §ogntw='EEX' or §ogntw='FED' or
021600150618     c                             §ogntw='DPD'
021700150618     c                   eval      artdmf=olddtr
021800150618    5c                   endif
021900150618     c                   endsr
022000051012     c*------------------------------------------------------------------
022100120618     c     Altredate     BEGSR
022200160919     c                   clear                   Wora              4 0
022300160919     c                   clear                   Wdata             8 0
022400161031     c                   clear                   WNOSPU            1
022500120618     c*
022600161031     c* se non ci sono spunte memorizzo
022700161031     c     kbrv          chain     fnbrv07l
022800161031   0ac                   if        not %found(fnbrv07l) or brvnvr='*'
022900161031     c                   eval      wNOSPU='N'
023000161031     c                   endif
023100161031     c
023200161031     c* la lettura del fiar5 la faccio solo se non ci sono più spunte
023300161031    0c                   if        arbncl=1  and wnospu='N'
023400160919     c
023500120618     c     kar5          chain     fiar501l
023600120618    1c                   if        %found(fiar501l)
023700120618     c                   eval      dar5gen=ar5uni
023800120618    2c                   if        §AR5ARRDT>*zeros
023900160914     c* solo se spedizione di un collo
024000160919     c                   movel     §AR5ARRDT     artdiaf
024100120618     c                   movel     §AR5ARRHM     arthamf
024200120618     c* se la linea finale di arrivo = al terminal --> aggiorno anche HAM
024300120618    3c                   if        artlna=arbtfa  and artdam>0
024400160919     c                   movel     §AR5ARRDT     artdia
024500160919     c                   movel     §AR5ARRHM     artham
024600120618    3c                   endif
024700160914
024800120618    2c                   endif
024900120618    1c                   endif
025000160919    0c                   endif
025100160914     c*
025200160914     c* Se DAMF =0
025300160914    1c                   if        arbtfa<>artlna and artdamf=0
025400161031     c                             and artdam>0  and wnospu=' '
025500160914     c     kbrv          setll     fnbrv07l
025600160914     c     kbrv          reade     fnbrv07l
025700160914   1ac                   dow       not %eof(fnbrv07l)
025800160914    2c                   if        brvdcs>0
025900160914     c* prendo il terminal di arrivo della spunta
026000160914     c                   clear                   fnlv55ds
026100160914     c
026200160914    3c                   if        brvpes<>artlna
026300160914     c                   movel     '6'           d55tpt
026400160914     c                   movel     artlna        d55lin
026500160914     c                   movel     brvdcs        d55drf
026600160914     C                   CALL      'FNLV55R'
026700160914     C                   PARM                    fnlv55ds
026800160914    3c                   endif
026900160914     c
027000160914    3c                   if        brvpes=artlna or brvpes=d55tfa
027100160914     c                   clear                   fnlv53ds
027200160914     C                   eval      D53NFV=brvnfv
027300160914     C                   MOVEL     brvNPG        D53NPG
027400160914     C                   MOVEL     brvFGS        D53FGS
027500160914     C                   CALL      'FNLV53R'
027600160914     C                   PARM                    fnlv53ds
027700160914
027800160914     c* utilizzo la spunta, per le bolle locali, solo se fatta in ambito "arrivo"
027900160914    4c                   select
028000160914     c                   when       arblnp=arblna and artlna=arblna
028100160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
028200160914     c                   when      arbtfp=arbtfa and artlna=arbtfa
028300160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
028400160914     c                   when      arbtfp=arbtfa and d55tfa=arbtfa
028500160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
028600160914   x4c                   other
028700160914     c
028800160914     c* memorizzo data più recente di spunta
028900160914    5c                   if        d53err<>' '
029000160914     c                   eval      d53dfv=brvdcs
029100160914    5c                   endif
029200160914     c
029300160914    5c                   if        d53dfv<artdamf or artdamf=0
029400160914     c                   eval      artdamf=d53dfv
029500160914     c                   eval      artnapf=brvnps
029600160919
029700160919     c                   exsr      CalcIMMISS
029800160919     c                   eval      artdiaf=wdata
029900160919     c                   eval      arthamf=wora
030000160914     c
030100160914   x5c                   else
030200160919     c                   exsr      CalcIMMISS
030300160919
030400160919    6c                   if        (wdata < artdiaf) or
030500160919     c                             (wdata = artdiaf and wora<arthamf)
030600160914     c                   eval      arthamf=wora
030700160919     c                   eval      artdiaf=wdata
030800160914    6c                   endif
030900160914     c
031000160914    5c                   endif
031100160914
031200160914    4c                   endsl
031300160914    3c                   endif
031400160914     c
031500160914    2c                   endif
031600160914     c
031700160914     c     kbrv          reade     fnbrv07l
031800160914   1ac                   enddo
031900160914     c
032000160914    1c                   endif
032100120618     c*
032200160919     c* leggo per aggiornare ARTHAM/ARTDIA/ARTFLNAP
032300161031    1c                   if        artdam>0 and artdia=0 and wnospu=' '
032400120618     c     kbrv          setll     fnbrv07l
032500120618     c     kbrv          reade     fnbrv07l
032600160914   1ac                   dow       not %eof(fnbrv07l)
032700160922    2c                   if        (brvnps=artnap or w_fnart='N')
032800160922     c                             and brvdcs>0
032900120618     c* controllo corrispondenza data
033000120618     c                   clear                   fnlv53ds
033100120618     C                   eval      D53NFV=brvnfv
033200120618     C                   MOVEL     brvNPG        D53NPG
033300120618     C                   MOVEL     brvFGS        D53FGS
033400120618     C                   CALL      'FNLV53R'
033500120618     C                   PARM                    fnlv53ds
033600120618    3c                   if        d53dfv=artdam
033700120618     c
033800120618    4c                   if        brvfgs<>arbtfa and brvfgs<>arblna
033900120618     c* prendo il terminal di arrivo della spunta
034000120618     c                   clear                   fnlv55ds
034100120618     c                   movel     'A'           d55tpt
034200120618     c                   movel     brvfgs        d55lin
034300120618     c                   movel     d53dfv        d55drf
034400120618     C                   CALL      'FNLV55R'
034500120618     C                   PARM                    fnlv55ds
034600120618    4c                   endif
034700120618     c
034800120618    4c                   if        brvfgs=arbtfa or brvfgs=arblna or
034900120618     c                             d55tfa=arbtfa
035000160919     c                   exsr      CalcIMMISS
035100160914     c
035200160919    5c                   if        artdia=0  or (wdata<artdia) or
035300160919     c                             (wdata=artdia and wora<artham)
035400160914     c                   eval      artham=wora
035500160919     c                   eval      artdia=wdata
035600160919     c                   eval      artflnap=brvpes
035700160922     c                   if        w_fnart='N'
035800160922     C                   eval      artnap=brvnps
035900160922     c                   endif
036000160922     c
036100160919    5c                   endif
036200120618     c
036300120618    4c                   endif
036400120618    3c                   endif
036500120618    2c                   endif
036600120618     c
036700120618     c     kbrv          reade     fnbrv07l
036800120618   1ac                   enddo
036900120618     c
037000160920     c* se arbtfa=artlna --> imposto anche le date all'arrivo finale
037100160920    2c                   if        arbtfa=artlna
037200160920     c                   eval      arthamf=artham
037300160920     c                   eval      artdiaf=artdia
037400160920    2c                   endif
037500160920    1c                   endif
037600120618     c
037700120618     c* Se ARTHSE vuoto --> leggo da spunte
037800161031    1c                   if        artdie=0 and artdse>0 and wnospu=' '
037900120618     c     kbrv          setll     fnbrv07l
038000120618     c     kbrv          reade     fnbrv07l
038100160914   1ac                   dow       not %eof(fnbrv07l)
038200120618    2c                   if        brvnps=artnpe and brvdcs>0
038300120618     c* controllo corrispondenza data
038400120618     c                   clear                   fnlv53ds
038500120618     C                   eval      D53NFV=brvnfv
038600120618     C                   MOVEL     brvNPG        D53NPG
038700120618     C                   MOVEL     brvFGS        D53FGS
038800120618     C                   CALL      'FNLV53R'
038900120618     C                   PARM                    fnlv53ds
039000120618    3c                   if        d53dfv=artdse
039100120618     c
039200120618    4c                   if        brvfgs<>arbtfp and brvfgs<>arblnp
039300120618     c* prendo il terminal di arrivo della spunta
039400120618     c                   clear                   fnlv55ds
039500120618     c                   movel     'P'           d55tpt
039600120618     c                   movel     brvfgs        d55lin
039700120618     c                   movel     d53dfv        d55drf
039800120618     C                   CALL      'FNLV55R'
039900120618     C                   PARM                    fnlv55ds
040000120618    4c                   endif
040100120618     c
040200120618    4c                   if        brvfgs=arbtfp or brvfgs=arblnp or
040300120618     c                             d55tfp=arbtfp
040400160919     c                   exsr      CalcIMMISS
040500160914
040600160919    5c                   if        artdie=0 or (wdata<artdie) or
040700160919     c                             (artdie=wdata and wora<arthse)
040800160914     c                   eval      arthse=wora
040900160919     c                   eval      artdie=wdata
041000160914    5c                   endif
041100120618     c
041200120618    4c                   endif
041300120618    3c                   endif
041400120618    2c                   endif
041500120618     c
041600120618     c     kbrv          reade     fnbrv07l
041700120618   1ac                   enddo
041800120618     c
041900120618    1c                   endif
042000120618     c
042100120618     c* ORA CONSEGNA MERCE
042200120618     C                   if        artdcm>0
042300120618     c
042400120618     c                   if        arthcm=0
042500120618     c                   movel     arbhmc        arthcm
042600120618    1c                   endif
042700120618     c
042800120618     c* se negli eventi c'e' consegna parziale --> imposto per il dettaglio con data diversa
042900120618     c     kevb          setll     fnevb04l
043000120618     c     kevb          reade     fnevb04l
043100120618     c                   dow       not  %eof(fnevb04l)
043200120618     c*
043300120618     c* se la data consegna sul dett colli corrrisponde alla data evento
043400120618     c*  come orario imposto quello dell'evento
043500120618     c                   if        artdcm=evbdev
043600120618     c                   movel     evbhev        arthcm
043700120618     c                   leave
043800120618     c                   endif
043900120618     c
044000120618     c     kevb          reade     fnevb04l
044100120618     c                   enddo
044200120618     c                   endif
044300160920
044400160920     c* Per bolle export, se ci sono le spunte, cerco spunta manifest
044500160920    1c                   if        §ogntw='EEX' or §ogntw='FED' or
044600161031     c                             §ogntw='DPD'  and wnospu=' '
044700160920    2c                   if        artdmf>0
044800160920     c     kbrv5         setll     fnbrv05l
044900160920     c     kbrv5         reade     fnbrv05l
045000160920    3c                   dow       not%eof(fnbrv05l)
045100160920     c* controllo corrispondenza data
045200160920     c                   clear                   fnlv53ds
045300160920     C                   eval      D53NFV=brvnfv
045400160920     C                   MOVEL     brvNPG        D53NPG
045500160920     C                   MOVEL     brvFGS        D53FGS
045600160920     C                   CALL      'FNLV53R'
045700160920     C                   PARM                    fnlv53ds
045800160920    4c                   if        d53dfv=artdmf and d53def='S'
045900160920     c                   eval      artnpdmf=brvnps
046000160920    4c                   endif
046100160920
046200160920     c     kbrv5         reade     fnbrv05l
046300160920    3c                   enddo
046400160920    2c                   endif
046500160920    1c                   endif
046600120618     c
046700120618     c                   ENDSR
046800160920     C**************************************************************************
046900160919     c     calcIMMISS    BEGSR
047000160919    6c                   if        ((brvdfs<brvdcs) or
047100160919     c                             (brvdfs=brvdcs and brvhms<brvhcs))
047200160919     c                   movel     brvhms        wora
047300160919     c                   movel     brvdfs        wdata
047400160919   x6c                   else
047500160919     c                   movel     brvhcs        wora
047600160919     c                   movel     brvdcs        wdata
047700160919    6c                   endif
047800160919     c                   ENDSR
047900150618     C**************************************************************************
048000150618     c     *inzsr        begsr
048100150618     c     Kbolla        Klist
048200150618     c                   Kfld                    i38aas
048300150618     c                   Kfld                    i38lnp
048400150618     c                   Kfld                    i38nrs
048500150618     c                   Kfld                    i38nsp
048600150618     c     Kdett         Klist
048700150618     c                   Kfld                    i38aas
048800150618     c                   Kfld                    i38lnp
048900150618     c                   Kfld                    i38nrs
049000150618     c                   Kfld                    i38nsp
049100150618     c                   Kfld                    i38nsc
049200150618     c     Kar5          Klist
049300150618     c                   Kfld                    artaas
049400150618     c                   Kfld                    artlnp
049500150618     c                   Kfld                    artnrs
049600150618     c                   Kfld                    artnsp
049700150618     c                   Kfld                    ktrd
049800150618     c     Kbrv          Klist
049900150618     c                   Kfld                    artfls
050000150618     c                   Kfld                    artlna
050100150618     c                   Kfld                    artnrs
050200150618     c                   Kfld                    artnsc
050300160920     c     Kbrv5         Klist
050400160920     c                   Kfld                    knpg
050500160920     c                   Kfld                    artfls
050600160920     c                   Kfld                    artlna
050700160920     c                   Kfld                    artnrs
050800160920     c                   Kfld                    artnsc
050900150618     c     KEVB          Klist
051000150618     c                   Kfld                    artaas
051100150618     c                   Kfld                    artlnp
051200150618     c                   Kfld                    artnrs
051300150618     c                   Kfld                    artnsp
051400150618     c                   Kfld                    kcev
051500150618     c                   ENDSR
