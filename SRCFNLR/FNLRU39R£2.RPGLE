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
013800150618    5c                   if        arbdcm>0 and arbdt2>0
013900150618     c                   eval      artbga='C'
014000150618     c                   eval      artabn='S'
014100150618    6c                   if        §ogntw='EEX' or §ogntw='FED' or
014200150618     c                             §ogntw='DPD'
014300150618     c                   eval      artdmf=artdam
014400150618    6c                   endif
014500150618    5c                   endif
014600150618     c
014700150618   x4c                   else
014800150618     c                   eval      artbga='S'
014900150618     c
015000150618    5c                   if        artvuc=0 and oldvuc>0
015100150618     c                   eval      artvuc=oldvuc
015200150618     c                   eval      artfvc=oldfvc
015300150618    5c                   endif
015400150618    5c                   if        artpuc=0 and oldpuc>0
015500150618     c                   eval      artpuc=oldpuc
015600150618     c                   eval      artfpc=oldfpc
015700150618    5c                   endif
015800150618    c
015900150618    4c                   endif
016000150618     c                   endsr
016100150618     c*------------------------------------------------------------------
016200150618     c     sr_DaArt      BEGSR
016300150618     c* i campi seguenti li imposto solo se non ho trovato dettaglio colli
016400150618     c* bolla part.
016500150618     c                   if        w_fnblt='N'
016600150618     c                   eval      artbga='A'
016700150618     c                   eval      artaas=oldaas
016800150618     c                   eval      artlnp=oldlnp
016900150618     c                   eval      artnrs=oldnrs
017000150618     c                   eval      artnsp=oldnsp
017100150618     c                   eval      artlna=oldlna
017200150618     c                   eval      artfls=oldfls
017300150618     c                   eval      artnsc=oldnsc
017400150618     c                   eval      artvuc=oldvuc
017500150618     c                   eval      artfvc=oldfvc
017600150618     c                   eval      artpuc=oldpuc
017700150618     c                   eval      artfpc=oldfpc
017800150618     c                   endif
017900150618     c                   eval      artdfv=olddfv
018000150618     c                   eval      artnpp=oldnpp
018100150618     c                   eval      artnfv=oldnfv
018200150618     c                   eval      artflp=oldflp
018300150618     c                   eval      artabn=oldabn
018400160922     c                   if        artcan<>' '  and artfta=' '
018500160922     c                   eval      artcan=oldcan
018600150618     c                   eval      artfta='A'
018700150618     c                   endif
018800150618     c                   eval      artdet=olddet
018900150618     c                   eval      artpet=oldpet
019000150618     c                   eval      artdut=olddut
019100150618     c                   eval      artput=oldput
019200150618     c                   eval      artdam=olddam
019300150618    3c                   if        artlna=arbtfa  and artdam>0
019400150618     c                   eval      artdamf=olddam
019500160914     c                   eval      artnapf=oldnap
019600150618     c                   endif
019700150618     c
019800150618     c                   eval      artnap=oldnap
019900150618     c                   eval      arttda=oldfl2
020000150618     c                   eval      artdab=olddab
020100150618     c                   eval      artdcm=olddcm
020200150618     c                   eval      artcca=oldfl1
020300150618     c* Verifico se si tratta di bolla export
020400150618     c     oldlna        chain     azorg01l
020500150618    4c                   if        %found(azorg01l)
020600150618     c                   movel     orgde3        og143
020700150618     c                   else
020800150618     c                   clear                   og143
020900150618    4c                   endif
021000150618     c                   if        §ogntw='EEX' or §ogntw='FED' or
021100150618     c                             §ogntw='DPD'
021200150618     c                   eval      artdmf=olddtr
021300150618    5c                   endif
021400150618     c                   endsr
021500051012     c*------------------------------------------------------------------
021600120618     c     Altredate     BEGSR
021700160919     c                   clear                   Wora              4 0
021800160919     c                   clear                   Wdata             8 0
021900120618     c*
022000160914    0c                   if        arbncl=1
022100160919     c
022200160919     c* la lettura del fiar5 la faccio solo se non ci sono più spunte
022300160919     c     kbrv          chain     fnbrv07l
022400160919   0ac                   if        not %found(fnbrv07l) or brvnvr='*'
022500160919     c
022600120618     c     kar5          chain     fiar501l
022700120618    1c                   if        %found(fiar501l)
022800120618     c                   eval      dar5gen=ar5uni
022900120618    2c                   if        §AR5ARRDT>*zeros
023000160914     c* solo se spedizione di un collo
023100160919     c                   movel     §AR5ARRDT     artdiaf
023200120618     c                   movel     §AR5ARRHM     arthamf
023300120618     c* se la linea finale di arrivo = al terminal --> aggiorno anche HAM
023400120618    3c                   if        artlna=arbtfa  and artdam>0
023500160919     c                   movel     §AR5ARRDT     artdia
023600160919     c                   movel     §AR5ARRHM     artham
023700120618    3c                   endif
023800160914
023900120618    2c                   endif
024000120618    1c                   endif
024100160919   0ac                   endif
024200160919    0c                   endif
024300160914     c*
024400160914     c* Se DAMF =0
024500160914    1c                   if        arbtfa<>artlna and artdamf=0
024600160914     c                             and artdam>0
024700160914     c     kbrv          setll     fnbrv07l
024800160914     c     kbrv          reade     fnbrv07l
024900160914   1ac                   dow       not %eof(fnbrv07l)
025000160914    2c                   if        brvdcs>0
025100160914     c* prendo il terminal di arrivo della spunta
025200160914     c                   clear                   fnlv55ds
025300160914     c
025400160914    3c                   if        brvpes<>artlna
025500160914     c                   movel     '6'           d55tpt
025600160914     c                   movel     artlna        d55lin
025700160914     c                   movel     brvdcs        d55drf
025800160914     C                   CALL      'FNLV55R'
025900160914     C                   PARM                    fnlv55ds
026000160914    3c                   endif
026100160914     c
026200160914    3c                   if        brvpes=artlna or brvpes=d55tfa
026300160914     c                   clear                   fnlv53ds
026400160914     C                   eval      D53NFV=brvnfv
026500160914     C                   MOVEL     brvNPG        D53NPG
026600160914     C                   MOVEL     brvFGS        D53FGS
026700160914     C                   CALL      'FNLV53R'
026800160914     C                   PARM                    fnlv53ds
026900160914
027000160914     c* utilizzo la spunta, per le bolle locali, solo se fatta in ambito "arrivo"
027100160914    4c                   select
027200160914     c                   when       arblnp=arblna and artlna=arblna
027300160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
027400160914     c                   when      arbtfp=arbtfa and artlna=arbtfa
027500160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
027600160914     c                   when      arbtfp=arbtfa and d55tfa=arbtfa
027700160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
027800160914   x4c                   other
027900160914     c
028000160914     c* memorizzo data più recente di spunta
028100160914    5c                   if        d53err<>' '
028200160914     c                   eval      d53dfv=brvdcs
028300160914    5c                   endif
028400160914     c
028500160914    5c                   if        d53dfv<artdamf or artdamf=0
028600160914     c                   eval      artdamf=d53dfv
028700160914     c                   eval      artnapf=brvnps
028800160919
028900160919     c                   exsr      CalcIMMISS
029000160919     c                   eval      artdiaf=wdata
029100160919     c                   eval      arthamf=wora
029200160914     c
029300160914   x5c                   else
029400160919     c                   exsr      CalcIMMISS
029500160919
029600160919    6c                   if        (wdata < artdiaf) or
029700160919     c                             (wdata = artdiaf and wora<arthamf)
029800160914     c                   eval      arthamf=wora
029900160919     c                   eval      artdiaf=wdata
030000160914    6c                   endif
030100160914     c
030200160914    5c                   endif
030300160914
030400160914    4c                   endsl
030500160914    3c                   endif
030600160914     c
030700160914    2c                   endif
030800160914     c
030900160914     c     kbrv          reade     fnbrv07l
031000160914   1ac                   enddo
031100160914     c
031200160914    1c                   endif
031300120618     c*
031400160919     c* leggo per aggiornare ARTHAM/ARTDIA/ARTFLNAP
031500160919    1c                   if        artdam>0 and artdia=0
031600120618     c     kbrv          setll     fnbrv07l
031700120618     c     kbrv          reade     fnbrv07l
031800160914   1ac                   dow       not %eof(fnbrv07l)
031900160922    2c                   if        (brvnps=artnap or w_fnart='N')
032000160922     c                             and brvdcs>0
032100120618     c* controllo corrispondenza data
032200120618     c                   clear                   fnlv53ds
032300120618     C                   eval      D53NFV=brvnfv
032400120618     C                   MOVEL     brvNPG        D53NPG
032500120618     C                   MOVEL     brvFGS        D53FGS
032600120618     C                   CALL      'FNLV53R'
032700120618     C                   PARM                    fnlv53ds
032800120618    3c                   if        d53dfv=artdam
032900120618     c
033000120618    4c                   if        brvfgs<>arbtfa and brvfgs<>arblna
033100120618     c* prendo il terminal di arrivo della spunta
033200120618     c                   clear                   fnlv55ds
033300120618     c                   movel     'A'           d55tpt
033400120618     c                   movel     brvfgs        d55lin
033500120618     c                   movel     d53dfv        d55drf
033600120618     C                   CALL      'FNLV55R'
033700120618     C                   PARM                    fnlv55ds
033800120618    4c                   endif
033900120618     c
034000120618    4c                   if        brvfgs=arbtfa or brvfgs=arblna or
034100120618     c                             d55tfa=arbtfa
034200160919     c                   exsr      CalcIMMISS
034300160914     c
034400160919    5c                   if        artdia=0  or (wdata<artdia) or
034500160919     c                             (wdata=artdia and wora<artham)
034600160914     c                   eval      artham=wora
034700160919     c                   eval      artdia=wdata
034800160919     c                   eval      artflnap=brvpes
034900160922     c                   if        w_fnart='N'
035000160922     C                   eval      artnap=brvnps
035100160922     c                   endif
035200160922     c
035300160919    5c                   endif
035400120618     c
035500120618    4c                   endif
035600120618    3c                   endif
035700120618    2c                   endif
035800120618     c
035900120618     c     kbrv          reade     fnbrv07l
036000120618   1ac                   enddo
036100120618     c
036200160920     c* se arbtfa=artlna --> imposto anche le date all'arrivo finale
036300160920    2c                   if        arbtfa=artlna
036400160920     c                   eval      arthamf=artham
036500160920     c                   eval      artdiaf=artdia
036600160920    2c                   endif
036700160920    1c                   endif
036800120618     c
036900120618     c* Se ARTHSE vuoto --> leggo da spunte
037000160919    1c                   if        artdie=0 and artdse>0
037100120618     c     kbrv          setll     fnbrv07l
037200120618     c     kbrv          reade     fnbrv07l
037300160914   1ac                   dow       not %eof(fnbrv07l)
037400120618    2c                   if        brvnps=artnpe and brvdcs>0
037500120618     c* controllo corrispondenza data
037600120618     c                   clear                   fnlv53ds
037700120618     C                   eval      D53NFV=brvnfv
037800120618     C                   MOVEL     brvNPG        D53NPG
037900120618     C                   MOVEL     brvFGS        D53FGS
038000120618     C                   CALL      'FNLV53R'
038100120618     C                   PARM                    fnlv53ds
038200120618    3c                   if        d53dfv=artdse
038300120618     c
038400120618    4c                   if        brvfgs<>arbtfp and brvfgs<>arblnp
038500120618     c* prendo il terminal di arrivo della spunta
038600120618     c                   clear                   fnlv55ds
038700120618     c                   movel     'P'           d55tpt
038800120618     c                   movel     brvfgs        d55lin
038900120618     c                   movel     d53dfv        d55drf
039000120618     C                   CALL      'FNLV55R'
039100120618     C                   PARM                    fnlv55ds
039200120618    4c                   endif
039300120618     c
039400120618    4c                   if        brvfgs=arbtfp or brvfgs=arblnp or
039500120618     c                             d55tfp=arbtfp
039600160919     c                   exsr      CalcIMMISS
039700160914
039800160919    5c                   if        artdie=0 or (wdata<artdie) or
039900160919     c                             (artdie=wdata and wora<arthse)
040000160914     c                   eval      arthse=wora
040100160919     c                   eval      artdie=wdata
040200160914    5c                   endif
040300120618     c
040400120618    4c                   endif
040500120618    3c                   endif
040600120618    2c                   endif
040700120618     c
040800120618     c     kbrv          reade     fnbrv07l
040900120618   1ac                   enddo
041000120618     c
041100120618    1c                   endif
041200120618     c
041300120618     c* ORA CONSEGNA MERCE
041400120618     C                   if        artdcm>0
041500120618     c
041600120618     c                   if        arthcm=0
041700120618     c                   movel     arbhmc        arthcm
041800120618    1c                   endif
041900120618     c
042000120618     c* se negli eventi c'e' consegna parziale --> imposto per il dettaglio con data diversa
042100120618     c     kevb          setll     fnevb04l
042200120618     c     kevb          reade     fnevb04l
042300120618     c                   dow       not  %eof(fnevb04l)
042400120618     c*
042500120618     c* se la data consegna sul dett colli corrrisponde alla data evento
042600120618     c*  come orario imposto quello dell'evento
042700120618     c                   if        artdcm=evbdev
042800120618     c                   movel     evbhev        arthcm
042900120618     c                   leave
043000120618     c                   endif
043100120618     c
043200120618     c     kevb          reade     fnevb04l
043300120618     c                   enddo
043400120618     c                   endif
043500160920
043600160920     c* Per bolle export, se ci sono le spunte, cerco spunta manifest
043700160920    1c                   if        §ogntw='EEX' or §ogntw='FED' or
043800160920     c                             §ogntw='DPD'
043900160920    2c                   if        artdmf>0
044000160920     c     kbrv5         setll     fnbrv05l
044100160920     c     kbrv5         reade     fnbrv05l
044200160920    3c                   dow       not%eof(fnbrv05l)
044300160920     c* controllo corrispondenza data
044400160920     c                   clear                   fnlv53ds
044500160920     C                   eval      D53NFV=brvnfv
044600160920     C                   MOVEL     brvNPG        D53NPG
044700160920     C                   MOVEL     brvFGS        D53FGS
044800160920     C                   CALL      'FNLV53R'
044900160920     C                   PARM                    fnlv53ds
045000160920    4c                   if        d53dfv=artdmf and d53def='S'
045100160920     c                   eval      artnpdmf=brvnps
045200160920    4c                   endif
045300160920
045400160920     c     kbrv5         reade     fnbrv05l
045500160920    3c                   enddo
045600160920    2c                   endif
045700160920    1c                   endif
045800120618     c
045900120618     c                   ENDSR
046000160920     C**************************************************************************
046100160919     c     calcIMMISS    BEGSR
046200160919    6c                   if        ((brvdfs<brvdcs) or
046300160919     c                             (brvdfs=brvdcs and brvhms<brvhcs))
046400160919     c                   movel     brvhms        wora
046500160919     c                   movel     brvdfs        wdata
046600160919   x6c                   else
046700160919     c                   movel     brvhcs        wora
046800160919     c                   movel     brvdcs        wdata
046900160919    6c                   endif
047000160919     c                   ENDSR
047100150618     C**************************************************************************
047200150618     c     *inzsr        begsr
047300150618     c     Kbolla        Klist
047400150618     c                   Kfld                    i38aas
047500150618     c                   Kfld                    i38lnp
047600150618     c                   Kfld                    i38nrs
047700150618     c                   Kfld                    i38nsp
047800150618     c     Kdett         Klist
047900150618     c                   Kfld                    i38aas
048000150618     c                   Kfld                    i38lnp
048100150618     c                   Kfld                    i38nrs
048200150618     c                   Kfld                    i38nsp
048300150618     c                   Kfld                    i38nsc
048400150618     c     Kar5          Klist
048500150618     c                   Kfld                    artaas
048600150618     c                   Kfld                    artlnp
048700150618     c                   Kfld                    artnrs
048800150618     c                   Kfld                    artnsp
048900150618     c                   Kfld                    ktrd
049000150618     c     Kbrv          Klist
049100150618     c                   Kfld                    artfls
049200150618     c                   Kfld                    artlna
049300150618     c                   Kfld                    artnrs
049400150618     c                   Kfld                    artnsc
049500160920     c     Kbrv5         Klist
049600160920     c                   Kfld                    knpg
049700160920     c                   Kfld                    artfls
049800160920     c                   Kfld                    artlna
049900160920     c                   Kfld                    artnrs
050000160920     c                   Kfld                    artnsc
050100150618     c     KEVB          Klist
050200150618     c                   Kfld                    artaas
050300150618     c                   Kfld                    artlnp
050400150618     c                   Kfld                    artnrs
050500150618     c                   Kfld                    artnsp
050600150618     c                   Kfld                    kcev
050700150618     c                   ENDSR
