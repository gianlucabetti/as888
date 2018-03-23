000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200150618     H* FNLR38UR *---------------------------------------------------*
000300090225     H*         - UNISCO FNBLT FNART IN FIART00F
000400140729     h*   ATTENZIONE ALLE BOLLE LEGATE!!!!!
000500140729     H*    cosa facciamo in caso di dirottamento ??
000600000000     H*--------------------------------------------------------------*
000700120618     fFNart01L  if   e           k Disk
000800090225     f                                     prefix(old:3)
000900090225     fFNBLt01L  if   e           k Disk
001000051114     fFnarb01L  if   e           k Disk
001100120618     fFnBLp01L  if   e           k Disk    prefix(arb:3)
001200090226     fazorg01l  if   e           k Disk
001300120618     ffiar501l  if   e           k Disk
001400120618     ffnbrv07l  if   e           k Disk
001500120618     ffnevb04l  if   e           k Disk
001600051012     F
001700060215     d
001800051012     D
001900120618     d ktrd            s                   like(ar5trd) inz('GEN')
002000120618     d kcev            s                   like(evbcev) inz('P  ')
002100150618     d w_fnart         s              1
002200150618     d w_fnblt         s              1
002300150618     d keybolla        s             16
002400150618     d savbolla        s                   like(keybolla)
002500120618     d dar5gen       e ds
002600120618     d og143         e ds
002700120618     d fnlv53ds      e ds
002800120618     d fnlv55ds      e ds
002900150618     d FiartDs       e ds                  extname(fiart00f)
003000150618     d fnlr38uds     e ds
003100051012     C
003200150618     c     *entry        plist
003300150618     c                   parm                    fnlr38uds
003400150618     c                   parm                    FiartDs
003500060217      *
003600051019     c
003700150618    1C     i38tla        IFNE      'C'
003800150618
003900150618     c                   clear                   fiartds
004000150618
004100150618     c* chain la testata bolla solo se diversa da quella elaborata precedentemente
004200150618     c                   eval      keybolla=%editc(i38aas:'X')
004300150618     c                             +%editc(i38lnp:'X')
004400150618     c                             +%editc(i38nrs:'X') + %editc(i38nsp:'X')
004500150618    2c                   if        keybolla<>savbolla
004600150618    3c                   if        i38tbo='A'
004700150618     c     kbolla        chain     fnarb01l
004800150618   x3c                   else
004900150618     c     kbolla        chain     fnblp01l
005000150618    3c                   endif
005100150618     c                   eval      savbolla=keybolla
005200150618    2c                   endif
005300160914
005400150618     c* Dati da dettaglio colli in partenza
005500150618     c                   clear                   w_fnblt
005600150618     c                   clear                   w_fnart
005700150618     c     kdett         chain     fnblt01l
005800150618     c                   if        not %found(fnblt01l)
005900150618     c                   eval      w_fnblt='N'
006000150618     c                   endif
006100150618     c     kdett         chain     fnart01l
006200150618     c                   if        not %found(fnart01l)
006300150618     c                   eval      w_fnart='N'
006400150618     c                   endif
006500150618     c                   if        w_fnblt=*blanks
006600150618     c                   exsr      sr_dablt
006700150618     c                   endif
006800150618     c* Dati da dettaglio colli in arrivo
006900150618     c                   if        w_fnart=*blanks
007000150618     c                   exsr      sr_daart
007100150618     c                   endif
007200150618     c* chain su spunte e fiar5 per le data e ora mancanti
007300150618     c                   exsr      altredate
007400150618
007500150618    1c                   endif
007600150618
007700150618     C     I38tla        IFNE      ' '
007800150618     c                   eval      d53tla='C'
007900150618     c                   call      'FNLV53R'
008000150618     c                   parm                    fnlv53ds
008100150618     c
008200150618     c                   eval      d55tla='C'
008300150618     c                   call      'FNLV55R'
008400150618     c                   parm                    fnlv55ds
008500150618
008600150618
008700150618     C                   SETON                                        LR
008800150618     C                   ELSE
008900150618     C                   SETON                                        RT
009000150618     C                   ENDIF
009100150618     c
009200051012     c***
009300150618     c*------------------------------------------------------------------
009400150618     c     sr_DaBlt      BEGSR
009500150618     c                   eval      artaas=bltaas
009600150618     c                   eval      artlnp=bltlnp
009700150618     c                   eval      artnrs=bltnrs
009800150618     c                   eval      artnsp=bltnsp
009900150618     c                   eval      artlna=bltlna
010000150618     c                   eval      artfls=bltfls
010100150618     c                   eval      artnsc=bltnsc
010200150618     c                   eval      artvuc=bltvuc
010300150618     c                   eval      artfvc=bltfvc
010400150618     c                   eval      artpuc=bltpuc
010500150618     c                   eval      artfpc=bltfpc
010600150618     c                   eval      artdfv=bltdfv
010700150618     c                   eval      artnpp=bltnpp
010800150618     c                   eval      artnfv=bltnfv
010900150618     c                   eval      artdse=bltdse
011000150618     c                   eval      artnpe=bltnpe
011100150618     c                   eval      artdet=bltdet
011200150618     c                   eval      artdam=bltdam
011300150618    3c                   if        artlna=arbtfa  and artdam>0
011400150618     c                   eval      artdamf=bltdam
011500150618     c                   endif
011600150618     c
011700150618     c                   eval      artdut=bltdut
011800150618     c                   eval      artdcm=bltdcm
011900150618     c                   eval      artcan=bltcan
012000150618    4c                   if        artcan<>' '
012100150618     c                   eval      artfta='P'
012200150618    4c                   endif
012300150618     c                   eval      artcca=bltfl1
012400150618     c                   eval      arttda=bltfl2
012500150618     c                   eval      artftr=bltftr
012600150618     c* Se bltftr=C significa trasmesso al cliente e quindi imposto
012700150618     c*  la data trasmissione al cliente
012800150618    4c                   if        bltftr='C'
012900150618     c                   eval      artdtc=artdcm
013000150618    4c                   endif
013100150618     c* Verifico se si tratta di bolla export
013200150618     c     bltlna        chain     azorg01l
013300150618    4c                   if        %found(azorg01l)
013400150618     c                   movel     orgde3        og143
013500150618     c                   else
013600150618     c                   clear                   og143
013700150618    4c                   endif
013800150618     c
013900150618     c* Se non c'e' il record in arrivo
014000150618    4C                   IF        w_fnart='N'
014100150618     c* Se non trovo bolla in arb e la bolla è già stata trasmessa e
014200150618     c*  consegnata, significa che è stata pulita da fnarb
014300150618    5c                   if        arbdcm>0 and arbdt2>0
014400150618     c                   eval      artbga='C'
014500150618     c                   eval      artabn='S'
014600150618    6c                   if        §ogntw='EEX' or §ogntw='FED' or
014700150618     c                             §ogntw='DPD'
014800150618     c                   eval      artdmf=artdam
014900150618    6c                   endif
015000150618    5c                   endif
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
018900150618     c                   eval      artcan=oldcan
019000150618     c                   if        artcan<>' '
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
022200160914     c                   clear                   Wora
022300120618     c*
022400160914    0c                   if        arbncl=1
022500120618     c     kar5          chain     fiar501l
022600120618    1c                   if        %found(fiar501l)
022700120618     c                   eval      dar5gen=ar5uni
022800120618    2c                   if        §AR5ARRDT>*zeros
022900160914     c* solo se spedizione di un collo
023000120618     c                   movel     §AR5ARRDT     artdamf
023100120618     c                   movel     §AR5ARRHM     arthamf
023200120618     c* se la linea finale di arrivo = al terminal --> aggiorno anche HAM
023300120618    3c                   if        artlna=arbtfa  and artdam>0
023400120618     c                   movel     §AR5ARRHM     artham
023500120618    3c                   endif
023600160914
023700120618    2c                   endif
023800120618    1c                   endif
023900160914    0c                   endif
024000160914     c*
024100160914     c* Se DAMF =0
024200160914    1c                   if        arbtfa<>artlna and artdamf=0
024300160914     c                             and artdam>0
024400160914     c     kbrv          setll     fnbrv07l
024500160914     c     kbrv          reade     fnbrv07l
024600160914   1ac                   dow       not %eof(fnbrv07l)
024700160914    2c                   if        brvdcs>0
024800160914     c* prendo il terminal di arrivo della spunta
024900160914     c                   clear                   fnlv55ds
025000160914     c
025100160914    3c                   if        brvpes<>artlna
025200160914     c                   movel     '6'           d55tpt
025300160914     c                   movel     artlna        d55lin
025400160914     c                   movel     brvdcs        d55drf
025500160914     C                   CALL      'FNLV55R'
025600160914     C                   PARM                    fnlv55ds
025700160914    3c                   endif
025800160914     c
025900160914    3c                   if        brvpes=artlna or brvpes=d55tfa
026000160914     c                   clear                   fnlv53ds
026100160914     C                   eval      D53NFV=brvnfv
026200160914     C                   MOVEL     brvNPG        D53NPG
026300160914     C                   MOVEL     brvFGS        D53FGS
026400160914     C                   CALL      'FNLV53R'
026500160914     C                   PARM                    fnlv53ds
026600160914
026700160914     c* utilizzo la spunta, per le bolle locali, solo se fatta in ambito "arrivo"
026800160914    4c                   select
026900160914     c                   when       arblnp=arblna and artlna=arblna
027000160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
027100160914     c                   when      arbtfp=arbtfa and artlna=arbtfa
027200160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
027300160914     c                   when      arbtfp=arbtfa and d55tfa=arbtfa
027400160914     c                             and (brvnpg=1 or brvnpg=5 or d53spg='P')
027500160914   x4c                   other
027600160914     c
027700160914     c* memorizzo data più recente di spunta
027800160914    5c                   if        d53err<>' '
027900160914     c                   eval      d53dfv=brvdcs
028000160914    5c                   endif
028100160914     c
028200160914    5c                   if        d53dfv<artdamf or artdamf=0
028300160914     c                   eval      artdamf=d53dfv
028400160914     c                   eval      artnapf=brvnps
028500160914    6c                   if        ((brvdfs<brvdcs) or
028600160914     c                             (brvdfs=brvdcs and brvhms<brvhcs))
028700160914     c                   movel     brvhms        arthamf
028800160914   x6c                   else
028900160914     c                   movel     brvhcs        arthamf
029000160914    6c                   endif
029100160914     c
029200160914   x5c                   else
029300160914    6c                   if        ((brvdfs<brvdcs) or
029400160914     c                             (brvdfs=brvdcs and brvhms<brvhcs))
029500160914     c                   movel     brvhms        wora
029600160914   x6c                   else
029700160914     c                   movel     brvhcs        wora
029800160914    6c                   endif
029900160914    6c                   if        wora<arthamf
030000160914     c                   eval      arthamf=wora
030100160914    6c                   endif
030200160914     c
030300160914    5c                   endif
030400160914
030500160914    4c                   endsl
030600160914    3c                   endif
030700160914     c
030800160914    2c                   endif
030900160914     c
031000160914     c     kbrv          reade     fnbrv07l
031100160914   1ac                   enddo
031200160914     c
031300160914    1c                   endif
031400120618     c*
031500120618     c* Se ARTHAM vuoto --> leggo da spunte
031600120618    1c                   if        artham=0 and artdam>0
031700160914     c                   clear                   primo             1
031800120618     c     kbrv          setll     fnbrv07l
031900120618     c     kbrv          reade     fnbrv07l
032000160914   1ac                   dow       not %eof(fnbrv07l)
032100120618    2c                   if        brvnps=artnap and brvdcs>0
032200120618     c* controllo corrispondenza data
032300120618     c                   clear                   fnlv53ds
032400120618     C                   eval      D53NFV=brvnfv
032500120618     C                   MOVEL     brvNPG        D53NPG
032600120618     C                   MOVEL     brvFGS        D53FGS
032700120618     C                   CALL      'FNLV53R'
032800120618     C                   PARM                    fnlv53ds
032900120618    3c                   if        d53dfv=artdam
033000120618     c
033100120618    4c                   if        brvfgs<>arbtfa and brvfgs<>arblna
033200120618     c* prendo il terminal di arrivo della spunta
033300120618     c                   clear                   fnlv55ds
033400120618     c                   movel     'A'           d55tpt
033500120618     c                   movel     brvfgs        d55lin
033600120618     c                   movel     d53dfv        d55drf
033700120618     C                   CALL      'FNLV55R'
033800120618     C                   PARM                    fnlv55ds
033900120618    4c                   endif
034000120618     c
034100120618    4c                   if        brvfgs=arbtfa or brvfgs=arblna or
034200120618     c                             d55tfa=arbtfa
034300120618    6c                   if        ((brvdfs<brvdcs) or
034400120618     c                             (brvdfs=brvdcs and brvhms<brvhcs))
034500160914     c                   movel     brvhms        wora              4 0
034600120618   x6c                   else
034700160914     c                   movel     brvhcs        wora
034800120618    6c                   endif
034900160914     c
035000160914     c                   if        primo=' ' or wora<artham
035100160914     c                   eval      artham=wora
035200160914     c                   eval      primo='1'
035300160914     c
035400160914     c                   if        artlna=arbtfa
035500160914     c                   eval      arthamf=artham
035600160914     c                   endif
035700160914     c                   endif
035800120618     c
035900120618    4c                   endif
036000120618    3c                   endif
036100120618    2c                   endif
036200120618     c
036300120618     c     kbrv          reade     fnbrv07l
036400120618   1ac                   enddo
036500120618     c
036600120618    1c                   endif
036700120618     c
036800120618     c* Se ARTHSE vuoto --> leggo da spunte
036900120618    1c                   if        arthse=0 and artdse>0
037000120618     c     kbrv          setll     fnbrv07l
037100120618     c     kbrv          reade     fnbrv07l
037200160914   1ac                   dow       not %eof(fnbrv07l)
037300120618    2c                   if        brvnps=artnpe and brvdcs>0
037400120618     c* controllo corrispondenza data
037500120618     c                   clear                   fnlv53ds
037600120618     C                   eval      D53NFV=brvnfv
037700120618     C                   MOVEL     brvNPG        D53NPG
037800120618     C                   MOVEL     brvFGS        D53FGS
037900120618     C                   CALL      'FNLV53R'
038000120618     C                   PARM                    fnlv53ds
038100120618    3c                   if        d53dfv=artdse
038200120618     c
038300120618    4c                   if        brvfgs<>arbtfp and brvfgs<>arblnp
038400120618     c* prendo il terminal di arrivo della spunta
038500120618     c                   clear                   fnlv55ds
038600120618     c                   movel     'P'           d55tpt
038700120618     c                   movel     brvfgs        d55lin
038800120618     c                   movel     d53dfv        d55drf
038900120618     C                   CALL      'FNLV55R'
039000120618     C                   PARM                    fnlv55ds
039100120618    4c                   endif
039200120618     c
039300120618    4c                   if        brvfgs=arbtfp or brvfgs=arblnp or
039400120618     c                             d55tfp=arbtfp
039500160914    5c                   if        ((brvdfs<brvdcs) or
039600120618     c                             (brvdfs=brvdcs and brvhms<brvhcs))
039700160914     c                   movel     brvhms        wora
039800120618     c                   else
039900160914     c                   movel     brvhcs        wora
040000160914    5c                   endif
040100160914
040200160914    5c                   if        arthse=0 or wora<arthse
040300160914     c                   eval      arthse=wora
040400160914    5c                   endif
040500120618     c
040600120618    4c                   endif
040700120618    3c                   endif
040800120618    2c                   endif
040900120618     c
041000120618     c     kbrv          reade     fnbrv07l
041100120618   1ac                   enddo
041200120618     c
041300120618    1c                   endif
041400120618     c
041500120618     c* ORA CONSEGNA MERCE
041600120618     C                   if        artdcm>0
041700120618     c
041800120618     c                   if        arthcm=0
041900120618     c                   movel     arbhmc        arthcm
042000120618    1c                   endif
042100120618     c
042200120618     c* se negli eventi c'e' consegna parziale --> imposto per il dettaglio con data diversa
042300120618     c     kevb          setll     fnevb04l
042400120618     c     kevb          reade     fnevb04l
042500120618     c                   dow       not  %eof(fnevb04l)
042600120618     c*
042700120618     c* se la data consegna sul dett colli corrrisponde alla data evento
042800120618     c*  come orario imposto quello dell'evento
042900120618     c                   if        artdcm=evbdev
043000120618     c                   movel     evbhev        arthcm
043100120618     c                   leave
043200120618     c                   endif
043300120618     c
043400120618     c     kevb          reade     fnevb04l
043500120618     c                   enddo
043600120618     c                   endif
043700120618     c
043800120618     c                   ENDSR
043900150618     C**************************************************************************
044000150618     c     *inzsr        begsr
044100150618     c     Kbolla        Klist
044200150618     c                   Kfld                    i38aas
044300150618     c                   Kfld                    i38lnp
044400150618     c                   Kfld                    i38nrs
044500150618     c                   Kfld                    i38nsp
044600150618     c     Kdett         Klist
044700150618     c                   Kfld                    i38aas
044800150618     c                   Kfld                    i38lnp
044900150618     c                   Kfld                    i38nrs
045000150618     c                   Kfld                    i38nsp
045100150618     c                   Kfld                    i38nsc
045200150618     c     Kar5          Klist
045300150618     c                   Kfld                    artaas
045400150618     c                   Kfld                    artlnp
045500150618     c                   Kfld                    artnrs
045600150618     c                   Kfld                    artnsp
045700150618     c                   Kfld                    ktrd
045800150618     c     Kbrv          Klist
045900150618     c                   Kfld                    artfls
046000150618     c                   Kfld                    artlna
046100150618     c                   Kfld                    artnrs
046200150618     c                   Kfld                    artnsc
046300150618     c     KEVB          Klist
046400150618     c                   Kfld                    artaas
046500150618     c                   Kfld                    artlnp
046600150618     c                   Kfld                    artnrs
046700150618     c                   Kfld                    artnsp
046800150618     c                   Kfld                    kcev
046900150618     c                   ENDSR
