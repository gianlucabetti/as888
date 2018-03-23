000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200090225     H* UNIBO04R *---------------------------------------------------*
000300090225     H*         - UNISCO FNBLT FNART IN FIART00F
000301140729     h*   ATTENZIONE ALLE BOLLE LEGATE!!!!!
000302140729     H*    cosa facciamo in caso di dirottamento ??
000400000000     H*--------------------------------------------------------------*
000500120618     fFNart01L  if   e           k Disk
000600090225     f                                     prefix(old:3)
000700090225     fFNBLt01L  if   e           k Disk
000800051114     fFnarb01L  if   e           k Disk
000900120618     fFnBLp01L  if   e           k Disk    prefix(arb:3)
001000090226     fazorg01l  if   e           k Disk
001100120618     ffiar501l  if   e           k Disk
001200120618     ffnbrv07l  if   e           k Disk
001300120618     ffnevb04l  if   e           k Disk
001400051012     F
001500051012     F* FILE NUOVO
001600120618     fFIART01L  uf a e           k Disk    usropn
001700051012     F
001800051019     fQsysprt   o    f  132        Printer Oflind(*InOf) usropn
001900051012     D
002000090226     d Te              s             66    Dim(7) Ctdata Perrcd(1)
002100090225     d ERR             s             35    Dim(3) Ctdata Perrcd(1)
002200051019     d OVRP            s             78    Dim(1) Ctdata Perrcd(1)
002300060215     d
002400051012     D
002500051012     d STAFILE         s              5
002600120618     d ktrd            s                   like(ar5trd) inz('GEN')
002700120618     d kcev            s                   like(evbcev) inz('P  ')
002800120618     d dar5gen       e ds
002900120618     d og143         e ds
003000120618     d fnlv53ds      e ds
003100120618     d fnlv55ds      e ds
003200051012     C
003300090225     c     Kbollap       Klist
003400090225     c                   Kfld                    bltaas
003500090225     c                   Kfld                    bltlnp
003600090225     c                   Kfld                    bltnrs
003700090225     c                   Kfld                    bltnsp
003800090225     c     KbollaA       Klist
003900090225     c                   Kfld                    oldaas
004000090225     c                   Kfld                    oldlnp
004100090225     c                   Kfld                    oldnrs
004200090225     c                   Kfld                    oldnsp
004300090225     c     KcolloP       Klist
004400090225     c                   Kfld                    bltaas
004500090225     c                   Kfld                    bltlnp
004600090225     c                   Kfld                    bltnrs
004700090226     c                   Kfld                    bltnsp
004800090225     c                   Kfld                    bltnsc
004900090225     c     KcolloA       Klist
005000090225     c                   Kfld                    oldaas
005100090225     c                   Kfld                    oldlnp
005200090225     c                   Kfld                    oldnrs
005300090226     c                   Kfld                    oldnsp
005400090225     c                   Kfld                    oldnsc
005500120618     c     Kar5          Klist
005600120618     c                   Kfld                    artaas
005700120618     c                   Kfld                    artlnp
005800120618     c                   Kfld                    artnrs
005900120618     c                   Kfld                    artnsp
006000120618     c                   Kfld                    ktrd
006100120618     c     Kbrv          Klist
006200120618     c                   Kfld                    artfls
006300120618     c                   Kfld                    artlna
006400120618     c                   Kfld                    artnrs
006500120618     c                   Kfld                    artnsc
006600120618     c     KEVB          Klist
006700120618     c                   Kfld                    artaas
006800120618     c                   Kfld                    artlnp
006900120618     c                   Kfld                    artnrs
007000120618     c                   Kfld                    artnsp
007100120618     c                   Kfld                    kcev
007200060217      *
007300090225     c                   open      fiart01l
007400051019     c* Eseguo ovrprtf su qsysprt
007500051019     C                   MOVEA     ovrp          COMMAN           80
007600051019     C                   CALL      'QCMDEXC'                            47
007700051019     C                   PARM                    COMMAN
007800051019     C                   PARM      80            LUNG             15 5
007900051019     c
008000051019     c                   open      qsysprt
008100051019     c
008200051012     c***
008300051012     c                   except    TESTA
008400980130     C***
008500090226     C* FNBLt00F
008600980130     C***
008700090225     C     *LOVAL        SETLL     FNBLt01L
008800090225     C                   READ      FNBLt01L
008900051012     C
009000090225    1C                   DOW       NOT %EOF(FNBLt01L)
009100051012     C                   SETOFF                                       0102
009200090225     c                   clear                   fiart000
009300051012     c
009400120618     c                   if        bltaas>1968
009500060209     c* Se record già presente in fiar4
009600090225     c     kcolloP       chain     fiart01l
009700090225    2c                   if        %found(fiart01l)
009800051012     c* conto
009900051013     c                   seton                                        04
010000051013     c                   except    errore
010100051013     c                   setoff                                       04
010200090225     c                   add       1             giablt            9 0
010300051012   X2c                   else
010400051012     c
010500051114     C* VERIFICO SE c'e' record in fnblp
010600090225     c     kbollaP       chain     fnblp01l
010700051114    3c                   if        not %found(fnblp01l)
010800051114     c* Stampo errore manca bolla
010900051012     c* non lo stampo
011000090225     c                   eval      stafile='FNBLT'
011100051012     c                   seton                                        01
011200051012     c                   except    Errore
011300051012     c                   setoff                                       01
011400051114     c                   add       1             noblp             7 0
011500051114   x3c                   else
011600051012     c
011700090226     c                   add       1             aggiodablt        9 0
011800090225     c                   eval      artaas=bltaas
011900090225     c                   eval      artlnp=bltlnp
012000090225     c                   eval      artnrs=bltnrs
012100090225     c                   eval      artnsp=bltnsp
012200090225     c                   eval      artlna=bltlna
012300090225     c                   eval      artfls=bltfls
012400090225     c                   eval      artnsc=bltnsc
012500090225     c                   eval      artvuc=bltvuc
012600090225     c                   eval      artfvc=bltfvc
012700090225     c                   eval      artpuc=bltpuc
012800090225     c                   eval      artfpc=bltfpc
012900090225     c                   eval      artdfv=bltdfv
013000090225     c                   eval      artnpp=bltnpp
013100090225     c                   eval      artnfv=bltnfv
013200090225     c                   eval      artdse=bltdse
013300090225     c                   eval      artnpe=bltnpe
013400120618     c                   eval      artdet=bltdet
013500120618     c                   eval      artdam=bltdam
013600120618    3c                   if        artlna=arbtfa  and artdam>0
013700120618     c                   eval      artdamf=bltdam
013800120618     c                   endif
013900120618     c
014000090225     c                   eval      artdut=bltdut
014100090225     c                   eval      artdcm=bltdcm
014200120618     c                   eval      artcan=bltcan
014300090226    4c                   if        artcan<>' '
014400090225     c                   eval      artfta='P'
014500090226    4c                   endif
014600090225     c                   eval      artcca=bltfl1
014700090225     c                   eval      arttda=bltfl2
014800090225     c                   eval      artftr=bltftr
014900090225     c* Se bltftr=C significa trasmesso al cliente e quindi imposto
015000090225     c*  la data trasmissione al cliente
015100090225    4c                   if        bltftr='C'
015200090225     c                   eval      artdtc=artdcm
015300090225    4c                   endif
015400090225     c* Verifico se si tratta di bolla export
015500090225     c     bltlna        chain     azorg01l
015600090225    4c                   if        %found(azorg01l)
015700090225     c                   movel     orgde3        og143
015800090225     c                   else
015900090225     c                   clear                   og143
016000090225    4c                   endif
016100090225     c
016200090225     c* Verifico se c'e' il record in arrivo
016300090225     c     KCOLLOP       CHAIN     FNART01L
016400090225    4C                   IF        not %found(fnart01l)
016500090225     c* Se non trovo bolla in arb e la bolla è già stata trasmessa e
016600090225     c*  consegnata, significa che è stata pulita da fnarb
016700120618    5c                   if        arbdcm>0 and arbdt2>0
016800090225     c                   eval      artbga='C'
016900090225     c                   eval      artabn='S'
017000090226    6c                   if        §ogntw='EEX' or §ogntw='FED' or
017100090225     c                             §ogntw='DPD'
017200090225     c                   eval      artdmf=artdam
017300090226    6c                   endif
017400090226    5c                   endif
017500090225     c
017600090225   x4c                   else
017700090226     c                   add       1             aggiodaart        9 0
017800090225     c                   eval      artbga='S'
017900090225     c
018000090225    5c                   if        artvuc=0 and oldvuc>0
018100090225     c                   eval      artvuc=oldvuc
018200090225     c                   eval      artfvc=oldfvc
018300090225    5c                   endif
018400090225    5c                   if        artpuc=0 and oldpuc>0
018500090225     c                   eval      artpuc=oldpuc
018600090225     c                   eval      artfpc=oldfpc
018700090225    5c                   endif
018800090225    c
018900090225     c* le date le riprendo dall'arrivo
019000090225     c                   eval      artdfv=olddfv
019100090225     c                   eval      artnpp=oldnpp
019200090225     c                   eval      artnfv=oldnfv
019300090225     c                   eval      artflp=oldflp
019400090225     c                   eval      artabn=oldabn
019500090225     c                   eval      artcan=oldcan
019600090225     c                   if        artcan<>' '
019700090225     c                   eval      artfta='A'
019800090225     c                   endif
019900090225     c                   eval      artdet=olddet
020000090225     c                   eval      artpet=oldpet
020100090225     c                   eval      artdut=olddut
020200090225     c                   eval      artput=oldput
020300090225     c                   eval      artdam=olddam
020400120618    3c                   if        artlna=arbtfa  and artdam>0
020500120618     c                   eval      artdamf=olddam
020600120618     c                   endif
020700120618     c
020800090225     c                   eval      artnap=oldnap
020900090225     c                   eval      arttda=oldfl2
021000090225     c                   eval      artdab=olddab
021100090225     c                   eval      artdcm=olddcm
021200090225     c                   eval      artcca=oldfl1
021300090225     c                   if        §ogntw='EEX' or §ogntw='FED' or
021400090225     c                             §ogntw='DPD'
021500090226     c                   eval      artdmf=olddtr
021600090225    5c                   endif
021700090225    4c                   endif
021800120618     c* chain su spunte e fiar5 per le data e ora mancanti
021900120618     c                   exsr      altredate
022000090225     c
022100090225     c                   write     fiart000
022200090225    3c                   endif
022300090225    2c                   endif
022400120618     c                   endif
022500051012     C
022600090226     C                   READ      FNBLt01L
022700051012    1c                   enddo
022800051012     C
022900051012     C***
023000090225     C     *LOVAL        SETLL     FNart01L
023100090225     C                   READ      FNart01L
023200051012     C
023300090225    1C                   DOW       NOT %EOF(FNart01L)
023400051012     C                   SETOFF                                       0302
023500051012     c
023600090225     c* Se record già presente in fiart, non lo scrivo
023700090225     c     kcolloa       chain     fiart01l
023800060209     c
023900090226   1ac                   if        not %found(fiart01l)
024000090225     C* VERIFICO SE c'e' record in fnarb
024100090225     c     kbollaA       chain     fnarb01l
024200090226    2c                   if        not %found(fnarb01l)
024300090225     c* Stampo errore manca bolla
024400090225     c* non lo stampo
024500090225     c                   eval      stafile='FNART'
024600090225     c                   seton                                        01
024700090225     c                   except    ErroreA
024800090225     c                   setoff                                       01
024900090225     c                   add       1             noARB             7 0
025000090225   x2c                   else
025100090225     c
025200120618     c                   if        bltaas>1968
025300090226     c                   add       1             inpiudaart        9 0
025400090225     c                   eval      artbga='A'
025500090225     c                   eval      artaas=oldaas
025600090225     c                   eval      artlnp=oldlnp
025700090225     c                   eval      artnrs=oldnrs
025800090225     c                   eval      artnsp=oldnsp
025900090225     c                   eval      artlna=oldlna
026000090225     c                   eval      artfls=oldfls
026100090225     c                   eval      artnsc=oldnsc
026200090225     c                   eval      artvuc=oldvuc
026300090225     c                   eval      artfvc=oldfvc
026400090225     c                   eval      artpuc=oldpuc
026500090225     c                   eval      artfpc=oldfpc
026600090225     c                   eval      artdfv=olddfv
026700090225     c                   eval      artnpp=oldnpp
026800090225     c                   eval      artnfv=oldnfv
026900090225     c                   eval      artflp=oldflp
027000090225     c                   eval      artabn=oldabn
027100090225     c                   eval      artcan=oldcan
027200090225     c                   if        artcan<>' '
027300090225     c                   eval      artfta='A'
027400090225     c                   endif
027500090225     c                   eval      artdet=olddet
027600090225     c                   eval      artpet=oldpet
027700090225     c                   eval      artdut=olddut
027800090225     c                   eval      artput=oldput
027900090225     c                   eval      artdam=olddam
028000120618    3c                   if        artlna=arbtfa  and artdam>0
028100120618     c                   eval      artdamf=olddam
028200120618     c                   endif
028300120618     c
028400090225     c                   eval      artnap=oldnap
028500090225     c                   eval      arttda=oldfl2
028600090225     c                   eval      artdab=olddab
028700090225     c                   eval      artdcm=olddcm
028800090225     c                   eval      artcca=oldfl1
028900090225     c                   if        §ogntw='EEX' or §ogntw='FED' or
029000090225     c                             §ogntw='DPD'
029100090226     c                   eval      artdmf=olddtr
029200090225    5c                   endif
029300120618     c
029400120618     c* chain su spunte e fiar5 per le data e ora mancanti
029500120618     c                   exsr      altredate
029600090225     c
029700090225     c                   write     fiart000
029800090225    2c                   endif
029900120618    2c                   endif
030000090225   1ac                   endif
030100051012     C
030200090226     C                   READ      FNart01L
030300051012    1c                   enddo
030400051012     c***
030500090226     C     aggiodablt    ADD       inpiudaart    AGGTOT            9 0
030600051012     C
030700051012     c                   except    totali
030800051012     c***
030900120618     c                   eval      d53tla='C'
031000120618     c                   call      'FNLV53R'
031100120618     c                   parm                    fnlv53ds
031200120618     c
031300120618     c                   eval      d55tla='C'
031400120618     c                   call      'FNLV55R'
031500120618     c                   parm                    fnlv55ds
031600120618     c
031700020502     C                   SETON                                        LR
031800051012     c*------------------------------------------------------------------
031900120618     c     Altredate     BEGSR
032000120618     c*
032100120618     c     kar5          chain     fiar501l
032200120618    1c                   if        %found(fiar501l)
032300120618     c                   eval      dar5gen=ar5uni
032400120618    2c                   if        §AR5ARRDT>*zeros
032500120618     c                   movel     §AR5ARRDT     artdamf
032600120618     c                   movel     §AR5ARRHM     arthamf
032700120618     c* se la linea finale di arrivo = al terminal --> aggiorno anche HAM
032800120618    3c                   if        artlna=arbtfa  and artdam>0
032900120618     c                   movel     §AR5ARRHM     artham
033000120618    3c                   endif
033100120618    2c                   endif
033200120618    1c                   endif
033300120618     c*
033400120618     c* Se ARTHAM vuoto --> leggo da spunte
033500120618    1c                   if        artham=0 and artdam>0
033600120618     c     kbrv          setll     fnbrv07l
033700120618     c     kbrv          reade     fnbrv07l
033800120618   1ac                   dow       not %eof(fnbrv07l) and artham=0
033900120618    2c                   if        brvnps=artnap and brvdcs>0
034000120618     c* controllo corrispondenza data
034100120618     c                   clear                   fnlv53ds
034200120618     C                   eval      D53NFV=brvnfv
034300120618     C                   MOVEL     brvNPG        D53NPG
034400120618     C                   MOVEL     brvFGS        D53FGS
034500120618     C                   CALL      'FNLV53R'
034600120618     C                   PARM                    fnlv53ds
034700120618    3c                   if        d53dfv=artdam
034800120618     c
034900120618    4c                   if        brvfgs<>arbtfa and brvfgs<>arblna
035000120618     c* prendo il terminal di arrivo della spunta
035100120618     c                   clear                   fnlv55ds
035200120618     c                   movel     'A'           d55tpt
035300120618     c                   movel     brvfgs        d55lin
035400120618     c                   movel     d53dfv        d55drf
035500120618     C                   CALL      'FNLV55R'
035600120618     C                   PARM                    fnlv55ds
035700120618    4c                   endif
035800120618     c
035900120618    4c                   if        brvfgs=arbtfa or brvfgs=arblna or
036000120618     c                             d55tfa=arbtfa
036100120618    6c                   if        ((brvdfs<brvdcs) or
036200120618     c                             (brvdfs=brvdcs and brvhms<brvhcs))
036300120618     c                   movel     brvhms        artham
036400120618   x6c                   else
036500120618     c                   movel     brvhcs        artham
036600120618    6c                   endif
036700120618     c
036800120618    4c                   endif
036900120618    3c                   endif
037000120618    2c                   endif
037100120618     c
037200120618     c     kbrv          reade     fnbrv07l
037300120618   1ac                   enddo
037400120618     c
037500120618    1c                   endif
037600120618     c
037700120618     c* Se ARTHSE vuoto --> leggo da spunte
037800120618    1c                   if        arthse=0 and artdse>0
037900120618     c     kbrv          setll     fnbrv07l
038000120618     c     kbrv          reade     fnbrv07l
038100120618   1ac                   dow       not %eof(fnbrv07l) and arthse=0
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
040400120618    6c                   if        ((brvdfs<brvdcs) or
040500120618     c                             (brvdfs=brvdcs and brvhms<brvhcs))
040600120618     c                   movel     brvhms        arthse
040700120618     c                   else
040800120618     c                   movel     brvhcs        arthse
040900120618    6c                   endif
041000120618     c
041100120618    4c                   endif
041200120618    3c                   endif
041300120618    2c                   endif
041400120618     c
041500120618     c     kbrv          reade     fnbrv07l
041600120618   1ac                   enddo
041700120618     c
041800120618    1c                   endif
041900120618     c
042000120618     c* ORA CONSEGNA MERCE
042100120618     C                   if        artdcm>0
042200120618     c
042300120618     c                   if        arthcm=0
042400120618     c                   movel     arbhmc        arthcm
042500120618    1c                   endif
042600120618     c
042700120618     c* se negli eventi c'e' consegna parziale --> imposto per il dettaglio con data diversa
042800120618     c     kevb          setll     fnevb04l
042900120618     c     kevb          reade     fnevb04l
043000120618     c                   dow       not  %eof(fnevb04l)
043100120618     c*
043200120618     c* se la data consegna sul dett colli corrrisponde alla data evento
043300120618     c*  come orario imposto quello dell'evento
043400120618     c                   if        artdcm=evbdev
043500120618     c                   movel     evbhev        arthcm
043600120618     c                   leave
043700120618     c                   endif
043800120618     c
043900120618     c     kevb          reade     fnevb04l
044000120618     c                   enddo
044100120618     c                   endif
044200120618     c
044300120618     c                   ENDSR
044400051012
044500051012      *  T E S T A T A
044600051012     oQsysprt   e            Testa            02
044700051012     o                       Te(1)               66
044800051012     o                       Te(2)              132
044900051114     o                       *Date              120 '  /  /    '
045000051012     o                       Page          Z    132
045100051012     O
045200051012     o          e            ERRORE      1
045300051012     o                       STAFILE
045400090225     o                       BLtaas           +   1
045500090225     o                       BLtLNp           +   1
045600090225     o                       BLtNRS           +   1
045700090225     o                       BLtNSp        2  +   1
045800090225     o                       BLtnsc           +   1
045900090225     o                       BLtlna           +   1
046000051012     o               01      err(1)             132
046100090225     o               04      err(3)             132
046200090225     o
046300090225     o          e            ERROREA     1
046400090225     o                       STAFILE
046500090225     o                       oLdaas           +   1
046600090225     o                       oLdLNp           +   1
046700090225     o                       oLdNRS           +   1
046800090225     o                       oLdNSp        2  +   1
046900090225     o                       oLdnsc           +   1
047000090225     o                       oLdlna           +   1
047100090225     o               01      err(2)             132
047200051012     o
047300051012     o          e            TOTALI      2
047400051012     o                       Te(3)               66
047500051012     o                       Te(4)              132
047600090225     o                       GIABLt        2     38
047700051114     o                       NOBLp         2     67
047800090226     o                       Aggiodablt    2    105
047900090226     o                       Aggiodaart    2    132
048000051012     O
048100051012     o          e            TOTALI      2
048200051012     o                       Te(5)               66
048300051012     o                       Te(6)              132
048400051114     o                       NOARb         2     67
048500090226     o                       inpiudaart    2    132
048600051012     o          e            TOTALI      2
048700051012     o                       Te(7)               66
048800051012     o                       AGGTOT        2     67
048900051012**         TE
049000090225                            **  UNIONE FNBLT00F FNART00F IN   F I    1
049100090225A R T O O F  **             UNIBO04R                     PAG. XXXX   2
049200090225FNBLT00F : già in Fiart00f xxx.xxx.xxx  Non presente FNBLPx.xxx.xx   3
049300090226x          Aggiornati da BLTxxx.xxx.xxx      con FNART xxx.xxx.xxx   4
049400090226FNART00F :                              Non presente FNARBx.xxx.xx   5
049500090226x                                            Aggiornatixxx.xxx.xxx   6
049600090226TOTALI GENERALI:                             AGGIORNATI XXX.XXX.XXX  7
049700051012**    ERR
049800051114MANCA bolla in FNBLP01L                                              1
049900051114MANCA bolla in FNARB01L
050000090225Già presente in FIART
050100051019**
050200090226OVRPRTF FILE(QSYSPRT) OUTQ(LJ4200CED) HOLD(*YES) SAVE(*YES) USRDTA('FIART00F')
