000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200090225     H* UNIBO04R *---------------------------------------------------*
000300090225     H*         - UNISCO FNBLT FNART IN FIART00F
000400000000     H*--------------------------------------------------------------*
000500120618     fFNart01L  if   e           k Disk
000600090225     f                                     prefix(old:3)
000700121106     fFNBLt01L  if   e           k Disk
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
003200121106     d kpjba         e ds
003300121106     D* DS PER TRUL06R - CARICAMENTO £X
003400121106     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003500121106     D  LIN                    1     90  0
003600121106     D                                     DIM(30)
003700051012     C
003800090225     c     Kbollap       Klist
003900090225     c                   Kfld                    bltaas
004000090225     c                   Kfld                    bltlnp
004100090225     c                   Kfld                    bltnrs
004200090225     c                   Kfld                    bltnsp
004300090225     c     KbollaA       Klist
004400090225     c                   Kfld                    oldaas
004500090225     c                   Kfld                    oldlnp
004600090225     c                   Kfld                    oldnrs
004700090225     c                   Kfld                    oldnsp
004800090225     c     KcolloP       Klist
004900090225     c                   Kfld                    bltaas
005000090225     c                   Kfld                    bltlnp
005100090225     c                   Kfld                    bltnrs
005200090226     c                   Kfld                    bltnsp
005300090225     c                   Kfld                    bltnsc
005400090225     c     KcolloA       Klist
005500090225     c                   Kfld                    oldaas
005600090225     c                   Kfld                    oldlnp
005700090225     c                   Kfld                    oldnrs
005800090226     c                   Kfld                    oldnsp
005900090225     c                   Kfld                    oldnsc
006000120618     c     Kar5          Klist
006100120618     c                   Kfld                    artaas
006200120618     c                   Kfld                    artlnp
006300120618     c                   Kfld                    artnrs
006400120618     c                   Kfld                    artnsp
006500120618     c                   Kfld                    ktrd
006600120618     c     Kbrv          Klist
006700120618     c                   Kfld                    artfls
006800120618     c                   Kfld                    artlna
006900120618     c                   Kfld                    artnrs
007000120618     c                   Kfld                    artnsc
007100120618     c     KEVB          Klist
007200120618     c                   Kfld                    artaas
007300120618     c                   Kfld                    artlnp
007400120618     c                   Kfld                    artnrs
007500120618     c                   Kfld                    artnsp
007600120618     c                   Kfld                    kcev
007700121106     c     KBLT          Klist
007800121106     c                   Kfld                    kaas              4 0
007900121106     c                   Kfld                    kfil              3 0
008000060217      *
008100090225     c                   open      fiart01l
008200051019     c* Eseguo ovrprtf su qsysprt
008300051019     C                   MOVEA     ovrp          COMMAN           80
008400051019     C                   CALL      'QCMDEXC'                            47
008500051019     C                   PARM                    COMMAN
008600051019     C                   PARM      80            LUNG             15 5
008700051019     c
008800051019     c                   open      qsysprt
008900051019     c
009000051012     c***
009100051012     c                   except    TESTA
009200121106     c* Carico £1 di 001
009300121106     C                   CLEAR                   DSUL06
009400121106     C                   MOVE      '£1'          D06COD
009500121106     C                   MOVEL     001           D06KEY
009600121106     C                   MOVEL     'L'           D06TLA
009700121106     C                   MOVEL     DSUL06        KPJBU
009800121106     C*
009900121106     C                   CALL      'TRUL06R'
010000121106     C                   PARM                    KPJBA
010100121106     C                   MOVEL     KPJBU         DSUL06
010200980130     C***
010300090226     C* FNBLt00F
010400980130     C***
010500121106     c                   z-add     1             xx                3 0
010600121106     c                   dow       lin(xx)>0
010700121106     c                   eval      kfil=lin(xx)
010800121106     c                   eval      kaas=2012
010900121106     c
011000121106     C     kblt          SETLL     FNBLt01L
011100121106     C     kblt          READe     FNBLt01L
011200051012     C
011300090225    1C                   DOW       NOT %EOF(FNBLt01L)
011400051012     C                   SETOFF                                       0102
011500090225     c                   clear                   fiart000
011600051012     c
011700120618     c                   if        bltaas>1968
011800121106     c* Se record già presente in fiart
011900090225     c     kcolloP       chain     fiart01l
012000090225    2c                   if        %found(fiart01l)
012100051012     c* conto
012200090225     c                   add       1             giablt            9 0
012300051012   X2c                   else
012400051012     c
012500051114     C* VERIFICO SE c'e' record in fnblp
012600090225     c     kbollaP       chain     fnblp01l
012700051114    3c                   if        not %found(fnblp01l)
012800051114     c* Stampo errore manca bolla
012900051012     c* non lo stampo
013000090225     c                   eval      stafile='FNBLT'
013100051012     c                   seton                                        01
013200051012     c                   except    Errore
013300051012     c                   setoff                                       01
013400051114     c                   add       1             noblp             7 0
013500051114   x3c                   else
013600051012     c
013700090226     c                   add       1             aggiodablt        9 0
013800090225     c                   eval      artaas=bltaas
013900090225     c                   eval      artlnp=bltlnp
014000090225     c                   eval      artnrs=bltnrs
014100090225     c                   eval      artnsp=bltnsp
014200090225     c                   eval      artlna=bltlna
014300090225     c                   eval      artfls=bltfls
014400090225     c                   eval      artnsc=bltnsc
014500090225     c                   eval      artvuc=bltvuc
014600090225     c                   eval      artfvc=bltfvc
014700090225     c                   eval      artpuc=bltpuc
014800090225     c                   eval      artfpc=bltfpc
014900090225     c                   eval      artdfv=bltdfv
015000090225     c                   eval      artnpp=bltnpp
015100090225     c                   eval      artnfv=bltnfv
015200090225     c                   eval      artdse=bltdse
015300090225     c                   eval      artnpe=bltnpe
015400120618     c                   eval      artdet=bltdet
015500120618     c                   eval      artdam=bltdam
015600120618    3c                   if        artlna=arbtfa  and artdam>0
015700120618     c                   eval      artdamf=bltdam
015800120618     c                   endif
015900120618     c
016000090225     c                   eval      artdut=bltdut
016100090225     c                   eval      artdcm=bltdcm
016200120618     c                   eval      artcan=bltcan
016300090226    4c                   if        artcan<>' '
016400090225     c                   eval      artfta='P'
016500090226    4c                   endif
016600090225     c                   eval      artcca=bltfl1
016700090225     c                   eval      arttda=bltfl2
016800090225     c                   eval      artftr=bltftr
016900090225     c* Se bltftr=C significa trasmesso al cliente e quindi imposto
017000090225     c*  la data trasmissione al cliente
017100090225    4c                   if        bltftr='C'
017200090225     c                   eval      artdtc=artdcm
017300090225    4c                   endif
017400090225     c* Verifico se si tratta di bolla export
017500090225     c     bltlna        chain     azorg01l
017600090225    4c                   if        %found(azorg01l)
017700090225     c                   movel     orgde3        og143
017800090225     c                   else
017900090225     c                   clear                   og143
018000090225    4c                   endif
018100090225     c
018200090225     c* Verifico se c'e' il record in arrivo
018300090225     c     KCOLLOP       CHAIN     FNART01L
018400090225    4C                   IF        not %found(fnart01l)
018500090225     c* Se non trovo bolla in arb e la bolla è già stata trasmessa e
018600090225     c*  consegnata, significa che è stata pulita da fnarb
018700120618    5c                   if        arbdcm>0 and arbdt2>0
018800090225     c                   eval      artbga='C'
018900090225     c                   eval      artabn='S'
019000090226    6c                   if        §ogntw='EEX' or §ogntw='FED' or
019100090225     c                             §ogntw='DPD'
019200090225     c                   eval      artdmf=artdam
019300090226    6c                   endif
019400090226    5c                   endif
019500090225     c
019600090225   x4c                   else
019700090226     c                   add       1             aggiodaart        9 0
019800090225     c                   eval      artbga='S'
019900090225     c
020000090225    5c                   if        artvuc=0 and oldvuc>0
020100090225     c                   eval      artvuc=oldvuc
020200090225     c                   eval      artfvc=oldfvc
020300090225    5c                   endif
020400090225    5c                   if        artpuc=0 and oldpuc>0
020500090225     c                   eval      artpuc=oldpuc
020600090225     c                   eval      artfpc=oldfpc
020700090225    5c                   endif
020800090225    c
020900090225     c* le date le riprendo dall'arrivo
021000090225     c                   eval      artdfv=olddfv
021100090225     c                   eval      artnpp=oldnpp
021200090225     c                   eval      artnfv=oldnfv
021300090225     c                   eval      artflp=oldflp
021400090225     c                   eval      artabn=oldabn
021500090225     c                   eval      artcan=oldcan
021600090225     c                   if        artcan<>' '
021700090225     c                   eval      artfta='A'
021800090225     c                   endif
021900090225     c                   eval      artdet=olddet
022000090225     c                   eval      artpet=oldpet
022100090225     c                   eval      artdut=olddut
022200090225     c                   eval      artput=oldput
022300090225     c                   eval      artdam=olddam
022400120618    3c                   if        artlna=arbtfa  and artdam>0
022500120618     c                   eval      artdamf=olddam
022600120618     c                   endif
022700120618     c
022800090225     c                   eval      artnap=oldnap
022900090225     c                   eval      arttda=oldfl2
023000090225     c                   eval      artdab=olddab
023100090225     c                   eval      artdcm=olddcm
023200090225     c                   eval      artcca=oldfl1
023300090225     c                   if        §ogntw='EEX' or §ogntw='FED' or
023400090225     c                             §ogntw='DPD'
023500090226     c                   eval      artdmf=olddtr
023600090225    5c                   endif
023700090225    4c                   endif
023800120618     c* chain su spunte e fiar5 per le data e ora mancanti
023900120618     c                   exsr      altredate
024000090225     c
024100090225     c                   write     fiart000
024200090225    3c                   endif
024300090225    2c                   endif
024400120618     c                   endif
024500051012     C
024600121106     C     kblt          READe     FNBLt01L
024700051012    1c                   enddo
024800121106     c
024900121106     c                   add       1             xx
025000121106     c                   enddo
025100051012     C
025200121106     c                   goto      fine
025300051012     C***
025400090225     C     *LOVAL        SETLL     FNart01L
025500090225     C                   READ      FNart01L
025600051012     C
025700090225    1C                   DOW       NOT %EOF(FNart01L)
025800051012     C                   SETOFF                                       0302
025900051012     c
026000090225     c* Se record già presente in fiart, non lo scrivo
026100090225     c     kcolloa       chain     fiart01l
026200060209     c
026300090226   1ac                   if        not %found(fiart01l)
026400090225     C* VERIFICO SE c'e' record in fnarb
026500090225     c     kbollaA       chain     fnarb01l
026600090226    2c                   if        not %found(fnarb01l)
026700090225     c* Stampo errore manca bolla
026800090225     c* non lo stampo
026900090225     c                   eval      stafile='FNART'
027000090225     c                   seton                                        01
027100090225     c                   except    ErroreA
027200090225     c                   setoff                                       01
027300090225     c                   add       1             noARB             7 0
027400090225   x2c                   else
027500090225     c
027600120618     c                   if        bltaas>1968
027700090226     c                   add       1             inpiudaart        9 0
027800090225     c                   eval      artbga='A'
027900090225     c                   eval      artaas=oldaas
028000090225     c                   eval      artlnp=oldlnp
028100090225     c                   eval      artnrs=oldnrs
028200090225     c                   eval      artnsp=oldnsp
028300090225     c                   eval      artlna=oldlna
028400090225     c                   eval      artfls=oldfls
028500090225     c                   eval      artnsc=oldnsc
028600090225     c                   eval      artvuc=oldvuc
028700090225     c                   eval      artfvc=oldfvc
028800090225     c                   eval      artpuc=oldpuc
028900090225     c                   eval      artfpc=oldfpc
029000090225     c                   eval      artdfv=olddfv
029100090225     c                   eval      artnpp=oldnpp
029200090225     c                   eval      artnfv=oldnfv
029300090225     c                   eval      artflp=oldflp
029400090225     c                   eval      artabn=oldabn
029500090225     c                   eval      artcan=oldcan
029600090225     c                   if        artcan<>' '
029700090225     c                   eval      artfta='A'
029800090225     c                   endif
029900090225     c                   eval      artdet=olddet
030000090225     c                   eval      artpet=oldpet
030100090225     c                   eval      artdut=olddut
030200090225     c                   eval      artput=oldput
030300090225     c                   eval      artdam=olddam
030400120618    3c                   if        artlna=arbtfa  and artdam>0
030500120618     c                   eval      artdamf=olddam
030600120618     c                   endif
030700120618     c
030800090225     c                   eval      artnap=oldnap
030900090225     c                   eval      arttda=oldfl2
031000090225     c                   eval      artdab=olddab
031100090225     c                   eval      artdcm=olddcm
031200090225     c                   eval      artcca=oldfl1
031300090225     c                   if        §ogntw='EEX' or §ogntw='FED' or
031400090225     c                             §ogntw='DPD'
031500090226     c                   eval      artdmf=olddtr
031600090225    5c                   endif
031700120618     c
031800120618     c* chain su spunte e fiar5 per le data e ora mancanti
031900120618     c                   exsr      altredate
032000090225     c
032100090225     c                   write     fiart000
032200090225    2c                   endif
032300120618    2c                   endif
032400090225   1ac                   endif
032500051012     C
032600090226     C                   READ      FNart01L
032700051012    1c                   enddo
032800051012     c***
032900090226     C     aggiodablt    ADD       inpiudaart    AGGTOT            9 0
033000051012     C
033100121106     c     fine          tag
033200121106     c
033300051012     c                   except    totali
033400051012     c***
033500120618     c                   eval      d53tla='C'
033600120618     c                   call      'FNLV53R'
033700120618     c                   parm                    fnlv53ds
033800120618     c
033900120618     c                   eval      d55tla='C'
034000120618     c                   call      'FNLV55R'
034100120618     c                   parm                    fnlv55ds
034200120618     c
034300020502     C                   SETON                                        LR
034400051012     c*------------------------------------------------------------------
034500120618     c     Altredate     BEGSR
034600120618     c*
034700120618     c     kar5          chain     fiar501l
034800120618    1c                   if        %found(fiar501l)
034900120618     c                   eval      dar5gen=ar5uni
035000120618    2c                   if        §AR5ARRDT>*zeros
035100120618     c                   movel     §AR5ARRDT     artdamf
035200120618     c                   movel     §AR5ARRHM     arthamf
035300120618     c* se la linea finale di arrivo = al terminal --> aggiorno anche HAM
035400120618    3c                   if        artlna=arbtfa  and artdam>0
035500120618     c                   movel     §AR5ARRHM     artham
035600120618    3c                   endif
035700120618    2c                   endif
035800120618    1c                   endif
035900120618     c*
036000120618     c* Se ARTHAM vuoto --> leggo da spunte
036100120618    1c                   if        artham=0 and artdam>0
036200120618     c     kbrv          setll     fnbrv07l
036300120618     c     kbrv          reade     fnbrv07l
036400120618   1ac                   dow       not %eof(fnbrv07l) and artham=0
036500120618    2c                   if        brvnps=artnap and brvdcs>0
036600120618     c* controllo corrispondenza data
036700120618     c                   clear                   fnlv53ds
036800120618     C                   eval      D53NFV=brvnfv
036900120618     C                   MOVEL     brvNPG        D53NPG
037000120618     C                   MOVEL     brvFGS        D53FGS
037100120618     C                   CALL      'FNLV53R'
037200120618     C                   PARM                    fnlv53ds
037300120618    3c                   if        d53dfv=artdam
037400120618     c
037500120618    4c                   if        brvfgs<>arbtfa and brvfgs<>arblna
037600120618     c* prendo il terminal di arrivo della spunta
037700120618     c                   clear                   fnlv55ds
037800120618     c                   movel     'A'           d55tpt
037900120618     c                   movel     brvfgs        d55lin
038000120618     c                   movel     d53dfv        d55drf
038100120618     C                   CALL      'FNLV55R'
038200120618     C                   PARM                    fnlv55ds
038300120618    4c                   endif
038400120618     c
038500120618    4c                   if        brvfgs=arbtfa or brvfgs=arblna or
038600120618     c                             d55tfa=arbtfa
038700120618    6c                   if        ((brvdfs<brvdcs) or
038800120618     c                             (brvdfs=brvdcs and brvhms<brvhcs))
038900120618     c                   movel     brvhms        artham
039000120618   x6c                   else
039100120618     c                   movel     brvhcs        artham
039200120618    6c                   endif
039300120618     c
039400120618    4c                   endif
039500120618    3c                   endif
039600120618    2c                   endif
039700120618     c
039800120618     c     kbrv          reade     fnbrv07l
039900120618   1ac                   enddo
040000120618     c
040100120618    1c                   endif
040200120618     c
040300120618     c* Se ARTHSE vuoto --> leggo da spunte
040400120618    1c                   if        arthse=0 and artdse>0
040500120618     c     kbrv          setll     fnbrv07l
040600120618     c     kbrv          reade     fnbrv07l
040700120618   1ac                   dow       not %eof(fnbrv07l) and arthse=0
040800120618    2c                   if        brvnps=artnpe and brvdcs>0
040900120618     c* controllo corrispondenza data
041000120618     c                   clear                   fnlv53ds
041100120618     C                   eval      D53NFV=brvnfv
041200120618     C                   MOVEL     brvNPG        D53NPG
041300120618     C                   MOVEL     brvFGS        D53FGS
041400120618     C                   CALL      'FNLV53R'
041500120618     C                   PARM                    fnlv53ds
041600120618    3c                   if        d53dfv=artdse
041700120618     c
041800120618    4c                   if        brvfgs<>arbtfp and brvfgs<>arblnp
041900120618     c* prendo il terminal di arrivo della spunta
042000120618     c                   clear                   fnlv55ds
042100120618     c                   movel     'P'           d55tpt
042200120618     c                   movel     brvfgs        d55lin
042300120618     c                   movel     d53dfv        d55drf
042400120618     C                   CALL      'FNLV55R'
042500120618     C                   PARM                    fnlv55ds
042600120618    4c                   endif
042700120618     c
042800120618    4c                   if        brvfgs=arbtfp or brvfgs=arblnp or
042900120618     c                             d55tfp=arbtfp
043000120618    6c                   if        ((brvdfs<brvdcs) or
043100120618     c                             (brvdfs=brvdcs and brvhms<brvhcs))
043200120618     c                   movel     brvhms        arthse
043300120618     c                   else
043400120618     c                   movel     brvhcs        arthse
043500120618    6c                   endif
043600120618     c
043700120618    4c                   endif
043800120618    3c                   endif
043900120618    2c                   endif
044000120618     c
044100120618     c     kbrv          reade     fnbrv07l
044200120618   1ac                   enddo
044300120618     c
044400120618    1c                   endif
044500120618     c
044600120618     c* ORA CONSEGNA MERCE
044700120618     C                   if        artdcm>0
044800120618     c
044900120618     c                   if        arthcm=0
045000120618     c                   movel     arbhmc        arthcm
045100120618    1c                   endif
045200120618     c
045300120618     c* se negli eventi c'e' consegna parziale --> imposto per il dettaglio con data diversa
045400120618     c     kevb          setll     fnevb04l
045500120618     c     kevb          reade     fnevb04l
045600120618     c                   dow       not  %eof(fnevb04l)
045700120618     c*
045800120618     c* se la data consegna sul dett colli corrrisponde alla data evento
045900120618     c*  come orario imposto quello dell'evento
046000120618     c                   if        artdcm=evbdev
046100120618     c                   movel     evbhev        arthcm
046200120618     c                   leave
046300120618     c                   endif
046400120618     c
046500120618     c     kevb          reade     fnevb04l
046600120618     c                   enddo
046700120618     c                   endif
046800120618     c
046900120618     c                   ENDSR
047000051012
047100051012      *  T E S T A T A
047200051012     oQsysprt   e            Testa            02
047300051012     o                       Te(1)               66
047400051012     o                       Te(2)              132
047500051114     o                       *Date              120 '  /  /    '
047600051012     o                       Page          Z    132
047700051012     O
047800051012     o          e            ERRORE      1
047900051012     o                       STAFILE
048000090225     o                       BLtaas           +   1
048100090225     o                       BLtLNp           +   1
048200090225     o                       BLtNRS           +   1
048300090225     o                       BLtNSp        2  +   1
048400090225     o                       BLtnsc           +   1
048500090225     o                       BLtlna           +   1
048600051012     o               01      err(1)             132
048700121106     o*****          04      err(3)             132
048800090225     o
048900090225     o          e            ERROREA     1
049000090225     o                       STAFILE
049100090225     o                       oLdaas           +   1
049200090225     o                       oLdLNp           +   1
049300090225     o                       oLdNRS           +   1
049400090225     o                       oLdNSp        2  +   1
049500090225     o                       oLdnsc           +   1
049600090225     o                       oLdlna           +   1
049700090225     o               01      err(2)             132
049800051012     o
049900051012     o          e            TOTALI      2
050000051012     o                       Te(3)               66
050100051012     o                       Te(4)              132
050200090225     o                       GIABLt        2     38
050300051114     o                       NOBLp         2     67
050400090226     o                       Aggiodablt    2    105
050500090226     o                       Aggiodaart    2    132
050600051012     O
050700051012     o          e            TOTALI      2
050800051012     o                       Te(5)               66
050900051012     o                       Te(6)              132
051000051114     o                       NOARb         2     67
051100090226     o                       inpiudaart    2    132
051200051012     o          e            TOTALI      2
051300051012     o                       Te(7)               66
051400051012     o                       AGGTOT        2     67
051500051012**         TE
051600090225                            **  UNIONE FNBLT00F FNART00F IN   F I    1
051700090225A R T O O F  **             UNIBO04R                     PAG. XXXX   2
051800090225FNBLT00F : già in Fiart00f xxx.xxx.xxx  Non presente FNBLPx.xxx.xx   3
051900090226x          Aggiornati da BLTxxx.xxx.xxx      con FNART xxx.xxx.xxx   4
052000090226FNART00F :                              Non presente FNARBx.xxx.xx   5
052100090226x                                            Aggiornatixxx.xxx.xxx   6
052200090226TOTALI GENERALI:                             AGGIORNATI XXX.XXX.XXX  7
052300051012**    ERR
052400051114MANCA bolla in FNBLP01L                                              1
052500051114MANCA bolla in FNARB01L
052600090225Già presente in FIART
052700051019**
052800090226OVRPRTF FILE(QSYSPRT) OUTQ(LJ4200CED) HOLD(*YES) SAVE(*YES) USRDTA('FIART00F')
