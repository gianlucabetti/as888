000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200161104     H* UNIBO04R4 *---------------------------------------------------*
000300161104     H*         - UNISCO FNBLT FNART IN FIART00F senza letture delle spunte
000400000000     H*--------------------------------------------------------------*
000500120618     fFNart01L  if   e           k Disk
000600090225     f                                     prefix(old:3)
000700090225     fFNBLt01L  if   e           k Disk
000800051114     fFnarb01L  if   e           k Disk
000900120618     fFnBLp01L  if   e           k Disk    prefix(arb:3)
001000090226     fazorg01l  if   e           k Disk
001100051012     F
001200051012     F* FILE NUOVO
001300120618     fFIART01L  uf a e           k Disk    usropn
001400051012     F
001500051019     fQsysprt   o    f  132        Printer Oflind(*InOf) usropn
001600051012     D
001700090226     d Te              s             66    Dim(7) Ctdata Perrcd(1)
001800090225     d ERR             s             35    Dim(3) Ctdata Perrcd(1)
001900161111     d OVRP            s             80    Dim(1) Ctdata Perrcd(1)
002000060215     d
002100051012     D
002200161111     d STAFILE         s              5
002300120618     d og143         e ds
002400051012     C
002500090225     c     Kbollap       Klist
002600090225     c                   Kfld                    bltaas
002700090225     c                   Kfld                    bltlnp
002800090225     c                   Kfld                    bltnrs
002900090225     c                   Kfld                    bltnsp
003000090225     c     KbollaA       Klist
003100090225     c                   Kfld                    oldaas
003200090225     c                   Kfld                    oldlnp
003300090225     c                   Kfld                    oldnrs
003400090225     c                   Kfld                    oldnsp
003500090225     c     KcolloP       Klist
003600090225     c                   Kfld                    bltaas
003700090225     c                   Kfld                    bltlnp
003800090225     c                   Kfld                    bltnrs
003900090226     c                   Kfld                    bltnsp
004000090225     c                   Kfld                    bltnsc
004100090225     c     KcolloA       Klist
004200090225     c                   Kfld                    oldaas
004300090225     c                   Kfld                    oldlnp
004400090225     c                   Kfld                    oldnrs
004500090226     c                   Kfld                    oldnsp
004600090225     c                   Kfld                    oldnsc
004700060217      *
004800090225     c                   open      fiart01l
004900051019     c* Eseguo ovrprtf su qsysprt
005000051019     C                   MOVEA     ovrp          COMMAN           80
005100051019     C                   CALL      'QCMDEXC'                            47
005200051019     C                   PARM                    COMMAN
005300051019     C                   PARM      80            LUNG             15 5
005400051019     c
005500051019     c                   open      qsysprt
005600051019     c
005700051012     c***
005800051012     c                   except    TESTA
005900980130     C***
006000090226     C* FNBLt00F
006100980130     C***
006200090225     C     *LOVAL        SETLL     FNBLt01L
006300090225     C                   READ      FNBLt01L
006400051012     C
006500090225    1C                   DOW       NOT %EOF(FNBLt01L)
006600051012     C                   SETOFF                                       0102
006700090225     c                   clear                   fiart000
006800161104     c*
006900090225     c     kcolloP       chain     fiart01l
007000051012     c
007100051114     C* VERIFICO SE c'e' record in fnblp
007200090225     c     kbollaP       chain     fnblp01l
007300161111    2c                   if        not %found(fnblp01l)
007400051114     c* Stampo errore manca bolla
007500051012     c* non lo stampo
007600090225     c                   eval      stafile='FNBLT'
007700051012     c                   seton                                        01
007800051012     c                   except    Errore
007900051012     c                   setoff                                       01
008000051114     c                   add       1             noblp             7 0
008100161111   x2c                   else
008200161104     c*
008300161111   2ac                   if        bltftr<>' 'and (arbaas<2016 or
008400161111     c                             arbaas=2016 and arbmgs<1100)
008500161111     c                   add       1             aggiodablt        9 0
008600090225     c                   eval      artaas=bltaas
008700090225     c                   eval      artlnp=bltlnp
008800090225     c                   eval      artnrs=bltnrs
008900090225     c                   eval      artnsp=bltnsp
009000090225     c                   eval      artlna=bltlna
009100090225     c                   eval      artfls=bltfls
009200090225     c                   eval      artnsc=bltnsc
009300090225     c                   eval      artvuc=bltvuc
009400090225     c                   eval      artfvc=bltfvc
009500090225     c                   eval      artpuc=bltpuc
009600090225     c                   eval      artfpc=bltfpc
009700090225     c                   eval      artdfv=bltdfv
009800090225     c                   eval      artnpp=bltnpp
009900161111    3c                   if        bltnpp=87
010000161104     c                   eval      artx88='S'
010100161104     c                   else
010200161104     c                   clear                   artx88
010300161111    3c                   endif
010400090225     c                   eval      artnfv=bltnfv
010500090225     c                   eval      artdse=bltdse
010600090225     c                   eval      artnpe=bltnpe
010700120618     c                   eval      artdet=bltdet
010800120618     c                   eval      artdam=bltdam
010900120618    3c                   if        artlna=arbtfa  and artdam>0
011000120618     c                   eval      artdamf=bltdam
011100161111    3c                   endif
011200120618     c
011300090225     c                   eval      artdut=bltdut
011400090225     c                   eval      artdcm=bltdcm
011500120618     c                   eval      artcan=bltcan
011600161111    3c                   if        artcan<>' '
011700090225     c                   eval      artfta='P'
011800161111    3c                   endif
011900090225     c                   eval      artcca=bltfl1
012000090225     c                   eval      arttda=bltfl2
012100090225     c                   eval      artftr=bltftr
012200090225     c* Se bltftr=C significa trasmesso al cliente e quindi imposto
012300090225     c*  la data trasmissione al cliente
012400161111    3c                   if        bltftr='C'
012500090225     c                   eval      artdtc=artdcm
012600161111    3c                   endif
012700090225     c* Verifico se si tratta di bolla export
012800090225     c     bltlna        chain     azorg01l
012900161111    3c                   if        %found(azorg01l)
013000090225     c                   movel     orgde3        og143
013100090225     c                   else
013200090225     c                   clear                   og143
013300161111    3c                   endif
013400090225     c
013500090225     c* Verifico se c'e' il record in arrivo
013600090225     c     KCOLLOP       CHAIN     FNART01L
013700161111    3C                   IF        not %found(fnart01l)
013800090225     c* Se non trovo bolla in arb e la bolla � gi� stata trasmessa e
013900090225     c*  consegnata, significa che � stata pulita da fnarb
014000161111    4c                   if        arbdcm>0 and arbft1<>' '
014100090225     c                   eval      artbga='C'
014200090225     c                   eval      artabn='S'
014300161111    5c                   if        �ogntw='EEX' or �ogntw='FED' or
014400090225     c                             �ogntw='DPD'
014500090225     c                   eval      artdmf=artdam
014600161111    5c                   endif
014700161111    4c                   endif
014800161111    4c                   if        arbdcm=0 and arbft1<>' '
014900161104     c                   eval      artbga='D'
015000161104     c                   eval      artabn='S'
015100161111    4c                   endif
015200090225     c
015300161111   x3c                   else
015400090226     c                   add       1             aggiodaart        9 0
015500090225     c                   eval      artbga='S'
015600090225     c
015700161111    4c                   if        artvuc=0 and oldvuc>0
015800090225     c                   eval      artvuc=oldvuc
015900090225     c                   eval      artfvc=oldfvc
016000161111    4c                   endif
016100161111    4c                   if        artpuc=0 and oldpuc>0
016200090225     c                   eval      artpuc=oldpuc
016300090225     c                   eval      artfpc=oldfpc
016400161111    4c                   endif
016500090225    c
016600090225     c* le date le riprendo dall'arrivo
016700090225     c                   eval      artdfv=olddfv
016800090225     c                   eval      artnpp=oldnpp
016900090225     c                   eval      artnfv=oldnfv
017000090225     c                   eval      artflp=oldflp
017100090225     c                   eval      artabn=oldabn
017200090225     c                   eval      artcan=oldcan
017300161111    4c                   if        artcan<>' '  and artfta=' '
017400161104     c                   eval      artcan=oldcan
017500161104     c                   eval      artfta='A'
017600161111    4c                   endif
017700090225     c                   eval      artdet=olddet
017800090225     c                   eval      artpet=oldpet
017900090225     c                   eval      artdut=olddut
018000090225     c                   eval      artput=oldput
018100090225     c                   eval      artdam=olddam
018200161111    4c                   if        artlna=arbtfa  and artdam>0
018300120618     c                   eval      artdamf=olddam
018400161104     c                   eval      artnapf=oldnap
018500161111    4c                   endif
018600120618     c
018700090225     c                   eval      artnap=oldnap
018800090225     c                   eval      arttda=oldfl2
018900090225     c                   eval      artdab=olddab
019000161104     c                   eval      artcca=oldfl1
019100161111     c
019200161111    4c                   if        �ogntw='EEX' or �ogntw='FED' or
019300090225     c                             �ogntw='DPD'
019400090226     c                   eval      artdmf=olddtr
019500161111    4c                   endif
019600161111    3c                   endif
019700161104
019800161111    3c                   if        %found(fiart01l)
019900161111     c                   update    fiart000
020000161111     c                   add       1             giablt            9 0
020100161111     c                   else
020200090225     c                   write     fiart000
020300161111    3c                   endif
020400161111
020500161111    2c                   endif
020600161111   2ac                   endif
020700051012     C
020800090226     C                   READ      FNBLt01L
020900051012    1c                   enddo
021000051012     C
021100051012     C***
021200090225     C     *LOVAL        SETLL     FNart01L
021300090225     C                   READ      FNart01L
021400051012     C
021500090225    1C                   DOW       NOT %EOF(FNart01L)
021600051012     C                   SETOFF                                       0302
021700051012     c
021800090225     c* Se record gi� presente in fiart, non lo scrivo
021900090225     c     kcolloa       chain     fiart01l
022000060209     c
022100090226   1ac                   if        not %found(fiart01l)
022200090225     C* VERIFICO SE c'e' record in fnarb
022300090225     c     kbollaA       chain     fnarb01l
022400090226    2c                   if        not %found(fnarb01l)
022500090225     c* Stampo errore manca bolla
022600090225     c* non lo stampo
022700090225     c                   eval      stafile='FNART'
022800090225     c                   seton                                        01
022900090225     c                   except    ErroreA
023000090225     c                   setoff                                       01
023100090225     c                   add       1             noARB             7 0
023200090225   x2c                   else
023300161111     c* Aggiorno bolle solo con data sped <1100
023400161111   2ac                   if        arbaas<2016 or arbaas=2016 and
023500161111     c                             arbmgs<1100
023600090225     c
023700090226     c                   add       1             inpiudaart        9 0
023800090225     c                   eval      artbga='A'
023900090225     c                   eval      artaas=oldaas
024000090225     c                   eval      artlnp=oldlnp
024100090225     c                   eval      artnrs=oldnrs
024200090225     c                   eval      artnsp=oldnsp
024300090225     c                   eval      artlna=oldlna
024400090225     c                   eval      artfls=oldfls
024500090225     c                   eval      artnsc=oldnsc
024600090225     c                   eval      artvuc=oldvuc
024700090225     c                   eval      artfvc=oldfvc
024800090225     c                   eval      artpuc=oldpuc
024900090225     c                   eval      artfpc=oldfpc
025000090225     c                   eval      artdfv=olddfv
025100090225     c                   eval      artnpp=oldnpp
025200090225     c                   eval      artnfv=oldnfv
025300090225     c                   eval      artflp=oldflp
025400090225     c                   eval      artabn=oldabn
025500090225     c                   eval      artcan=oldcan
025600161111    3c                   if        artcan<>' '
025700090225     c                   eval      artfta='A'
025800161111    3c                   endif
025900090225     c                   eval      artdet=olddet
026000090225     c                   eval      artpet=oldpet
026100090225     c                   eval      artdut=olddut
026200090225     c                   eval      artput=oldput
026300090225     c                   eval      artdam=olddam
026400120618    3c                   if        artlna=arbtfa  and artdam>0
026500120618     c                   eval      artdamf=olddam
026600161111    3c                   endif
026700120618     c
026800090225     c                   eval      artnap=oldnap
026900090225     c                   eval      arttda=oldfl2
027000090225     c                   eval      artdab=olddab
027100090225     c                   eval      artdcm=olddcm
027200090225     c                   eval      artcca=oldfl1
027300150618     c* Verifico se si tratta di bolla export
027400150618     c     oldlna        chain     azorg01l
027500161111    3c                   if        %found(azorg01l)
027600150618     c                   movel     orgde3        og143
027700150618     c                   else
027800150618     c                   clear                   og143
027900161111    3c                   endif
028000161111    3c                   if        �ogntw='EEX' or �ogntw='FED' or
028100090225     c                             �ogntw='DPD'
028200090226     c                   eval      artdmf=olddtr
028300161111    3c                   endif
028400090225     c
028500090225     c                   write     fiart000
028600120618    2c                   endif
028700161111   2ac                   endif
028800090225   1ac                   endif
028900051012     C
029000090226     C                   READ      FNart01L
029100051012    1c                   enddo
029200051012     c***
029300090226     C     aggiodablt    ADD       inpiudaart    AGGTOT            9 0
029400051012     C
029500051012     c                   except    totali
029600120618     c
029700020502     C                   SETON                                        LR
029800051012      *  T E S T A T A
029900051012     oQsysprt   e            Testa            02
030000051012     o                       Te(1)               66
030100051012     o                       Te(2)              132
030200051114     o                       *Date              120 '  /  /    '
030300051012     o                       Page          Z    132
030400051012     O
030500051012     o          e            ERRORE      1
030600051012     o                       STAFILE
030700090225     o                       BLtaas           +   1
030800090225     o                       BLtLNp           +   1
030900090225     o                       BLtNRS           +   1
031000090225     o                       BLtNSp        2  +   1
031100090225     o                       BLtnsc           +   1
031200090225     o                       BLtlna           +   1
031300051012     o               01      err(1)             132
031400090225     o
031500090225     o          e            ERROREA     1
031600090225     o                       STAFILE
031700090225     o                       oLdaas           +   1
031800090225     o                       oLdLNp           +   1
031900090225     o                       oLdNRS           +   1
032000090225     o                       oLdNSp        2  +   1
032100090225     o                       oLdnsc           +   1
032200090225     o                       oLdlna           +   1
032300090225     o               01      err(2)             132
032400051012     o
032500051012     o          e            TOTALI      2
032600051012     o                       Te(3)               66
032700051012     o                       Te(4)              132
032800090225     o                       GIABLt        2     38
032900051114     o                       NOBLp         2     67
033000090226     o                       Aggiodablt    2    105
033100090226     o                       Aggiodaart    2    132
033200051012     O
033300051012     o          e            TOTALI      2
033400051012     o                       Te(5)               66
033500051012     o                       Te(6)              132
033600051114     o                       NOARb         2     67
033700090226     o                       inpiudaart    2    132
033800051012     o          e            TOTALI      2
033900051012     o                       Te(7)               66
034000051012     o                       AGGTOT        2     67
034100051012**         TE
034200090225                            **  UNIONE FNBLT00F FNART00F IN   F I    1
034300090225A R T O O F  **             UNIBO04R                     PAG. XXXX   2
034400090225FNBLT00F : gi� in Fiart00f xxx.xxx.xxx  Non presente FNBLPx.xxx.xx   3
034500090226x          Aggiornati da BLTxxx.xxx.xxx      con FNART xxx.xxx.xxx   4
034600090226FNART00F :                              Non presente FNARBx.xxx.xx   5
034700090226x                                            Aggiornatixxx.xxx.xxx   6
034800090226TOTALI GENERALI:                             AGGIORNATI XXX.XXX.XXX  7
034900051012**    ERR
035000051114MANCA bolla in FNBLP01L                                              1
035100051114MANCA bolla in FNARB01L
035200090225Gi� presente in FIART
035300051019**
035400161111OVRPRTF FILE(QSYSPRT) OUTQ(P046197EDP) HOLD(*YES) SAVE(*YES) USRDTA('FIART00F')
