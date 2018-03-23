000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300140326     fFNblp37L  iF   E           K DISK
000400140327     ffiar501L  uF   E           K DISK
000500140326     F                                     RENAME(Fiar5000:fiar5f)
000600140327     ffiar531c  uF   E           K DISK    extfile(war5F) usropn
000700140326     ftabel00f  IF   E           K DISK
000800140326     ffnbrv07l  IF   E           K DISK
000900140326     ffnblt01l  IF   E           K DISK
001000140326     ftntam01l  IF   E           K DISK
001100140326     fqsysprt   O    F  132        PRINTER  oflind(*INOF)
001200140326     f
001300140326     D L6              S              3  0 DIM(30)
001400140326
001500140326     d War5f           s             21    inz('GAITRAGRU /FIAR531C')
001600140326     d kpjba         e ds                  Inz
001700140326     d dAr5gen       e ds                  Inz
001800140326     d fnlv59ds      e ds                  Inz
001900140326     d trul27ds      e ds                  Inz
002000140326     d dsta01        e ds                  Inz
002100140326     D                 DS
002200140326     D  blpaas                 1      4  0
002300140326     D  blpmgs                 5      8  0
002400140326     D  blpdsp                 1      8  0
002500140326     D* DS PER TRUL06R - CARICAMENTO £X
002600140326     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002700140326     D  LIN                    1     90  0
002800140326     D                                     DIM(30)
002900140326
003000140326     iFIAR5000      50
003100140326     iFIAR5P00      51
003200980611      * ---------------------------------------------------------
003300140326     C     Ktam          KLIST
003400140326     C                   KFLD                    ilv59ksc
003500140326     C                   KFLD                    blpctr
003600140326     C                   KFLD                    olv59prg
003700041222     C     KART          KLIST
003800140326     C                   KFLD                    blpAAS
003900140326     C                   KFLD                    blpLNP
004000140326     C                   KFLD                    blpNRS
004100140326     C                   KFLD                    blpNSP
004200140326     C     Kbrv          KLIST
004300140326     C                   KFLD                    bltfls
004400140326     C                   KFLD                    bltlna
004500140326     C                   KFLD                    bltnrs
004600140326     C                   KFLD                    bltnsc
004700140326     C     KAR5          KLIST
004800140326     C                   KFLD                    blpAAS
004900140326     C                   KFLD                    blpLNP
005000140326     C                   KFLD                    blpNRS
005100140326     C                   KFLD                    blpNSP
005200140326     C                   KFLD                    TRD               3
005300140326     C                   EVAL      TRD='GEN'
005400140326     C     KArb          KLIST
005500140326     C                   KFLD                    klnp              3 0
005600140326     C                   KFLD                    kdcm              8 0
005700140326
005800140326     c                   clear                   kcut              1 0
005900140326     c                   clear                   kcod              2
006000140326     c                   clear                   kkey              8
006100140326     c                   clear                   savpes
006200140326     c
006300140326     c                   Open(e)   fiar531c
006400140326    1c                   if        not %open(fiar531C)
006500140326     c                   eval      %subst(War5f:7:5)='GRPS'
006600140326     c                   Open      fiar531C
006700140326    1c                   endif
006800140326     c                   clear                   conta             9 0
006900140326     c                   clear                   contan            9 0
007000140327     c                   clear                   contaV            9 0
007100041222     C
007200140327     c                   goto      sotto
007300140327     c
007400140326     c     kLNp          SETgt     fnblp000                           30
007500140326     c                   READ      fnblp000
007600041222     C
007700140326    1C                   DOW       NOT %EOF(FNblp37L)
007800140326     C                   EVAL      KLNp=blplnp
007900140326     c                   clear                   kdcm
008000140326     C     KARB          SETLL     FNblp37l
008100140326     C     KARB          READE     FNblp37L
008200041222     C
008300140326    2C                   DOW       NOT %EOF(FNblp37L)
008400140327     c                   exsr      Elabora
008500041222     C
008600041222     c
008700140326     C     KARB          READE     FNblp37L
008800041222    2c                   enddo
008900041222     c
009000140326     c     klnp          setgt     fnblp37l
009100140326     c                   read      fnblp37l
009200041222    1c                   enddo
009300140327     c
009400140327     c     sotto         tag
009500140327     c*************************
009600140327     c* CONSEGNATE
009700140327     c*************************
009800140327     c                   eval      kdcm=20140313
009900140327     c                   clear                   klnp
010000140327     C
010100140327     c     kLNp          SETgt     fnblp000                           30
010200140327     c                   READ      fnblp000
010300140327     C
010400140327    1C                   DOW       NOT %EOF(FNblp37L)
010500140327     C                   EVAL      KLNp=blplnp
010600140327     C     KARB          SETLL     FNblp37l
010700140327     C     Klnp          READE     FNblp37L
010800140327     C
010900140327    2C                   DOW       NOT %EOF(FNblp37L)
011000140327     C                   EXSR      ELABORA
011100140327     c
011200140327     C     Klnp          READE     FNblp37L
011300140327    2c                   enddo
011400140327     c
011500140327     c     klnp          setgt     fnblp37l
011600140327     c                   read      fnblp37l
011700140327    1c                   enddo
011800140327     c
011900140326     c                   except    totale
012000140326     c
012100140326     C                   CLEAR                   trul27ds
012200140326     C                   MOVEL     'C'           I27TLA
012300140326     C                   CALL      'TRUL27R'
012400140326     C                   PARM                    TRUL27DS
012500140326     c
012600140326     C                   CLEAR                   fnlv59ds
012700140326     C                   MOVEL     'C'           Ilv59TLA
012800140326     C                   CALL      'FNLV59R'
012900140326     C                   PARM                    fnlv59ds
013000041222     c                   seton                                        lr
013100041222     c
013200140327     c* --------------------------------------------------------------------
013300140327     c     ELABORA       BEGSR
013400140327     c                   clear                   valore
013500140327     c                   clear                   datreale          8 0
013600140327     c                   clear                   orareale          4 0
013700140327     c                   clear                   warrdt            8 0
013800140327     c                   clear                   warrhm            4 0
013900140327    5c                   if        blpctm<>'4 '  and blpctm<>'4F'
014000140327     c
014100140327     c     kar5          chain     fiar501l
014200140327    3c                   if        %found(fiar501l)
014300140327     c                   eval      dar5gen=ar5uni
014400140327     c
014500140327     c* Elaboro solo i record senza video e utente
014600140327    4c                   if        §ar5bpru=*blanks  and blpaas>=2012
014700140327     c*
014800140327     c* leggo la tariffa
014900140327     c                   clear                   trul27ds
015000140327     c                   clear                   fnlv59ds
015100140327     C* SE ESISTONO TARIFFE PER IL CLIENTE CERCO
015200140327     C                   MOVEL     'P'           ilv59tbo
015300140327
015400140327    6c                   if        blpccm>0
015500140327     C                   MOVEL     blpccm        ilv59ksc
015600140327     c                   else
015700140327     C                   MOVEL     blpksc        ilv59ksc
015800140327    6c                   endif
015900140327     c                   Eval      I27Pkg = blpPkf
016000140327     c                   eval      i27tsp = blptsp
016100140327     c                   eval      i27lna = blplna
016200140327     c                   eval      i27lnp = blplnp
016300140327     C                   eval      i27cli = ilv59ksc
016400140327     C                   eval      i27dta = blpdsp
016500140327     C                   movel     blpctr        blpctr
016600140327     c                   call      'TRUL27R'
016700140327     c                   parm                    trul27ds
016800140327     c                   eval      ilv59fie = o27fie
016900140327     c
017000140327     c                   movel     'S'           ilv59blo
017100140327     C                   MOVEL     'S'           ilv59ric
017200140327     C                   MOVEL     'S'           ilv59con
017300140327     C                   MOVEL     151           ilv59kci
017400140327     C                   MOVEL     BLPctr        ilv59ctr
017500140327     C                   MOVEL     BLPDSP        ilv59dsp
017600140327     C                   MOVEL     blpTSP        ilv59tsp
017700140327     C                   CALL      'FNLV59R'
017800140327     C                   PARM                    fnlv59ds
017900140327     C*
018000140327    6c                   if        olv59tks='S'
018100140327     c     ktam          chain     tntam01l
018200140327     c                   eval      dsta01=tamflo
018300140327     C                   MOVEL     §tabva        §ar5bva
018400140327     c                   movel     §tabva        valore            1
018500140327    6c                   endif
018600140327     c* metto data arrivo primo collo
018700140327     c     kart          setll     fnblt01l
018800140327     c     kart          reade     fnblt01l
018900140327    6c                   dow       not %eof(fnblt01l)
019000140327     c* leggo le spunte
019100140327     c                   exsr      leggispu
019200140327     c     kart          reade     fnblt01l
019300140327    6c                   enddo
019400140327
019500140327     c                   movel     warrdt        §ar5arrdt
019600140327     c                   movel     warrhm        §ar5arrhm
019700140327     c
019800140327     c                   eval      ar5uni=dar5gen
019900140327     c                   update    fiar5f
020000140327     c                   add       1             conta
020100140327     c                   if        valore<>' '
020200140327     c                   add       1             contaV            9 0
020300140327     c                   endif
020400140327     c
020500140327     c                   except    bolla
020600140327     c
020700140327     c                   setoff                                       5051
020800140327
020900140327     c     kar5          chain     fiar531c
021000140327    6c                   if        %found(fiar531c)
021100140327     c                   eval      dar5gen=ar5uni
021200140327     c                   movel     valore        §ar5bva
021300140327     c                   movel     warrdt        §ar5arrdt
021400140327     c                   movel     warrhm        §ar5arrhm
021500140327     c
021600140327     c                   eval      ar5uni=dar5gen
021700140327     c   50              update    fiar5000
021800140327     c   51              update    fiar5P00
021900140327   x6c                   else
022000140327     c                   add       1             c_noar5s          9 0
022100140327     c     kar5          chain     fiar501l
022200140327   3ac                   if        %found(fiar501l)
022300140327     c                   clear                   ar5ft2
022400140327     c                   clear                   ar5ft3
022500140327     c                   update    fiar5f
022600140327     c                   endif
022700140327     c
022800140327    6c                   endif
022900140327     c
023000140327    5c                   endif
023100140327    4c                   endif
023200140327    3c                   endif
023300140327     c                   ENDSR
023400980611      *------------------------------------------------------------
023500140326     c     Leggispu      BEGSR
023600140326     c     kbrv          setll     fnbrv07l
023700140326     c     kbrv          reade     fnbrv07l
023800140326    1c                   dow       not %eof(fnbrv07l)
023900140326
024000140327     c* Escludo pistole autogenerate (senza  data caricamento)
024100140327     c                   if        brvdcs>0
024200140327     c* se locale non ealboro spunte partenze ed entrata
024300140327     c                   if        blptfp<> blptfa or (brvnpg<>1 and
024400140327     c                             brvnpg<>5)
024500140326    2C     brvpes        IFNE      SAVPES
024600140326     C                   MOVEL     brvpes        SAVPES            3 0
024700140326     C                   EXSR      CARL6
024800140326    2C                   ENDIF
024900140326
025000140326     C     brvLNA        LOOKUP    L6                                     88
025100140326     c
025200140326    2c                   if        (*in88 or bltlna=brvpes)
025300140326     c
025400140326     c* Uso data e ora immissione se non superiore all'ora caricamento
025500140326    3c                   if        brvdfs> 20101101 and
025600140326     c                             ((brvdfs<brvdcs) or
025700140326     c                             (brvdfs=brvdcs and brvhms<brvhcs))
025800140326     c                   movel     brvdfs        datReale
025900140326     c                   movel     brvhms        oraReale
026000140326     c                   else
026100140326     c                   movel     brvdcs        datReale
026200140326     c                   movel     brvhcs        oraReale
026300140326    3c                   endif
026400140326
026500140326     c                   if         warrdt=0  or
026600140326    3c                             (warrdt>0 and warrdt>datReale) or
026700140326     c                             (warrdt=datReale and warrhm>oraReale)
026800140326     c                   z-add     datreale      warrdt
026900140326     c                   z-add     orareale      warrhm
027000140326    3c                   endif
027100140326    2c                   endif
027200140327    2c                   endif
027300140327    2c                   endif
027400140326     c*
027500140326     c     kbrv          reade     fnbrv07l
027600140326    1c                   enddo
027700140326     C                   ENDSR
027800140326     c
027900140326     C**************************************************************************
028000140326     C     CARL6         BEGSR
028100140326     C                   CLEAR                   DSUL06
028200140326     C                   MOVE      '£6'          D06COD
028300140326     C                   MOVEL     SAVPES        D06KEY
028400140326     C                   MOVEL     DSUL06        KPJBU
028500140326     C*
028600140326     C                   CALL      'TRUL06R'
028700140326     C                   PARM                    KPJBA
028800140326     C                   MOVEL     KPJBU         DSUL06
028900140326     C                   MOVEA     LIN           L6
029000140326     C                   ENDSR
029100140326      *------------------------------------------------------------
029200140326     Oqsysprt   E            bolla       1
029300140326     O                                        +   0 'bolla '
029400140326     O                                        +   0 'per ARR e V'
029500140326     O                       blpaas           +   2
029600140326     O                       blplnp           +   2
029700140326     O                       blpnrs        z  +   2
029800140326     O                       blpnsp        z  +   2
029900140326     o                                        +   2 'ARR '
030000140326     O                       warrdt           +   2 '    /  /  '
030100140326     O                       warrhm           +   2 '  :  '
030200140326     o                                        +   2 'val= '
030300140326     O                       valore           +   2
030400140326     Oqsysprt   E            totale      1
030500140326     O                                        +   0 'totale '
030600140326     O                       conta         2  +   2
030700140326     O                                        +   0 'totale valore'
030800140326     O                       contaV        2  +   2
030900140327     O                                        +   0 'senza FIAR5 SEDE'
031000140327     O                       c_noar5S      2  +   2
