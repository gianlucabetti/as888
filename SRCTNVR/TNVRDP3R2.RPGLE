000100000103     H DECEDIT('0,') DATEDIT(*DMY/)
000200000103      *                                                                     *****
000300151203     FTIVGD00F  IF   E             DISK    USROPN extfile(WTNBLA)
000400151202     Ffnarb33L  IF   E           K DISK
000500151203     Ffnbrv09L  IF   E           K DISK
000600000103      *
000700151202     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
000800000103      *----------------------------------------------------*
000900151203     d WTNBLA          s             21    inz('EDPES/TIVGD00F')
001000151202     dski              s              1    dim(150)
001100151203     D                 DS
001200151203     D  brvdfs                 1      8  0
001300151203     d  brvhms                 9     14  0
001400151203     d  brvimm                 1     14  0
001500151203     D                 DS
001600151203     D  savdfs                 1      8  0
001700151203     d  savhms                 9     14  0
001800151203     d  savimm                 1     14  0
001900151203     d  savpuc         s                   like(brvpuc)
002000151203     d  savtsu         s                   like(brvtsu)
002100151203     d  totbol         s              7  0
002200151203     d  sivdl          s              7  0
002300151203     d  novdl          s              7  0
002400151203     d  erra           s              7  0
002500000103      *----------------------------------------------------*
002600151203     c                   eval      knpg=2
002700151203     c                   open      tivgd00f
002800151202     c                   read      tivgd00f
002900151202
003000151203    1c                   dow       not %eof(tivgd00f)
003100151203    2c                   if        %subst(vgddta:1:4)='0102'
003200151202     c                             and vgdtip='$3'
003300151202     c                   clear                   iniz              4 0
003400151202     c                   clear                   fine              4 0
003500151202     c                   clear                   leng              4 0
003600151202     c                   clear                   rmna             14
003700151202     c                   clear                   rmn
003800151202     c                   movel     *all'0'       peso             10
003900151202
004000151203     c                   eval      iniz=%scan('YWG':vgddta)
004100151202     c                   eval      iniz=iniz+3
004200151203     c                   eval      fine=%scan('|':vgddta:iniz)
004300151203
004400151203     c                   eval      leng=fine-iniz
004500151202     c                   eval      %subst(peso:(11-leng))=
004600151203     c                              %subst(vgddta:iniz:leng)
004700151202     c
004800151202     c                   movel     peso          wpeso            10 2
004900151202     c
005000151202     c
005100151203     c                   eval      rmna=%subst(vgddta:32:14)
005200151202     c                   move      rmna          rmn              15 0
005300151202     c     rmn           chain     fnarb33l
005400151202     c
005500151203    3c                   if        %found(fnarb33l)
005600151203     c                   eval      totbol=totbol+1
005700151203    4c                   if        arbpkc>0
005800151203     c                   eval      sivdl=sivdl+1
005900151203     c                   else
006000151203     c                   eval      novdl=novdl+1
006100151203    4c                   endif
006200151203     c
006300151203    4c                   if        arbpkc>0   and arbpkc<>wpeso
006400151203     c                   eval      erra=erra+1
006500151203     c                   clear                   savimm
006600151203     c                   clear                   savpuc
006700151203     c                   clear                   savtsu
006800151203     c                   setoff                                       1011
006900151203     c* Verifico se la prima spunta aveva o non aveva il peso vdl
007000151203     c                   eval      knpg=2
007100151203     c     kbrv          setll     fnbrv09l
007200151203     c     kbrv          reade     fnbrv09l
007300151203    5c                   dow       not %eof(fnbrv09l)
007400151203    6c                   if        savimm=0 or brvimm<savimm
007500151203     c                   eval      savimm=brvimm
007600151203     c                   eval      savpuc=brvpuc
007700151203     c                   eval      savtsu=BRVTSU
007800151203    6c                   endif
007900151203     c     kbrv          reade     fnbrv09l
008000151203    5c                   enddo
008100151203     c* Verifico se in partenza aveva il peso VDL
008200151203     c                   eval      knpg=1
008300151203     c     kbrv          setll     fnbrv09l
008400151203     c     kbrv          reade     fnbrv09l
008500151203    5c                   dow       not %eof(fnbrv09l)
008600151203    6c                   if        brvpuc>0  and brvatr=' '
008700151203     c                   seton                                        10
008800151203    6c                   endif
008900151203    6c                   if        brvpuc>0  and brvatr<>' '
009000151203     c                   seton                                        11
009100151203    6c                   endif
009200151203     c     kbrv          reade     fnbrv09l
009300151203    5c                   enddo
009400151203
009500151203     c                   except    err
009600151203
009700151203    4c                   endif
009800151203    3c                   endif
009900151203    2c                   endif
010000151202     c
010100151202     c                   read      tivgd00f
010200151203    1c                   enddo
010300151202
010400151203     c                   except    tot
010500151202     c
010600151202     c                   seton                                        lr
010700151203     c     kbrv          klist
010800151203     c                   kfld                    knpg              1 0
010900151203     c                   kfld                    arbfls
011000151203     c                   kfld                    arblna
011100151203     c                   kfld                    arbnrs
011200151203     c                   kfld                    arbncd
011300151202     c
011400000103      *-------------------------------------------------------------------------
011500151202     Oqsysprt   E            err         1
011600151202     o                                              'bolle errate'
011700151202     o                       arbaas        z  +   1
011800151202     o                       arblnp        z  +   1
011900151202     o                       arbnrs        z  +   1
012000151202     o                       arbnsp        z  +   1
012100151203     o                       arbdam        z  +   1
012200151202     O                       arbpkc        2  +   1
012300151203     o                                        +   1 'peso VGD'
012400151202     O                       wpeso         2  +   1
012500151203     o               10                       +   1 'VDL in part'
012600151203     o               11                          86 'VDL in disg'
012700151203     o                                        +   1 'prima spunta Arrivi'
012800151203     O                       savimm           +   1
012900151203     O                       savpuc        4  +   1
013000151203     O                       savtsu           +   1
013100151203     Oqsysprt   E            tot         1
013200151203     o                                              'totali. bolle elaborate'
013300151203     O                       totbol        4  +   1
013400151203     o                                        +   1 'senza peso VDL'
013500151203     O                       novdl         4  +   1
013600151203     o                                        +   1 'con   peso VDL'
013700151203     O                       sivdl         4  +   1
013800151203     o                                        +   1 'pesoVDL non segnalato DPD'
013900151203     O                       erra          4  +   1
