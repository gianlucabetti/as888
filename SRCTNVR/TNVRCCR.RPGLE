000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200160516     FTITAS26C  IF   E           K DISK
000300030930     FTITAA30C  IF   E           K DISK    IGNORE(TITAaP00)
000400160516     FwftasccF  if a E             DISK
000500160516     Ffiapd01l  IF   E           K DISK
000600160516
000700030929     D KPJBA         E DS
000800030930     D TIBS69        E DS                  EXTNAME(TIBS69DS)
000900030930     D CNACO         E DS                  EXTNAME(CNACO00F)
001000030930     D CNIND         E DS                  EXTNAME(CNIND00F)
001100030930     D CNCLP         E DS                  EXTNAME(CNCLP00F)
001200030930     D FNCLS         E DS                  EXTNAME(FNCLS00F)
001300030930
001400030930     d §trc            s                   like(taatrc)
001500961108     C*
001600030929     C     *ENTRY        PLIST
001700030929     C                   PARM                    KPJBA
001800030930     c*
001900030930     C     KTAA          KLIST
002000030930     C                   KFLD                    TASAAS
002100030930     C                   KFLD                    TASLNP
002200030930     C                   KFLD                    TASNRS
002300030930     C                   KFLD                    TASNSP
002400030930     C                   KFLD                    §trc
002500160516     C     Kapd          KLIST
002600160516     C                   KFLD                    ktipo             1
002700160516     C                   KFLD                    taspdc
002800030930     c*
002900030929      *
003000160516     c                   eval      ktipo='A'
003100160516     C                   Z-ADD     20160101      DCM               8 0
003200980703     C**
003300160516     C     dcm           SETLL     titas26c
003400980703     C**
003500030929    1C                   DO        *hival
003600160516     C                   READ      titas26c
003700030929      *
003800160516    2c                   if        %eof
003900030929     c                   leave
004000160516    2c                   endif
004100030929      *
004200160516     c                   if        tasdcm>0 and tascca<>'7' and
004300160516     c                             tascca<>'5'
004400160516    2c                   if        taspdc=0700043 or
004500160516     c                             taspdc=0700392 or
004600160516     c                             taspdc=0700339 or
004700160516     c                             taspdc=0700210
004800160516     c                   exsr      memtas
004900160516    2c                   endif
005000160516    2c                   endif
005100030930     c*
005200030930    1c                   enddo
005300030930     C*
005400030930     C                   CLEAR                   TIBS69
005500030930     C                   MOVEL     'C'           I69TLA
005600030930     C                   CALL      'TIBS69R'
005700030930     C                   PARM                    TIBS69
005800030930     C                   PARM                    CNACO
005900030930     C                   PARM                    CNIND
006000030930     C                   PARM                    CNCLP
006100030930     C                   PARM                    FNCLS
006200000000     C                   SETON                                        LR
006300030930     C********************************************************
006400030930     C** MEMORIZZO SU TNASC00F LE BOLLE CON ASS. PER ASS. X CONTO
006500030930     C********************************************************
006600160516     C     MEMtas        BEGSR
006700160516
006800160516     C                   MOVE      TASccm        KSCCOD            4 0
006900160516
007000160516     C                   MOVEL     *BLANKS       tasrsm
007100160516     C                   MOVEL     *BLANKS       taslom
007200160516     C                   MOVEL     *BLANKS       tascam
007300160516     C                   MOVEL     *BLANKS       tasnzm
007400160516     C                   MOVEL     *BLANKS       tasprm
007500030930     C     KSCCOD        IFEQ      8888
007600030930     C                   SETOFF                                       10
007700030930     c                   clear                   titaa000
007800030930     c                   movel     'M'           §trc
007900160516     c     ktaa          chain     titaa000                           10
008000160516     C  N10              MOVEL     taarsc        tasRSm
008100160516     C  N10              MOVEL     taacap        tascam
008200160516     C  N10              MOVEL     taaloc        taslom
008300160516     C  N10              MOVEL     taanaz        tasnzm
008400160516     C  N10              MOVEL     taaprv        tasprm
008500030930     C**
008600160516     c
008700160516     c                   else
008800030930
008900030930      * CHIAMO TIBS69 PER RICERCA RAGIONE SOCIALE CLIENTE
009000030930     C                   CLEAR                   TIBS69
009100030930     C                   CLEAR                   CNACO
009200030930     C                   CLEAR                   CNIND
009300030930     C                   CLEAR                   CNCLP
009400030930     C                   CLEAR                   FNCLS
009500030930     C                   MOVEL     ' '           I69TLA
009600030930     C                   MOVEL     KNSIF         I69SIF
009700030930     C                   Z-ADD     tasksc        I69KAC
009800030930     C                   Z-ADD     tasksc        I69KIN
009900030930     C                   CALL      'TIBS69R'
010000030930     C                   PARM                    TIBS69
010100030930     C                   PARM                    CNACO
010200030930     C                   PARM                    CNIND
010300030930     C                   PARM                    CNCLP
010400030930     C                   PARM                    FNCLS
010500030930     C     O69ERR        IFEQ      *BLANK
010600160516     C                   MOVEL     ACORAG        tasrsm
010700160516     C                   MOVEL     indCAe        tascam
010800160516     C                   MOVEL     indCIT        taslom
010900160516     C                   MOVEL     indprv        tasprm
011000160516     C                   MOVEL     indsta        tasnzm
011100030930     C                   ENDIF
011200160516     C                   ENDIF
011300160516     c* codice aut
011400160516     c     kapd          chain     fiapd01l
011500160516     c                   if        %found(fiapd01l)
011600160516     c                   movel     apdrsc        taspdc_d
011700160516     c                   else
011800160516     c                   clear                   taspdc_d
011900160516     c                   endif
012000160516
012100160516     C                   WRITE     wftas0cc
012200160516     C
012300030930     C                   ENDSR
