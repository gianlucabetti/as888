000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200140307     FFNARB01L  IF   E           K DISK
000300140307     Ffipct02l  IF   E           K DISK
000400140307     D trulorsds     E DS                  prefix(S_)
000500140307     D trulor2ds     E DS
000600140307     d fipctcords    e ds
000700140307     d tnsd99ds      e ds
000800140307     d kpjba         e ds
000900140307     d
001000140307     D wrkoggiiso      s               d
001100140307     d s_arbdei        S                   LIKE(d98dei)
001200140307     d s_arboei        S                   LIKE(d98oei)
001300140307     d
001400140307     c     *entry        plist
001500140307     c                   parm                    aas               4
001600140307     c                   parm                    lnp               3
001700140307     c                   parm                    nrs               2
001800140307     c                   parm                    nsp               7
001900140307     c
002000140307     c                   movel     aas           kaas              4 0
002100140307     c                   movel     lnp           klnp              3 0
002200140307     c                   movel     nrs           knrs              2 0
002300140307     c                   movel     nsp           knsp              7 0
002400140307     c     kbolla        klist
002500140307     c                   kfld                    kaas
002600140307     c                   kfld                    klnp
002700140307     c                   kfld                    knrs
002800140307     c                   kfld                    knsp
002900140307     C     Kcet          KLIST
003000140307     C                   KFLD                    pctfgs
003100140307     C                   KFLD                    pctNDC
003200140307     C                   KFLD                    arbpdc
003300140307     C                   KFLD                    TTRD              3
003400140307     C                   KFLD                    arbAAS
003500140307     C                   KFLD                    arbLNP
003600140307     C                   KFLD                    arbNRS
003700140307     C                   KFLD                    arbNSP
003800140307     c
003900140307     c     kbolla        chain     fnarb01l
004000140307     c                   if        %found(fnarb01l)
004100140307     c                   clear                   tnsd99ds
004200140307     C                   Z-ADD     0             amgDCE            8 0
004300140307     C                   MOVEL     'L'           D98TLA
004400140307     C                   MOVEL     'A'           D98TBO
004500140307     C                   MOVEL     ARBAAS        D98AAS
004600140307     C                   MOVEL     ARBLNP        D98LNP
004700140307     C                   MOVEL     ARBNRS        D98NRS
004800140307     C                   MOVEL     ARBNSP        D98NSP
004900140307     C*
005000140307     C                   CALL      'TNSD99R'
005100140307     C                   PARM                    TNSD99DS
005200140307     c
005300140307     c* Mi salvo data teorica consegna
005400140307     c                   eval      s_arbdei=d98dei
005500140307     c                   eval      s_arboei=d98oei
005600140307    2c                   if        d98dce>0 and arbdce=0
005700140307     c                   eval      amgdce=d98dce
005800140307   x1C                   ELSE
005900140307     c                   eval      amgdce=arbdce
006000140307     c                   endif
006100140307     C*
006200140307     c                   clear                   trulorSds
006300140307     c                   eval       s_IOREtfp = ARBtfp
006400140307     c                   eval       s_IOREtfa = ARBtfa
006500140307     c                   eval       s_IOREdti = ARBdti
006600140307     c                   eval       s_IOREhti = ARBhti
006700140307     c                   eval       s_IOREdcr = ARBdcr
006800140307     c                   eval       s_IOREhcr = ARBhcr
006900140307     c                   eval       s_IOREtcr = ARBtcr
007000140307     c                   eval       s_IOREDEI = s_ARBdei
007100140307     c                   eval       s_IOREOEI = s_ARBoei
007200140307     c                   eval       s_IOREfil = ARBlna
007300140307     c                   eval       s_IOREcap = ARBcad
007400140307     c                   eval       s_IOREloc = ARBlod
007500140307     c                   eval       s_IOREnar = ARBnzd
007600140307     c* Data di Riferimento: data consegna se consegnata
007700140307     c                   select
007800140307     c                   when      arbdcm>0 and arbica<>' ' and arbica<>'R'
007900140307     c                             and arbicc<>' ' and arbicc<>'R'
008000140307     c                   eval       s_ioredta=arbdcm
008100140307     c* per ritorno all'incasso data del giorno
008200140307     c                   when      arbdcm>0 and arbddc>0
008300140307     C                   time                    wrkoggiiso
008400140307     C     *iso          move      wrkoggiiso    s_ioredta
008500140307     c                   other
008600140307     c* altrimenti data prevista consegna se > = oggi
008700140307     C                   time                    wrkoggiiso
008800140307     C     *iso          move      wrkoggiiso    s_ioredta
008900140307     c* se < di oggi, imposto oggi
009000140307     c                   if        amgdce>s_ioredta
009100140307     c                   eval       s_ioredta=amgdce
009200140307     c                   endif
009300140307     c                   endsl
009400140307     c
009500140307     c                   eval       s_IOREtser = 'C'
009600140307     c                   if         (ARBfbc = *blanks and
009700140307     c                               ARBddc > 0 and
009800140307     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
009900140307     c                               arbica='R' or arbicc=' ' or arbicc='R'))
010000140307     c                   eval       s_IOREcons = 'S'
010100140307     c                   eval       s_IOREDDC=arbddc
010200140307     c                   eval       s_IOREnDC=arbndc
010300140307     c* Orario presunto ritiro/consegna
010400140307     c                   move      arbndc        pctndc
010500140307     c                   move      arbifp        pctfgs
010600140307     c                   move      'COR'         ttrd
010700140307     c     kcet          setgt     fipct02l
010800140307     c     kcet          readpe    fipct02l
010900140310     c                   if        not %eof(fipct02l)
011000140307     c                   movel     pctdati       fipctcords
011100140307     c                   eval      s_ioreorp=§PCTORASTI
011200140307     c                   endif
011300140307     c
011400140307     c                   ENDIF
011500140307     c
011600140307     c                   eval       s_IOREtsp = ARBtsp
011700140307     c
011800140307     c                   call      'TRULORSR'
011900140307     c                   parm                    kpjba
012000140307     c                   parm                    trulorSds
012100140307     c                   parm                    trulor2ds
012200140307     c                   endif
012300100514
012400020502     C                   SETON                                        LR
