000100141113     FWFTID01L  IF   E           k DISK
000200141113     FWFACTGR0F O    E             DISK
000300140819     D pIn_RcvVar      s            500a
000400140819     D pIn_ErrCode     s              8a
000500141113     D pIn_CrtUser     s             10a
000600140819     D Time            s               z
000700140819     D PGM0100         ds           500
000800140819     D  Pgm                    9     18a
000900140819     D  Lib                   19     28a
001000140819     D  Attr                  39     48a
001100160111     D  SrcFile               62     71a                                         Source File
001200160111     D  SrcLib                72     81a                                         Source File Library
001300160111     D  SrcMbr                82     91a                                         Source File Member
001400140819     D  UsrPrf               106    106a
001500140819     D  Descr                111    160a
001600140819     D  ActGrp               369    398a
001700160111     D SrcFileN        S             10                                         Source File
001800160111     D SrcLibN         S             10                                         Source File Library
001900160111     D SrcMbrN         S             10                                         Source File Member
002000140819     C     *ENTRY        PLIST
002100140819     C                   PARM                    pIn_RcvVar
002200141113     C                   PARM                    pIn_CrtUser
002300141113     C                   PARM                    pIn_ErrCode
002400160108     C*
002500160108     C* klist
002600160108     C*
002700160108     C* WFTID01L
002800160108     C     K03TID01      KLIST
002900160108     C                   KFLD                    TIDNome
003000160108     C                   KFLD                    TIDFile
003100160108     C                   KFLD                    TIDLib
003200140819     C*
003300140819     C                   IF        Time = *loval
003400140819     C                   EVAL      Time = %timestamp()
003500140819     C                   ENDIF
003600140819     C*
003700140819     C* valorizzo i campi del rcd
003800140819     C                   EVAL      PGM0100 = pIn_RcvVar
003900140819     C                   EVAL      ACTTime = Time
004000140819     C                   EVAL      ACTActGrp = ActGrp
004100140819     C                   EVAL      ACTPgm = Pgm
004200140819     C                   EVAL      ACTLib = Lib
004300140819     C                   EVAL      ACTAttr = Attr
004400160111     C* testo se c'è stato un errore
004500140819     C                   IF        %subst(pIn_ErrCode : 5 : 4) <> *blank
004600140819     C                   EVAL      ACTDescr = 'ERRORE NEL REPERIMENTO DATI'
004700140819     C                   ELSE
004800140819     C                   EVAL      ACTDescr = Descr
004900140819     C                   ENDIF
005000140819     C                   SELECT
005100140819     C                   WHEN      UsrPrf = 'O'
005200140819     C                   EVAL      ACTUsrPrf = '*OWNER'
005300140819     C                   OTHER
005400140819     C                   EVAL      ACTUsrPrf = '*USER'
005500140819     C                   ENDSL
005600141113     C                   EVAL      ACTCrtUsr = pIn_CrtUser
005700160111     C*
005800160111     C* reperisco i dati del sorgente da cui è nato l'oggetto
005900160111     C* se è un ILE lo reperisco dal modulo, altrimenti dal pgm stesso
006000160111     C                   IF        Attr = 'RPGLE' or
006100160111     C                             Attr = 'CLLE'
006200160111     C                   CALL      'UBACTGPMR4'
006300160111     c                   parm                    pgm
006400160111     c                   parm                    lib
006500160111     C                   PARM                    SrcFileN
006600160111     C                   PARM                    SrcLibN
006700160111     C                   PARM                    SrcMbrN
006800160108     C* controllo se il sorgente da cui è stato creato l'oggetto è stato recensito
006900160111     C                   EVAL      TIDNome = SrcMbrN
007000160111     C                   EVAL      TIDFile = SrcFileN
007100160111     C                   EVAL      TIDLib  = SrcLibN
007200160111     C*
007300160111     C                   ELSE
007400160111     C* controllo se il sorgente da cui è stato creato l'oggetto è stato recensito
007500160111     C                   EVAL      TIDNome = SrcMbr
007600160111     C                   EVAL      TIDFile = SrcFile
007700160111     C                   EVAL      TIDLib  = SrcLib
007800160111     C                   ENDIF
007900160111     C*
008000160108     C     K03TID01      CHAIN     WFTID000
008100141113     C                   IF        %found()
008200141113     C*
008300160108     C* controllo se l'ACTGRP dell'oggetto è diverso da quello del sorgente
008400141113     C                   SELECT
008500141113     C* default activation group
008600141113     C                   WHEN      ACTACTGRP = '*DFTACTGRP'
008700141113     C                   IF        %scan('DFTACTGRP(*YES)': TIDResult) = 0
008800141113     C                   EVAL      ACTActGrpS = TIDResult
008900160401     C* invece che l'attributo oggetto uso quello sorgente
009000160401     C                   EVAL      ACTAttr    = TIDAttr
009100141113     C                   WRITE     WFACTGR0
009200141113     C                   ENDIF
009300141113     C* *caller
009400141113     C                   WHEN      ACTACTGRP = '*CALLER'
009500141113     C                   IF        %scan('ACTGRP(*CALLER)': TIDResult) = 0
009600141113     C                   EVAL      ACTActGrpS = TIDResult
009700160401     C* invece che l'attributo oggetto uso quello sorgente
009800160401     C                   EVAL      ACTAttr    = TIDAttr
009900141113     C                   WRITE     WFACTGR0
010000141113     C                   ENDIF
010100141113     C* *new
010200141113     C                   WHEN      ACTACTGRP = '*NEW'
010300141113     C                   IF        %scan('ACTGRP(*NEW)': TIDResult) = 0
010400141113     C                   EVAL      ACTActGrpS = TIDResult
010500160401     C* invece che l'attributo oggetto uso quello sorgente
010600160401     C                   EVAL      ACTAttr    = TIDAttr
010700141113     C                   WRITE     WFACTGR0
010800141113     C                   ENDIF
010900160401     C* QILE
011000160401     C                   WHEN      ACTACTGRP = 'QILE'
011100160401     C* QILE è un default usato quando non viene impostato il parametro ACTGRP nelle H-spec
011200160401     C* (nel caso non si usino i teraspace)
011300160401     C* per cui l'errore non ci sarà se nel sorgente è impostato solo DFTACTGRP(*NO)
011400160401     C                   IF        %trim(TIDResult) <>
011500160401     C                             'DFTACTGRP(*NO)'                 AND
011600160401     C* oppure DFTACTGRP(*NO) ACTGRP(QILE)
011700160401     C                             %trim(TIDResult) <>
011800160401     C                             'DFTACTGRP(*NO) ACTGRP(QILE)'   AND
011900160401     C* oppure DFTACTGRP(*NO) ACTGRP('QILE')
012000160401     C                             %trim(TIDResult) <>
012100160401     C                             'DFTACTGRP(*NO) ACTGRP(''QILE'')'
012200160401     C                   EVAL      ACTActGrpS = TIDResult
012300160401     C* invece che l'attributo oggetto uso quello sorgente
012400160401     C                   EVAL      ACTAttr    = TIDAttr
012500160401     C                   WRITE     WFACTGR0
012600160401     C                   ENDIF
012700141113     C* qualsiasi altro activation group
012800141113     C                   OTHER
012900160401     C* testo che l'ACTGRP del sorgente sia scritto
013000160401     C*  ACTGRP(nome) [modo compilatore Gruppo PRO]
013100160401     C*  oppure ACTGRP('nome') [modo specifica D]
013200160401     C                   IF        %scan(
013300160401     C                              'ACTGRP('+%trim(ACTACTGRP)+')' :
013400160401     C                              TIDResult) = 0                      AND
013500160401     C                             %scan(
013600160401     C                              'ACTGRP('''+%trim(ACTACTGRP)+''')' :
013700160401     C                              TIDResult) = 0
013800160401     C                   EVAL      ACTActGrpS = TIDResult
013900160401     C* invece che l'attributo oggetto uso quello sorgente
014000160401     C                   EVAL      ACTAttr    = TIDAttr
014100160401     C                   WRITE     WFACTGR0
014200160401     C                   ENDIF
014300160401     C* questo test che controlla esserci scritto almeno DFTACTGRP(*NO) nel sorgente quando
014400160401     C* è presente un ACTGRP nell'oggetto è un po' troppo invadente (per ora lo tolgo)
014500160401     C*                  IF        %scan('DFTACTGRP(*NO)': TIDResult) = 0
014600160401     C*                  EVAL      ACTActGrpS = TIDResult
014700160401     C* invece che l'attributo oggetto uso quello sorgente
014800160401     C*                  EVAL      ACTAttr    = TIDAttr
014900160401     C*                  WRITE     WFACTGR0
015000160401     C*                  ENDIF
015100141113
015200141113     C                   ENDSL
015300141113
015400141113     C                   ENDIF
015500140819
015600140819     C                   SETON                                        RT
