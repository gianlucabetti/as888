000100160523     H DECEDIT('0,') DATEDIT(*DMY.)
000200160523     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000160315     D fHEAD           s              1    inz('N')
001100160315     D chCSV           s              1    inz(',')
001200160315     D chDLM           s              1    inz('') varying
001300140708
001400160315     D*
001500160315     D DSCAPSIMPLE     DS                  qualified inz
001600160315     D  ORGFIL                        3  0
001700160315     D  CPRVER                        5  0
001800160315     D  CPRDDE                        8  0
001900160315     D  AMBITO                        5
002000160315     D  CPCNAR                        3
002100160315     D  CPCPRV                        2
002200160315     D  CPCCAP                        9
002300160315     D  CPLLOC                       37
002400160315     D  CPCLNA                        3  0
002500160315     D  CPCZNC                        2  0
002600160315     D  CAETFE                        3  0
002700140708
002800140708
002900991027
003000060804     C                   exsr      traduci
003100060804     C                   seton                                        LR
003200140708
003300991027
003400060804
003500991027     C     traduci       BEGSR
003600991027     C*
003700160315     C* Verifico i parametri di personalizzazinoe formato output
003800160315     C                   if        %subst(prmppt:1:1) <> *blanks
003900160315     C                   eval      fHEAD = %subst(prmppt:1:1)
004000160315     C                   endif
004100160315     C                   if        %subst(prmppt:2:1) <> *blanks
004200160315     C                   eval      chCSV = %subst(prmppt:2:1)
004300160315     C                   endif
004400160315     C                   if        %subst(prmppt:3:1) <> *blanks
004500160315     C                   eval      chDLM = %trim(%subst(prmppt:3:1))
004600160315     C                   endif
004700160315     C*
004800160315     C* Se richiesta intestazione colonne => scrivo subito Header
004900160315     C                   if        fHEAD = 'S'
005000160315     C                   eval      vawDTA =
005100160315     C                              chDLM+'Version'+chDLM+chCSV+
005200160315     C                              chDLM+'EffectiveDate'+chDLM+chCSV+
005300160315     C                              chDLM+'Service'+chDLM+chCSV+
005400160315     C                              chDLM+'DepartureHub'+chDLM+chCSV+
005500160315     C                              chDLM+'CountryCode'+chDLM+chCSV+
005600160315     C                              chDLM+'ProvinceCode'+chDLM+chCSV+
005700160315     C                              chDLM+'ZIPcode'+chDLM+chCSV+
005800160315     C                              chDLM+'City'+chDLM+chCSV+
005900160315     C                              chDLM+'ArrivalHub'+chDLM+chCSV+
006000160315     C                              chDLM+'ArrivalDepot'+chDLM+chCSV+
006100160315     C                              chDLM+'DeliveryZone'+chDLM
006200160315     C                   WRITE     TIVAW000
006300160315     C                   endif
006400160315     C*
006500060804     C                   read      TIVGDTMP
006600991027     C                   dow       not %eof
006700050121     C*
006800050121     C* Inizializzazione campi e variabili d out
006900060804     C                   clear                   tivaw000
007000140708     C*
007100160315     C                   eval      DSCAPSIMPLE = vgdDTA
007200160315     C                   eval      vawDTA =
007300160315     C                              %editc(DSCAPSIMPLE.CPRVER:'X')+chCSV+
007400160315     C                              %editc(DSCAPSIMPLE.CPRDDE:'X')+chCSV+
007500160315     C                              chDLM+%trim(DSCAPSIMPLE.AMBITO)+chDLM+chCSV+
007600160315     C                              %editc(DSCAPSIMPLE.ORGFIL:'X')+chCSV+
007700160315     C                              chDLM+%trim(DSCAPSIMPLE.CPCNAR)+chDLM+chCSV+
007800160315     C                              chDLM+%trim(DSCAPSIMPLE.CPCPRV)+chDLM+chCSV+
007900160315     C                              chDLM+%trim(DSCAPSIMPLE.CPCCAP)+chDLM+chCSV+
008000160315     C                              chDLM+%trim(DSCAPSIMPLE.CPLLOC)+chDLM+chCSV+
008100160315     C                              %editc(DSCAPSIMPLE.CPCLNA:'X')+chCSV+
008200160315     C                              %editc(DSCAPSIMPLE.CPCZNC:'X')+chCSV+
008300160315     C                              %editc(DSCAPSIMPLE.CAETFE:'X')
008400930409     C*
008500050121     C                   WRITE     TIVAW000
008600121023     C*
008700050909     C                   DELETE    TIVGD000
008800050909     C                   READ      TIVGDTMP
008900991027     C                   ENDdo
009000991027     C*
009100991027     C                   eval      wrkesito = '0'
009200991027     C*
009300910830     C                   ENDSR
009400060804
009500991027
009600991027      /TITLE Operazioni iniziali.
009700991027     C     *inzsr        BEGSR
009800991027     C*
009900991027     C     *ENTRY        PLIST
010000991027     C                   parm                    prmppt
010100991027     C     wrkesito      parm      wrkesito      prmesito
010200991027     C*
010300991027     C                   ENDSR
