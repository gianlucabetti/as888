000100161130     H DECEDIT('0,') DATEDIT(*ymd)
000200161130     H OPTION( *SrcStmt : *NoDebugIO)
000300161130     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000400161128
000500161128     Ftntbe01l  IF   E           K DISK
000600161128
000700161128     D*******
000800161128     D* COSTANTI CODICI DI STATO
000900161128     D*******
001000161128     D OK              C                   CONST(0)                             * OK
001100161128     D ErrGeneric      C                   CONST(-1)                            * errore generico
001200161128     D ErrTabelle      C                   CONST(-2)                            * tabelle mancanti
001300161128     D ErrJDOCoff      C                   CONST(-3)                            * JDOC non risponde
001400161128     D ErrPODnot       C                   CONST(-4)                            * POD non trovata
001500161128
001600161128     D*******
001700161128     D* PASSAGGIO PARAMETRI
001800161128     D*******
001900161128     D iSPED           S             14A
002000161128     D oURI_POD        S            128A
002100161128     D oStato          S              5I 0
002200161128
002300161128     D*******
002400161128     D* VARIABILI DI WRK
002500161128     D*******
002600161130     D $PathTmpImg     S            640A   INZ
002700161128     D $JDOC_url       S           1024A   INZ
002800161128     D $Response       S           1024A   INZ
002900161130     D $ServerName     S            255A   INZ
003000161130     D $PortNumber     S             10I 0 INZ
003100161128
003200161128
003300161128     C*
003400161130     C* Inizializzazione parametri di output e variabili di wrk
003500110923     C                   eval      *inlr = *on
003600161130     C                   eval      oStato = ErrGeneric
003700161128     C*
003800161128     C* Reperisco i dati tabellari
003900161128     C                   eval      tbeCOD = 'IMS'
004000161128     C                   eval      tbeKE1 = '*VAS'
004100161128     C                   eval      tbeKE2 = '*IMAGEDIR'
004200161128     C     KEYtbe01_P    chain     tntbe01l
004300161128     C                   if        not %found(tntbe01l)
004400161128     C                   dump(a)
004500161128     C                   eval      oStato = ErrTabelle
004600161128     C                   return
004700161128     C                   endif
004800161130     C                   eval      $PathTmpImg = tbeUNI
004900161128     C*
005000161128     C                   eval      tbeKE1 = '*PROVIDER'
005100161128     C                   eval      tbeKE2 = '*CURRENT'
005200161128     C     KEYtbe01_P    chain     tntbe01l
005300161128     C                   if        not %found(tntbe01l)
005400161128     C                   dump(a)
005500161128     C                   eval      oStato = ErrTabelle
005600161128     C                   return
005700161128     C                   endif
005800161128     C*
005900161128     C                   eval      tbeKE1 = tbeUNI
006000161128     C                   eval      tbeKE2 = '*URLPATH'
006100161128     C     KEYtbe01_P    chain     tntbe01l
006200161128     C                   if        not %found(tntbe01l)
006300161128     C                   dump(a)
006400161128     C                   eval      oStato = ErrTabelle
006500161128     C                   return
006600110923     C                   endif
006700161128     C                   eval      $JDOC_url = tbeUNI
006800161130     C*
006900161130     C                   eval      tbeKE2 = '*URLDOMAINNAME'
007000161130     C     KEYtbe01_P    chain     tntbe01l
007100161130     C                   if        not %found(tntbe01l)
007200161130     C                   dump(a)
007300161130     C                   eval      oStato = ErrTabelle
007400161130     C                   return
007500161130     C                   endif
007600161130     C                   eval      $ServerName = tbeUni
007700161130     C*
007800161130     C                   eval      tbeKE2 = '*URLPORTNUMBER'
007900161130     C     KEYtbe01_P    chain     tntbe01l
008000161130     C                   if        not %found(tntbe01l)
008100161130     C                   dump(a)
008200161130     C                   eval      oStato = ErrTabelle
008300161130     C                   return
008400161130     C                   endif
008500161130     C                   eval      $PortNumber = %int(tbeUni)
008600161128     C*
008700161128     C                   eval      tbeKE2 = '*URLPRM_IDBOLLA'
008800161128     C     KEYtbe01_P    chain     tntbe01l
008900161128     C                   if        not %found(tntbe01l)
009000161128     C                   dump(a)
009100161128     C                   eval      oStato = ErrTabelle
009200161128     C                   return
009300110923     C                   endif
009400161128     C*
009500161128     C* Compongo l'URL completo finale
009600161128     C                   eval      $JDOC_url = %trimr($JDOC_url) +
009700161128     C                             %trimr(tbeUNI) + iSPED
009800161130     C*
009900161130     C***                call      'TIS182R'
010000161130     C***                parm                    $JDOC_url
010100161130     C***                parm                    $Response
010200161130     C*
010300161130     C                   call      'UBJDOC03R'
010400161130     C                   parm                    $JDOC_url
010500161130     C                   parm                    $ServerName
010600161130     C                   parm                    $PortNumber
010700161130     C                   parm                    $Response
010800161128     C*
010900040204     C                   select
011000161130     C                   when      %subst($Response:1:14)='ERRORE_CONNECT'
011100161128     C                   eval      oStato = ErrJDOCoff
011200161130     C                   when      %subst($Response:1:6)='ERRORE'
011300161128     C                   eval      oStato = ErrPODnot
011400040204     C                   other
011500161128     C*
011600161128     C                   eval      oStato = OK
011700161130     C                   eval      oURI_POD = %trimr($PathTmpImg) + '/' +
011800161130     C                             %trim($Response)
011900040204     C                   endsl
012000161128     C*
012100040204     C                   return
012200161128
012300161128
012400161128
012500161128     C     *inzsr        begsr
012600161128     C*
012700161128     C     *entry        plist
012800161128     C                   parm                    iSPED
012900161128     C                   parm                    oURI_POD
013000161128     C                   parm                    oStato
013100161128     C*
013200161128     C     KEYtbe01_P    klist
013300161128     C                   kfld                    tbeCOD
013400161128     C                   kfld                    tbeKE1
013500161128     C                   kfld                    tbeKE2
013600161128     C*
013700040204     C                   endsr
013800161128
