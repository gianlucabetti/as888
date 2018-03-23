000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400170124     Ftivgdtmp  if   E             DISK
000500170124     FTIVAWWWT  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000170124     D wVAWDTA         s           2048A   inz varying
001100170124     D tnvasDS       e ds                  extname(tnvas00t)
001200170124     D tnvasAsciDS   e ds                  extname(tnvasWWT) prefix(A_)
001300170124     D tnvasEbcdDS   e ds                  extname(tnvas1WT) prefix(E_)
001400170124     D l_VS_A          s              4  0 inz(%size(tnvasAsciDS))
001500170124     D l_VS_E          s              4  0 inz(%size(tnvasEbcdDS))
001600170124     D iASCI           s               N   inz
001700170124     D iEBCD           s               N   inz
001800170124     D pSEP            s              1A   inz
001900991027
002000170124
002100170124
002200170124     C*
002300170124     C* Avvio il monitoring del intero flusso
002400170124     C                   monitor
002500170124     C*
002600170124     C* Effettuo traduzione
002700991027     C                   EXSR      traduci
002800170124     C*
002900170124     C* Gestisco eventuale errore
003000170124     C                   on-error
003100170124     C*
003200170124     C* Imposto esito transazione a errore
003300170124     C                   EVAL      wrkesito = '2'
003400170124     C*
003500170124     C* Se errore => emetto operazione d ROLLBACK
003600170124     C                   ROLBK
003700170124     C*
003800170124     C* Arresto il monitoring
003900170124     C                   endmon
004000170124     C*
004100921023     C                   SETON                                        LR
004200170124
004300170124
004400991027
004500991027     C     traduci       BEGSR
004600170124     C*
004700170124     C* Inizializzazione indicatori di procedure per parametri input
004800170124     C                   eval      iASCI = *off
004900170124     C                   eval      iEBCD = *off
005000170124     C*
005100170124     C* Se richiesto ritorno dati in formato EBCDIC altrimenti ASCII
005200170124     C                   if        %subst(prmppt:1:1) = 'S'
005300170124     C                   eval      iEBCD = *on
005400170124     C                   else
005500170124     C                   eval      iASCI = *on
005600170124     C                   endif
005700170124     C*
005800170124     C* Verifico il carattere separatore decimale indicato nei parametri del cliente/tipo file
005900170124     C                   select
006000170124     C                   when      %subst(prmppt:2:1) = '.'
006100170124     C                   eval      pSEP = '.'
006200170124     C                   other
006300170124     C                   eval      pSEP = ','
006400170124     C                   endsl
006500170124     C*
006600170124     C* Scorro tutto il file
006700170124     C                   read      tivgdtmp
006800170124     C                   dow       not %eof(tivgdtmp)
006900170124     C*
007000170124     C* Inizializzo DS di output
007100170124     C                   clear                   TIVAW000
007200170124     C                   clear                   tnvasAsciDS
007300170124     C                   clear                   tnvasEbcdDS
007400170124     C*
007500170124     C                   eval      tnvasDS = vgdDTA
007600170124     C*
007700170124     C* Gestisco formato richiesto ASCII/EBCDIC
007800170124     C                   select
007900170124     C                   when      iASCI
008000170124     C                   exsr      fmtVAS_ASCI
008100170124     C                   when      iEBCD
008200170124     C                   exsr      fmtVAS_EBCD
008300170124     C                   other
008400170124     C                   eval      wrkesito = '2'
008500170124     C                   endsl
008600170124     C*
008700170124     C* Se tutto ok => procedo
008800170124     C                   if        wrkesito = *blanks
008900170124     C*
009000170124     C* Scarico il buffer di output formato richiesto ASCII/EBCDIC
009100170124     C                   select
009200170124     C                   when      iASCI
009300170124     C                   eval      wVAWDTA = tnvasAsciDS
009400170124     C                   when      iEBCD
009500170124     C                   eval      wVAWDTA = tnvasEbcdDS
009600170124     C                   endsl
009700170124     C*
009800170124     C                   eval      VAWDTA = wVAWDTA
009900170124     C                   write     TIVAW000
010000170124     C                   endif
010100170124     C*
010200170124     C* Proseguo con il record successivo
010300170124     C                   read      tivgdtmp
010400170124     C                   enddo
010500170124     C*
010600170124     C                   if        wrkesito <> '2'
010700170124     C                   eval      wrkesito  = '0'
010800170124     C                   endif
010900991027     C*
011000910830     C                   ENDSR
011100170124
011200170124
011300170124
011400170124     C*
011500170124     C     fmtVAS_ASCI   BEGSR
011600170124     C*
011700170124     C                   MOVE      VASLNP        A_VXSLNP
011800170124     C                   MOVE      VASLNA        A_VXSLNA
011900170124     C                   MOVE      VASAAS        A_VXSAAS
012000170124     C                   MOVE      VASNRS        A_VXSNRS
012100170124     C                   MOVE      VASNSP        A_VXSNSP
012200170124     C                   MOVE      VASRMN        A_VXSRMN
012300170124     C                   MOVEL     VASCAS        W11              11            METTO
012400170124     C                   MOVE      pSEP          W11                            LA ,
012500170124     C                   MOVE      W11           W12              12            NEL C/A
012600170124     C                   MOVE      VASCAS        A_VXSCAS
012700170124     C                   MOVEL     W12           A_VXSCAS
012800170124     C                   MOVE      VASVCA        A_VXSVCA
012900170124     C                   MOVE      VASNRA        A_VXSNRA
013000170124     C                   MOVE      VASDTE        A_VXSDTE
013100170124     C                   MOVE      VASTPI        A_VXSTPI
013200170124     C                   MOVE      VASABI        A_VXSABI
013300170124     C                   MOVE      VASCAI        A_VXSCAI
013400170124     C                   MOVE      VASNDP        A_VXSNDP
013500170124     C                   MOVE      VASDDP        A_VXSDDP
013600170124     C                   MOVE      VASCCB        A_VXSCCB
013700170124     C                   MOVE      VASABP        A_VXSABP
013800170124     C                   MOVE      VASCAP        A_VXSCAP
013900170124     C                   MOVE      VASFPC        A_VXSFPC
014000170124     C                   MOVE      VASDVA        A_VXSDVA
014100170124     C                   MOVE      VASKSC        A_VXSKSC
014200170124     C                   MOVE      VASRMA        A_VXSRMA
014300170124     C*
014400170124     C                   ENDSR
014500170124
014600170124
014700170124
014800170124     C*
014900170124     C     fmtVAS_EBCD   BEGSR
015000170124     C*
015100170124     C                   MOVE      VASLNP        E_VXSLNP
015200170124     C                   MOVE      VASLNA        E_VXSLNA
015300170124     C                   MOVE      VASAAS        E_VXSAAS
015400170124     C                   MOVE      VASNRS        E_VXSNRS
015500170124     C                   MOVE      VASNSP        E_VXSNSP
015600170124     C                   MOVE      VASRMN        E_VXSRMN
015700170124     C                   MOVE      VASCAS        E_VXSCAS
015800170124     C                   MOVE      VASVCA        E_VXSVCA
015900170124     C                   MOVE      VASNRA        E_VXSNRA
016000170124     C                   MOVE      VASDTE        E_VXSDTE
016100170124     C                   MOVE      VASTPI        E_VXSTPI
016200170124     C                   MOVE      VASABI        E_VXSABI
016300170124     C                   MOVE      VASCAI        E_VXSCAI
016400170124     C                   MOVE      VASNDP        E_VXSNDP
016500170124     C                   MOVE      VASDDP        E_VXSDDP
016600170124     C                   MOVE      VASCCB        E_VXSCCB
016700170124     C                   MOVE      VASABP        E_VXSABP
016800170124     C                   MOVE      VASCAP        E_VXSCAP
016900170124     C                   MOVE      VASFPC        E_VXSFPC
017000170124     C                   MOVE      VASDVA        E_VXSDVA
017100170124     C                   MOVE      VASKSC        E_VXSKSC
017200170124     C                   MOVE      VASRMA        E_VXSRMA
017300170124     C*
017400170124     C                   ENDSR
017500170124
017600170124
017700991027
017800991027      /TITLE Operazioni iniziali.
017900991027     C     *inzsr        BEGSR
018000991027     C*
018100991027     C     *ENTRY        PLIST
018200991027     C                   parm                    prmppt
018300991027     C     wrkesito      parm      wrkesito      prmesito
018400991027     C*
018500991027     C                   ENDSR
