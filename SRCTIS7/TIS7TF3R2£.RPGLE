000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030709     FTITAS30C  IF   E           K DISK
000500030709     FTITAA30C  IF   E           K DISK
000600161214     FTITAH30C  IF   E           K DISK
000700161214     FFIAR531C  IF   E           K DISK
000800161214     Ftivgdtmp  if   E             DISK
000900161214     FTIVAWWWT  uF A E             DISK    commit
001000030709     D*------------------
001100030709     D* DS REPERIMENTO DATI CLIENTE
001200030709     D*-------------------
001300030709     D BS69DS        E DS                  EXTNAME(TIBS69DS)
001400030709     D ACODS         E DS                  EXTNAME(CNACO00F)
001500030709     D INDDS         E DS                  EXTNAME(CNIND00F)
001600030709     D CLPDS         E DS                  EXTNAME(CNCLP00F)
001700030709     D CLSDS         E DS                  EXTNAME(FNCLS00F)
001800991027
001900991027     D prmppt          s             50
002000991027     D prmesito        s              1
002100991027     D wrkesito        s                   like(prmesito)
002200090325     D tivafds       e ds                  extname(tivaf00t)
002300090325     D dsvafa        e ds                  extname(tivafwwt1)
002400161214     D DAR5FAT       e ds                  inz
002500161214     D pCSV            s              1    inz
002600161214     D pDLM            s              1    inz varying
002700160926
002800160926     D wPKB            s              8
002900160926     D wVLF            s              6
003000160926     D wQFT            s             14
003100160926     D wPOR            s             12
003200160926     D wVA1            s             12
003300160926     D wVA2            s             12
003400160926     D wVA3            s             12
003500160926     D wVA4            s             12
003600160926     D wVA5            s             12
003700160926     D wVA6            s             12
003800160926     D wVA7            s             12
003900160926     D wVA8            s             12
004000160926     D wVAX            s             12
004100160926     D wIMV            s             14
004200160926     D wIAS            s             14
004300160926     D wPKC            s              8
004400160926     D wVLC            s              6
004500161214     D wPVL            s              8
004600161214     D wDCM            s              8
004700161214     D wCCA            s              1
004800161214     D wNNC            s             35
004900161214     D wPKTAS          s              8
005000161214     D wVLTAS          s              6
005100170828     D wIDSPED         s             16
005200161214
005300161214     D iINTI           s               N   inz
005400161214     D iINTD           s               N   inz
005500161214     D iINTE           s               N   inz
005600161214     D iEXT            s               N   inz
005700161214     D iSEGN           s               N   inz
005800170127     D iPVL            s               N   inz
005900161214     D iFAT            s               N   inz
006000170118     D iDCM            s               N   inz
006100161214
006200161214
006300170118     C*
006400170118     C* Avvio il monitoring del intero flusso
006500170118     C                   monitor
006600170118     C*
006700170118     C* Verifica esistenza record nel membro di output
006800030908     C                   EXSR      chkRECFILMBR
006900170118     C*
007000170118     C* Effettuo traduzione
007100991027     C                   EXSR      traduci
007200170118     C*
007300170118     C* Gestisco eventuale errore
007400170118     C                   on-error
007500170118     C*
007600170118     C* Imposto esito transazione a errore
007700170118     C                   EVAL      wrkesito = '2'
007800170118     C*
007900170118     C* Se errore => emetto operazione d ROLLBACK
008000170118     C                   ROLBK
008100170118     C*
008200170118     C* Arresto il monitoring
008300170118     C                   endmon
008400170118     C*
008500921023     C                   SETON                                        LR
008600161214
008700161214
008800030908
008900030908     C     chkRECFILMBR  BEGSR
009000030908     C*
009100100209     C                   READ      TIVAWWWT                               55
009200030908     C*
009300030908     C                   ENDSR
009400161214
009500161214
009600991027
009700991027     C     traduci       BEGSR
009800161214     C*
009900161214     C* Inizializzazione indicatori di procedure per parametri input
010000161214     C                   eval      iINTI = *off
010100161214     C                   eval      iINTD = *off
010200161214     C                   eval      iINTE = *off
010300161214     C                   eval      iEXT  = *off
010400161214     C                   eval      iSEGN = *off
010500170127     C                   eval      iPVL  = *off
010600161214     C                   eval      iFAT  = *off
010700170118     C                   eval      iDCM  = *off
010800161214     C*
010900161214     C* Se richiesta intestazione
011000161214     C                   select
011100161214     C                   when      %subst(prmppt:1:1) = 'I'
011200161214     C                   eval      iINTI = *on
011300161214     C                   when      %subst(prmppt:1:1) = 'D'
011400161214     C                   eval      iINTD = *on
011500161214     C                   when      %subst(prmppt:1:1) = 'E'
011600161214     C                   eval      iINTE = *on
011700161214     C                   endsl
011800161214     C*
011900161214     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
012000161214     C                   eval      pCSV = %subst(prmppt:2:1)
012100161214     C                   if        pCSV = *blanks
012200161214     C                   eval      pCSV = ','
012300161214     C                   endif
012400161214     C*
012500161214     C* Verifico se richiesto un particolare carattere d separatore decimale
012600161214     C                   if        %subst(prmppt:3:1) = ','
012700161214     C                   setoff                                       60
012800161214     C                   else
012900161214     C                   seton                                        60
013000161214     C                   endif
013100161214     C*
013200161214     C* Verifico il carattere delimitatore test indicato nei parametri del cliente/tipo file
013300161214     C                   select
013400161214     C                   when      %subst(prmppt:4:1) = *blanks
013500161214     C                   eval      pDLM = '"'
013600161214     C                   when      %subst(prmppt:4:1) = 'N'
013700161214     C                   eval      pDLM = ''
013800161214     C                   other
013900161214     C                   eval      pDLM = %subst(prmppt:4:1)
014000161214     C                   endsl
014100161214     C*
014200161214     C* Se richiesti dati di estensione destinatario/mittente/mittente originale
014300161214     C                   if        %subst(prmppt:5:1) = 'S'
014400161214     C                   eval      iEXT = *on
014500161214     C                   endif
014600161214     C*
014700161214     C* Se richiesto anche indicazione PRIMO segnacollo cliente ("chi sono")
014800161214     C                   if        %subst(prmppt:6:1) = 'S'
014900161214     C                   eval      iSEGN = *on
015000161214     C                   endif
015100161214     C*
015200170127     C* Se richiesto anche dati del peso tassabile (PVL)
015300161214     C                   if        %subst(prmppt:7:1) = 'S'
015400170127     C                   eval      iPVL = *on
015500161214     C                   endif
015600170118     C*
015700170118     C* Se richiesti dati di estensione peso tassabile, data consegna merce e codice cons. anomala
015800170118     C                   if        %subst(prmppt:8:1) <> 'N' AND iEXT
015900170118     C                   eval      iDCM = *on
016000170118     C                   endif
016100170127     C*
016200170127     C* Se richiesto anche dati di tipo e valore peso/volume fatturati
016300170127     C                   if        %subst(prmppt:9:1) = 'S'
016400170127     C                   eval      iFAT = *on
016500170127     C                   endif
016600170828     C*
016700170828     C* Se richiesto di ritornare l'ID spedizione BRT come unico campo
016800170828     C                   setoff                                       6162
016900170828     C                   select
017000170828     C                   when      %subst(prmppt:10:1) = 'S'
017100170828     C                   seton                                        61
017200170828     C                   when      %subst(prmppt:10:1) = 'A'
017300170828     C                   seton                                        6162
017400170828     C                   endsl
017500161214     C*
017600161214     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
017700161214     C                   if        (iINTI OR iINTD OR iINTE) AND *in55 = *on
017800161214     C                   if        iINTI
017900170828     C  N61              eval      VAWDTA  =
018000161214     C                             pDLM+'VAFAAS'                     +pDLM+pCSV+
018100170828     C                             pDLM+'VAFLNP'                     +pDLM+pCSV+
018200161214     C                             pDLM+'VAFNRS'                     +pDLM+pCSV+
018300161214     C                             pDLM+'VAFNSP'                     +pDLM+pCSV+
018400161214     C                             pDLM+'VAFTBL'                     +pDLM+pCSV+
018500161214     C                             pDLM+'VAFMGS'                     +pDLM+pCSV+
018600161214     C                             pDLM+'VAFKSC'                     +pDLM+pCSV+
018700161214     C                             pDLM+'VAFLNA'                     +pDLM+pCSV+
018800161214     C                             pDLM+'VAFNCL'                     +pDLM+pCSV+
018900161214     C                             pDLM+'VAFPKB'                     +pDLM+pCSV+
019000161214     C                             pDLM+'VAFVLF'                     +pDLM+pCSV+
019100161214     C                             pDLM+'VAFQFT'                     +pDLM+pCSV+
019200161214     C                             pDLM+'VAFPOR'                     +pDLM+pCSV+
019300161214     C                             pDLM+'VAFSV1'                     +pDLM+pCSV+
019400161214     C                             pDLM+'VAFVA1'                     +pDLM+pCSV+
019500161214     C                             pDLM+'VAFSV2'                     +pDLM+pCSV+
019600161214     C                             pDLM+'VAFVA2'                     +pDLM+pCSV+
019700161214     C                             pDLM+'VAFSV3'                     +pDLM+pCSV+
019800161214     C                             pDLM+'VAFVA3'                     +pDLM+pCSV+
019900161214     C                             pDLM+'VAFSV4'                     +pDLM+pCSV+
020000161214     C                             pDLM+'VAFVA4'                     +pDLM+pCSV+
020100161214     C                             pDLM+'VAFSV5'                     +pDLM+pCSV+
020200161214     C                             pDLM+'VAFVA5'                     +pDLM+pCSV+
020300161214     C                             pDLM+'VAFSV6'                     +pDLM+pCSV+
020400161214     C                             pDLM+'VAFVA6'                     +pDLM+pCSV+
020500161214     C                             pDLM+'VAFSV7'                     +pDLM+pCSV+
020600161214     C                             pDLM+'VAFVA7'                     +pDLM+pCSV+
020700161214     C                             pDLM+'VAFSV8'                     +pDLM+pCSV+
020800161214     C                             pDLM+'VAFVA8'                     +pDLM+pCSV+
020900161214     C                             pDLM+'VAFVAX'                     +pDLM+pCSV+
021000161214     C                             pDLM+'VAFIMV'                     +pDLM+pCSV+
021100161214     C                             pDLM+'VAFDFT'                     +pDLM+pCSV+
021200161214     C                             pDLM+'VAFNFT'                     +pDLM+pCSV+
021300161214     C                             pDLM+'VAFFIV'                     +pDLM+pCSV+
021400161214     C                             pDLM+'VAFDIV'                     +pDLM+pCSV+
021500161214     C                             pDLM+'VAFDRT'                     +pDLM+pCSV+
021600161214     C                             pDLM+'VAFRMN'                     +pDLM+pCSV+
021700161214     C                             pDLM+'VAFRMA'                     +pDLM+pCSV+
021800161214     C                             pDLM+'VAFRMX'                     +pDLM+pCSV+
021900161214     C                             pDLM+'VAFFTC'                     +pDLM+pCSV+
022000161214     C                             pDLM+'VAFTC2'                     +pDLM+pCSV+
022100161214     C                             pDLM+'VAFTSP'                     +pDLM+pCSV+
022200161214     C                             pDLM+'VAFFAP'                     +pDLM+pCSV+
022300161214     C                             pDLM+'VAFFIN'                     +pDLM+pCSV+
022400161214     C                             pDLM+'VAFCTR'                     +pDLM+pCSV+
022500161214     C                             pDLM+'VAFNAS'                     +pDLM+pCSV+
022600161214     C                             pDLM+'VAFCTS'                     +pDLM+pCSV+
022700161214     C                             pDLM+'VAFCAD'                     +pDLM+pCSV+
022800161214     C                             pDLM+'VAFLOD'                     +pDLM+pCSV+
022900161214     C                             pDLM+'VAFPRD'                     +pDLM+pCSV+
023000161214     C                             pDLM+'VAFNZD'                     +pDLM+pCSV+
023100161214     C                             pDLM+'VAFVAS'                     +pDLM+pCSV+
023200161214     C                             pDLM+'VAFIAS'                     +pDLM+pCSV+
023300161214     C                             pDLM+'VAFFPC'                     +pDLM+pCSV+
023400161214     C                             pDLM+'VAFPKC'                     +pDLM+pCSV+
023500161214     C                             pDLM+'VAFFVC'                     +pDLM+pCSV+
023600161214     C                             pDLM+'VAFVLC'                     +pDLM
023700170828     C   61              eval      VAWDTA  =
023800170828     C                             pDLM+'VAFAAS'                     +pDLM+pCSV+
023900170828     C                             pDLM+'VAFIDS'                     +pDLM+pCSV+
024000170828     C                             pDLM+'VAFTBL'                     +pDLM+pCSV+
024100170828     C                             pDLM+'VAFMGS'                     +pDLM+pCSV+
024200170828     C                             pDLM+'VAFKSC'                     +pDLM+pCSV+
024300170828     C                             pDLM+'VAFLNA'                     +pDLM+pCSV+
024400170828     C                             pDLM+'VAFNCL'                     +pDLM+pCSV+
024500170828     C                             pDLM+'VAFPKB'                     +pDLM+pCSV+
024600170828     C                             pDLM+'VAFVLF'                     +pDLM+pCSV+
024700170828     C                             pDLM+'VAFQFT'                     +pDLM+pCSV+
024800170828     C                             pDLM+'VAFPOR'                     +pDLM+pCSV+
024900170828     C                             pDLM+'VAFSV1'                     +pDLM+pCSV+
025000170828     C                             pDLM+'VAFVA1'                     +pDLM+pCSV+
025100170828     C                             pDLM+'VAFSV2'                     +pDLM+pCSV+
025200170828     C                             pDLM+'VAFVA2'                     +pDLM+pCSV+
025300170828     C                             pDLM+'VAFSV3'                     +pDLM+pCSV+
025400170828     C                             pDLM+'VAFVA3'                     +pDLM+pCSV+
025500170828     C                             pDLM+'VAFSV4'                     +pDLM+pCSV+
025600170828     C                             pDLM+'VAFVA4'                     +pDLM+pCSV+
025700170828     C                             pDLM+'VAFSV5'                     +pDLM+pCSV+
025800170828     C                             pDLM+'VAFVA5'                     +pDLM+pCSV+
025900170828     C                             pDLM+'VAFSV6'                     +pDLM+pCSV+
026000170828     C                             pDLM+'VAFVA6'                     +pDLM+pCSV+
026100170828     C                             pDLM+'VAFSV7'                     +pDLM+pCSV+
026200170828     C                             pDLM+'VAFVA7'                     +pDLM+pCSV+
026300170828     C                             pDLM+'VAFSV8'                     +pDLM+pCSV+
026400170828     C                             pDLM+'VAFVA8'                     +pDLM+pCSV+
026500170828     C                             pDLM+'VAFVAX'                     +pDLM+pCSV+
026600170828     C                             pDLM+'VAFIMV'                     +pDLM+pCSV+
026700170828     C                             pDLM+'VAFDFT'                     +pDLM+pCSV+
026800170828     C                             pDLM+'VAFNFT'                     +pDLM+pCSV+
026900170828     C                             pDLM+'VAFFIV'                     +pDLM+pCSV+
027000170828     C                             pDLM+'VAFDIV'                     +pDLM+pCSV+
027100170828     C                             pDLM+'VAFDRT'                     +pDLM+pCSV+
027200170828     C                             pDLM+'VAFRMN'                     +pDLM+pCSV+
027300170828     C                             pDLM+'VAFRMA'                     +pDLM+pCSV+
027400170828     C                             pDLM+'VAFRMX'                     +pDLM+pCSV+
027500170828     C                             pDLM+'VAFFTC'                     +pDLM+pCSV+
027600170828     C                             pDLM+'VAFTC2'                     +pDLM+pCSV+
027700170828     C                             pDLM+'VAFTSP'                     +pDLM+pCSV+
027800170828     C                             pDLM+'VAFFAP'                     +pDLM+pCSV+
027900170828     C                             pDLM+'VAFFIN'                     +pDLM+pCSV+
028000170828     C                             pDLM+'VAFCTR'                     +pDLM+pCSV+
028100170828     C                             pDLM+'VAFNAS'                     +pDLM+pCSV+
028200170828     C                             pDLM+'VAFCTS'                     +pDLM+pCSV+
028300170828     C                             pDLM+'VAFCAD'                     +pDLM+pCSV+
028400170828     C                             pDLM+'VAFLOD'                     +pDLM+pCSV+
028500170828     C                             pDLM+'VAFPRD'                     +pDLM+pCSV+
028600170828     C                             pDLM+'VAFNZD'                     +pDLM+pCSV+
028700170828     C                             pDLM+'VAFVAS'                     +pDLM+pCSV+
028800170828     C                             pDLM+'VAFIAS'                     +pDLM+pCSV+
028900170828     C                             pDLM+'VAFFPC'                     +pDLM+pCSV+
029000170828     C                             pDLM+'VAFPKC'                     +pDLM+pCSV+
029100170828     C                             pDLM+'VAFFVC'                     +pDLM+pCSV+
029200170828     C                             pDLM+'VAFVLC'                     +pDLM
029300161214     C*
029400161214     C                   if        iEXT
029500161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
029600161214     C                             pDLM+'VAFRSD'                     +pDLM+pCSV+
029700161214     C                             pDLM+'VAFIND'                     +pDLM+pCSV+
029800161214     C                             pDLM+'VAFRSM'                     +pDLM+pCSV+
029900161214     C                             pDLM+'VAFINM'                     +pDLM+pCSV+
030000161214     C                             pDLM+'VAFCAM'                     +pDLM+pCSV+
030100161214     C                             pDLM+'VAFLOM'                     +pDLM+pCSV+
030200161214     C                             pDLM+'VAFPRM'                     +pDLM+pCSV+
030300161214     C                             pDLM+'VAFNZM'                     +pDLM+pCSV+
030400161214     C                             pDLM+'VAFRMO'                     +pDLM+pCSV+
030500161214     C                             pDLM+'VAFCMO'                     +pDLM+pCSV+
030600170118     C                             pDLM+'VAFNMO'                     +pDLM
030700161214     C                   endif
030800170130     C*
030900170130     C                   if        iPVL
031000170130     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
031100170130     C                             pDLM+'VAFPVL'                     +pDLM
031200170130     C                   endif
031300170118     C*
031400170127     C                   if        iDCM
031500170118     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
031600170118     C                             pDLM+'VAFDCM'                     +pDLM+pCSV+
031700170118     C                             pDLM+'VAFCCA'                     +pDLM
031800170118     C                   endif
031900161214     C*
032000161214     C                   if        iFAT
032100161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
032200161214     C                             pDLM+'VAFFPTAS'                   +pDLM+pCSV+
032300161214     C                             pDLM+'VAFPKTAS'                   +pDLM+pCSV+
032400161214     C                             pDLM+'VAFFVTAS'                   +pDLM+pCSV+
032500161214     C                             pDLM+'VAFVLTAS'                   +pDLM
032600161214     C                   endif
032700161214     C*
032800161214     C                   if        iSEGN
032900161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
033000161214     C                             pDLM+'VAFDKC'                     +pDLM
033100161214     C                   endif
033200161214     C                   endif
033300161214     C*
033400161214     C                   if        iINTD
033500170828     C  N61              eval      VAWDTA  =
033600161214     C                             pDLM+'ANNO SPED.'                 +pDLM+pCSV+
033700161215     C                             pDLM+'FIL. PARTENZA'              +pDLM+pCSV+
033800161214     C                             pDLM+'NUMERO SERIE'               +pDLM+pCSV+
033900161214     C                             pDLM+'NUMERO SPED.'               +pDLM+pCSV+
034000161214     C                             pDLM+'TIPO BOLLA'                 +pDLM+pCSV+
034100161214     C                             pDLM+'MESE/GIORNO SPED.'          +pDLM+pCSV+
034200161214     C                             pDLM+'CODICE CLIENTE'             +pDLM+pCSV+
034300161215     C                             pDLM+'FIL. ARRIVO'                +pDLM+pCSV+
034400161214     C                             pDLM+'NUMERO COLLI'               +pDLM+pCSV+
034500161214     C                             pDLM+'PESO BOLLETTATO IN KG.'     +pDLM+pCSV+
034600161214     C                             pDLM+'VOLUME DA FATTURARE'        +pDLM+pCSV+
034700161214     C                             pDLM+'QUANTITA DA FATTURARE'      +pDLM+pCSV+
034800161214     C                             pDLM+'PORTO'                      +pDLM+pCSV+
034900161214     C                             pDLM+'SIGLA VARIA 1'              +pDLM+pCSV+
035000161214     C                             pDLM+'VARIA 1'                    +pDLM+pCSV+
035100161214     C                             pDLM+'SIGLA VARIA 2'              +pDLM+pCSV+
035200161214     C                             pDLM+'VARIA 2'                    +pDLM+pCSV+
035300161214     C                             pDLM+'SIGLA VARIA 3'              +pDLM+pCSV+
035400161214     C                             pDLM+'VARIA 3'                    +pDLM+pCSV+
035500161214     C                             pDLM+'SIGLA VARIA 4'              +pDLM+pCSV+
035600161214     C                             pDLM+'VARIA 4'                    +pDLM+pCSV+
035700161214     C                             pDLM+'SIGLA VARIA 5'              +pDLM+pCSV+
035800161214     C                             pDLM+'VARIA 5'                    +pDLM+pCSV+
035900161214     C                             pDLM+'SIGLA VARIA 6'              +pDLM+pCSV+
036000161214     C                             pDLM+'VARIA 6'                    +pDLM+pCSV+
036100161214     C                             pDLM+'SIGLA VARIA 7'              +pDLM+pCSV+
036200161214     C                             pDLM+'VARIA 7'                    +pDLM+pCSV+
036300161214     C                             pDLM+'SIGLA VARIA 8'              +pDLM+pCSV+
036400161214     C                             pDLM+'VARIA 8'                    +pDLM+pCSV+
036500161214     C                             pDLM+'CUMULATIVO ALTRE VARIE'     +pDLM+pCSV+
036600161214     C                             pDLM+'IMPONIBILE IVA'             +pDLM+pCSV+
036700161214     C                             pDLM+'DATA FATTURA (AMG)'         +pDLM+pCSV+
036800161214     C                             pDLM+'NUMERO FATTURA'             +pDLM+pCSV+
036900161214     C                             pDLM+'NUMERO REGISTRO FATTURA'    +pDLM+pCSV+
037000161214     C                             pDLM+'VALUTA FATTURA'             +pDLM+pCSV+
037100161214     C                             pDLM+'DATA RITIRO (AMG)'          +pDLM+pCSV+
037200161214     C                             pDLM+'RIFERIM.MITTENTE NUMERICO'  +pDLM+pCSV+
037300161214     C                             pDLM+'RIFERIM.MITTENTE ALFABETICO'+pDLM+pCSV+
037400161214     C                             pDLM+'ALTRO RIFERIM.MITTENTE'     +pDLM+pCSV+
037500161214     C                             pDLM+'1 CONSEGNA PARTICOLARE'     +pDLM+pCSV+
037600161214     C                             pDLM+'2 CONSEGNA PARTICOLARE'     +pDLM+pCSV+
037700161214     C                             pDLM+'TIPO SERVIZIO SPED.'        +pDLM+pCSV+
037800161214     C                             pDLM+'TIPO ANTEPORTO'             +pDLM+pCSV+
037900161214     C                             pDLM+'TIPO INOLTRO'               +pDLM+pCSV+
038000161214     C                             pDLM+'CODICE TARIFFA'             +pDLM+pCSV+
038100161214     C                             pDLM+'NATURA MERCE'               +pDLM+pCSV+
038200161214     C                             pDLM+'COD.TASSAZIONE DESTINATARIO'+pDLM+pCSV+
038300161214     C                             pDLM+'CAP DESTINATARIO'           +pDLM+pCSV+
038400161214     C                             pDLM+'LOCALITA DESTINATARIO'      +pDLM+pCSV+
038500161214     C                             pDLM+'PROVINCIA DESTINATARIO'     +pDLM+pCSV+
038600161214     C                             pDLM+'NAZIONE DESTINATARIO'       +pDLM+pCSV+
038700161214     C                             pDLM+'VALUTA IMPORTO DA ASSICUR.' +pDLM+pCSV+
038800161214     C                             pDLM+'IMPORTO DA ASSICURARE'      +pDLM+pCSV+
038900161214     C                             pDLM+'TIPO PESO RILEVATO'         +pDLM+pCSV+
039000161214     C                             pDLM+'PESO RILEVATO IN KG.'       +pDLM+pCSV+
039100161214     C                             pDLM+'TIPO VOLUME RILEVATO'       +pDLM+pCSV+
039200161214     C                             pDLM+'VOLUME RILEVATO'            +pDLM
039300170828     C   61              eval      VAWDTA  =
039400170828     C                             pDLM+'ANNO SPED.'                 +pDLM+pCSV+
039500170828     C                             pDLM+'ID SPED. BRT'               +pDLM+pCSV+
039600170828     C                             pDLM+'TIPO BOLLA'                 +pDLM+pCSV+
039700170828     C                             pDLM+'MESE/GIORNO SPED.'          +pDLM+pCSV+
039800170828     C                             pDLM+'CODICE CLIENTE'             +pDLM+pCSV+
039900170828     C                             pDLM+'FIL. ARRIVO'                +pDLM+pCSV+
040000170828     C                             pDLM+'NUMERO COLLI'               +pDLM+pCSV+
040100170828     C                             pDLM+'PESO BOLLETTATO IN KG.'     +pDLM+pCSV+
040200170828     C                             pDLM+'VOLUME DA FATTURARE'        +pDLM+pCSV+
040300170828     C                             pDLM+'QUANTITA DA FATTURARE'      +pDLM+pCSV+
040400170828     C                             pDLM+'PORTO'                      +pDLM+pCSV+
040500170828     C                             pDLM+'SIGLA VARIA 1'              +pDLM+pCSV+
040600170828     C                             pDLM+'VARIA 1'                    +pDLM+pCSV+
040700170828     C                             pDLM+'SIGLA VARIA 2'              +pDLM+pCSV+
040800170828     C                             pDLM+'VARIA 2'                    +pDLM+pCSV+
040900170828     C                             pDLM+'SIGLA VARIA 3'              +pDLM+pCSV+
041000170828     C                             pDLM+'VARIA 3'                    +pDLM+pCSV+
041100170828     C                             pDLM+'SIGLA VARIA 4'              +pDLM+pCSV+
041200170828     C                             pDLM+'VARIA 4'                    +pDLM+pCSV+
041300170828     C                             pDLM+'SIGLA VARIA 5'              +pDLM+pCSV+
041400170828     C                             pDLM+'VARIA 5'                    +pDLM+pCSV+
041500170828     C                             pDLM+'SIGLA VARIA 6'              +pDLM+pCSV+
041600170828     C                             pDLM+'VARIA 6'                    +pDLM+pCSV+
041700170828     C                             pDLM+'SIGLA VARIA 7'              +pDLM+pCSV+
041800170828     C                             pDLM+'VARIA 7'                    +pDLM+pCSV+
041900170828     C                             pDLM+'SIGLA VARIA 8'              +pDLM+pCSV+
042000170828     C                             pDLM+'VARIA 8'                    +pDLM+pCSV+
042100170828     C                             pDLM+'CUMULATIVO ALTRE VARIE'     +pDLM+pCSV+
042200170828     C                             pDLM+'IMPONIBILE IVA'             +pDLM+pCSV+
042300170828     C                             pDLM+'DATA FATTURA (AMG)'         +pDLM+pCSV+
042400170828     C                             pDLM+'NUMERO FATTURA'             +pDLM+pCSV+
042500170828     C                             pDLM+'NUMERO REGISTRO FATTURA'    +pDLM+pCSV+
042600170828     C                             pDLM+'VALUTA FATTURA'             +pDLM+pCSV+
042700170828     C                             pDLM+'DATA RITIRO (AMG)'          +pDLM+pCSV+
042800170828     C                             pDLM+'RIFERIM.MITTENTE NUMERICO'  +pDLM+pCSV+
042900170828     C                             pDLM+'RIFERIM.MITTENTE ALFABETICO'+pDLM+pCSV+
043000170828     C                             pDLM+'ALTRO RIFERIM.MITTENTE'     +pDLM+pCSV+
043100170828     C                             pDLM+'1 CONSEGNA PARTICOLARE'     +pDLM+pCSV+
043200170828     C                             pDLM+'2 CONSEGNA PARTICOLARE'     +pDLM+pCSV+
043300170828     C                             pDLM+'TIPO SERVIZIO SPED.'        +pDLM+pCSV+
043400170828     C                             pDLM+'TIPO ANTEPORTO'             +pDLM+pCSV+
043500170828     C                             pDLM+'TIPO INOLTRO'               +pDLM+pCSV+
043600170828     C                             pDLM+'CODICE TARIFFA'             +pDLM+pCSV+
043700170828     C                             pDLM+'NATURA MERCE'               +pDLM+pCSV+
043800170828     C                             pDLM+'COD.TASSAZIONE DESTINATARIO'+pDLM+pCSV+
043900170828     C                             pDLM+'CAP DESTINATARIO'           +pDLM+pCSV+
044000170828     C                             pDLM+'LOCALITA DESTINATARIO'      +pDLM+pCSV+
044100170828     C                             pDLM+'PROVINCIA DESTINATARIO'     +pDLM+pCSV+
044200170828     C                             pDLM+'NAZIONE DESTINATARIO'       +pDLM+pCSV+
044300170828     C                             pDLM+'VALUTA IMPORTO DA ASSICUR.' +pDLM+pCSV+
044400170828     C                             pDLM+'IMPORTO DA ASSICURARE'      +pDLM+pCSV+
044500170828     C                             pDLM+'TIPO PESO RILEVATO'         +pDLM+pCSV+
044600170828     C                             pDLM+'PESO RILEVATO IN KG.'       +pDLM+pCSV+
044700170828     C                             pDLM+'TIPO VOLUME RILEVATO'       +pDLM+pCSV+
044800170828     C                             pDLM+'VOLUME RILEVATO'            +pDLM
044900161214     C*
045000161214     C                   if        iEXT
045100161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
045200161214     C                             pDLM+'RAGIONE SOCIALE DESTINAT.'  +pDLM+pCSV+
045300161214     C                             pDLM+'INDIRIZZO DESTINATARIO'     +pDLM+pCSV+
045400161214     C                             pDLM+'RAGIONE SOCIALE MITTENTE'   +pDLM+pCSV+
045500161214     C                             pDLM+'INDIRIZZO MITTENTE'         +pDLM+pCSV+
045600161214     C                             pDLM+'CAP MITTENTE'               +pDLM+pCSV+
045700161214     C                             pDLM+'LOCALITA MITTENTE'          +pDLM+pCSV+
045800161214     C                             pDLM+'PROVINCIA MITTENTE'         +pDLM+pCSV+
045900161214     C                             pDLM+'NAZIONE MITTENTE'           +pDLM+pCSV+
046000161214     C                             pDLM+'RAG.SOC. MITTENTE ORIGINALE'+pDLM+pCSV+
046100161214     C                             pDLM+'CAP MITTENTE ORIGINALE'     +pDLM+pCSV+
046200170118     C                             pDLM+'NAZIONE MITTENTE ORIGINALE' +pDLM
046300161214     C                   endif
046400170130     C*
046500170130     C                   if        iPVL
046600170130     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
046700170130     C                             pDLM+'PESO TASSABILE'             +pDLM
046800170130     C                   endif
046900170118     C*
047000170118     C                   if        iDCM
047100170118     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
047200170118     C                             pDLM+'DATA CONSEGNA MERCE'        +pDLM+pCSV+
047300170118     C                             pDLM+'CONSEGNA ANOMALA'           +pDLM
047400170118     C                   endif
047500161214     C*
047600161214     C                   if        iFAT
047700161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
047800161214     C                             pDLM+'FLAG PESO TASSAZZ.'         +pDLM+pCSV+
047900170127     C                             pDLM+'PESO TASSATO'               +pDLM+pCSV+
048000161214     C                             pDLM+'FLAG VOLUME TASSAZZ.'       +pDLM+pCSV+
048100170127     C                             pDLM+'VOLUME TASSATO'             +pDLM
048200161214     C                   endif
048300161214     C*
048400161214     C                   if        iSEGN
048500161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
048600161214     C                             pDLM+'SEGNAC.'                    +pDLM
048700161214     C                   endif
048800161214     C                   endif
048900161214     C*
049000161214     C                   if        iINTE
049100170828     C  N61              eval      VAWDTA  =
049200161214     C                             pDLM+'YEAR OF SHIPMENT'           +pDLM+pCSV+
049300161215     C                             pDLM+'DEPARTURE DEPOT'            +pDLM+pCSV+
049400161214     C                             pDLM+'SERIAL NUMBER'              +pDLM+pCSV+
049500161214     C                             pDLM+'SHIPMENT NUMBER'            +pDLM+pCSV+
049600161214     C                             pDLM+'DELIV. NOTE TYPE'           +pDLM+pCSV+
049700161214     C                             pDLM+'SHIPMENT MONTH/DAY'         +pDLM+pCSV+
049800161214     C                             pDLM+'CLIENT CODE'                +pDLM+pCSV+
049900161215     C                             pDLM+'DISTRIBUTION DEPOT'         +pDLM+pCSV+
050000161214     C                             pDLM+'NO. OF PACKS'               +pDLM+pCSV+
050100170124     C                             pDLM+'DATA ENTRY WEIGHT/KG.'      +pDLM+pCSV+
050200161214     C                             pDLM+'VOLUME TO INVOICE'          +pDLM+pCSV+
050300161214     C                             pDLM+'QUANTITY TO INVOICE'        +pDLM+pCSV+
050400161214     C                             pDLM+'FREIGHTS'                   +pDLM+pCSV+
050500161214     C                             pDLM+'MISC. SYMBOL 1'             +pDLM+pCSV+
050600161214     C                             pDLM+'MISC. 1'                    +pDLM+pCSV+
050700161214     C                             pDLM+'MISC. SYMBOL 2'             +pDLM+pCSV+
050800161214     C                             pDLM+'MISC. 2'                    +pDLM+pCSV+
050900161214     C                             pDLM+'MISC. SYMBOL 3'             +pDLM+pCSV+
051000161214     C                             pDLM+'MISC. 3'                    +pDLM+pCSV+
051100161214     C                             pDLM+'MISC. SYMBOL 4'             +pDLM+pCSV+
051200161214     C                             pDLM+'MISC. 4'                    +pDLM+pCSV+
051300161214     C                             pDLM+'MISC. SYMBOL 5'             +pDLM+pCSV+
051400161214     C                             pDLM+'MISC. 5'                    +pDLM+pCSV+
051500161214     C                             pDLM+'MISC. SYMBOL 6'             +pDLM+pCSV+
051600161214     C                             pDLM+'MISC. 6'                    +pDLM+pCSV+
051700161214     C                             pDLM+'MISC. SYMBOL 7'             +pDLM+pCSV+
051800161214     C                             pDLM+'MISC. 7'                    +pDLM+pCSV+
051900161214     C                             pDLM+'MISC. SYMBOL 8'             +pDLM+pCSV+
052000161214     C                             pDLM+'MISC. 8'                    +pDLM+pCSV+
052100161214     C                             pDLM+'OTHER MISC. SUM'            +pDLM+pCSV+
052200161214     C                             pDLM+'TAXABLE SUBTOTAL'           +pDLM+pCSV+
052300161214     C                             pDLM+'INVOICE DATE'               +pDLM+pCSV+
052400161214     C                             pDLM+'INVOICE NUMBER'             +pDLM+pCSV+
052500161214     C                             pDLM+'INVOICE REG. NUMBER'        +pDLM+pCSV+
052600161214     C                             pDLM+'INVOICE CURRENCY'           +pDLM+pCSV+
052700161214     C                             pDLM+'COLLECTION DATE (YMD)'      +pDLM+pCSV+
052800170124     C                             pDLM+'NUMERICAL SENDER REF.'      +pDLM+pCSV+
052900161214     C                             pDLM+'ALPHAB. SENDER REF.'        +pDLM+pCSV+
053000161214     C                             pDLM+'OTHER SENDER REF.'          +pDLM+pCSV+
053100161214     C                             pDLM+'1 SPECIAL DELIVERY'         +pDLM+pCSV+
053200161214     C                             pDLM+'2 SPECIAL DELIVERY'         +pDLM+pCSV+
053300161214     C                             pDLM+'NOTE SERVICE TYPE'          +pDLM+pCSV+
053400161214     C                             pDLM+'FLAG COLLECTION IN PROVINCE'+pDLM+pCSV+
053500161214     C                             pDLM+'FLAG DELIVERY IN PROVINCE'  +pDLM+pCSV+
053600161214     C                             pDLM+'TARIFF CODE'                +pDLM+pCSV+
053700161214     C                             pDLM+'GOODS TYPE'                 +pDLM+pCSV+
053800161214     C                             pDLM+'DEST. TAXATION CODE'        +pDLM+pCSV+
053900161215     C                             pDLM+'DEST. ZIP CODE'             +pDLM+pCSV+
054000161215     C                             pDLM+'DEST. CITY'                 +pDLM+pCSV+
054100161215     C                             pDLM+'DEST. PROVINCE'             +pDLM+pCSV+
054200161215     C                             pDLM+'DEST. COUNTRY'              +pDLM+pCSV+
054300161214     C                             pDLM+'CURRENCY OF SUM TO INSURE'  +pDLM+pCSV+
054400161214     C                             pDLM+'SUM TO INSURE'              +pDLM+pCSV+
054500161214     C                             pDLM+'TYPE OF WEIGHT READ'        +pDLM+pCSV+
054600161214     C                             pDLM+'WEIGHT READ IN KG.'         +pDLM+pCSV+
054700161214     C                             pDLM+'TYPE OF VOLUME READ'        +pDLM+pCSV+
054800161214     C                             pDLM+'VOLUME READ'                +pDLM
054900170828     C   61              eval      VAWDTA  =
055000170828     C                             pDLM+'YEAR OF SHIPMENT'           +pDLM+pCSV+
055100170828     C                             pDLM+'BRT SHIPMENT ID'            +pDLM+pCSV+
055200170828     C                             pDLM+'DELIV. NOTE TYPE'           +pDLM+pCSV+
055300170828     C                             pDLM+'SHIPMENT MONTH/DAY'         +pDLM+pCSV+
055400170828     C                             pDLM+'CLIENT CODE'                +pDLM+pCSV+
055500170828     C                             pDLM+'DISTRIBUTION DEPOT'         +pDLM+pCSV+
055600170828     C                             pDLM+'NO. OF PACKS'               +pDLM+pCSV+
055700170828     C                             pDLM+'DATA ENTRY WEIGHT/KG.'      +pDLM+pCSV+
055800170828     C                             pDLM+'VOLUME TO INVOICE'          +pDLM+pCSV+
055900170828     C                             pDLM+'QUANTITY TO INVOICE'        +pDLM+pCSV+
056000170828     C                             pDLM+'FREIGHTS'                   +pDLM+pCSV+
056100170828     C                             pDLM+'MISC. SYMBOL 1'             +pDLM+pCSV+
056200170828     C                             pDLM+'MISC. 1'                    +pDLM+pCSV+
056300170828     C                             pDLM+'MISC. SYMBOL 2'             +pDLM+pCSV+
056400170828     C                             pDLM+'MISC. 2'                    +pDLM+pCSV+
056500170828     C                             pDLM+'MISC. SYMBOL 3'             +pDLM+pCSV+
056600170828     C                             pDLM+'MISC. 3'                    +pDLM+pCSV+
056700170828     C                             pDLM+'MISC. SYMBOL 4'             +pDLM+pCSV+
056800170828     C                             pDLM+'MISC. 4'                    +pDLM+pCSV+
056900170828     C                             pDLM+'MISC. SYMBOL 5'             +pDLM+pCSV+
057000170828     C                             pDLM+'MISC. 5'                    +pDLM+pCSV+
057100170828     C                             pDLM+'MISC. SYMBOL 6'             +pDLM+pCSV+
057200170828     C                             pDLM+'MISC. 6'                    +pDLM+pCSV+
057300170828     C                             pDLM+'MISC. SYMBOL 7'             +pDLM+pCSV+
057400170828     C                             pDLM+'MISC. 7'                    +pDLM+pCSV+
057500170828     C                             pDLM+'MISC. SYMBOL 8'             +pDLM+pCSV+
057600170828     C                             pDLM+'MISC. 8'                    +pDLM+pCSV+
057700170828     C                             pDLM+'OTHER MISC. SUM'            +pDLM+pCSV+
057800170828     C                             pDLM+'TAXABLE SUBTOTAL'           +pDLM+pCSV+
057900170828     C                             pDLM+'INVOICE DATE'               +pDLM+pCSV+
058000170828     C                             pDLM+'INVOICE NUMBER'             +pDLM+pCSV+
058100170828     C                             pDLM+'INVOICE REG. NUMBER'        +pDLM+pCSV+
058200170828     C                             pDLM+'INVOICE CURRENCY'           +pDLM+pCSV+
058300170828     C                             pDLM+'COLLECTION DATE (YMD)'      +pDLM+pCSV+
058400170828     C                             pDLM+'NUMERICAL SENDER REF.'      +pDLM+pCSV+
058500170828     C                             pDLM+'ALPHAB. SENDER REF.'        +pDLM+pCSV+
058600170828     C                             pDLM+'OTHER SENDER REF.'          +pDLM+pCSV+
058700170828     C                             pDLM+'1 SPECIAL DELIVERY'         +pDLM+pCSV+
058800170828     C                             pDLM+'2 SPECIAL DELIVERY'         +pDLM+pCSV+
058900170828     C                             pDLM+'NOTE SERVICE TYPE'          +pDLM+pCSV+
059000170828     C                             pDLM+'FLAG COLLECTION IN PROVINCE'+pDLM+pCSV+
059100170828     C                             pDLM+'FLAG DELIVERY IN PROVINCE'  +pDLM+pCSV+
059200170828     C                             pDLM+'TARIFF CODE'                +pDLM+pCSV+
059300170828     C                             pDLM+'GOODS TYPE'                 +pDLM+pCSV+
059400170828     C                             pDLM+'DEST. TAXATION CODE'        +pDLM+pCSV+
059500170828     C                             pDLM+'DEST. ZIP CODE'             +pDLM+pCSV+
059600170828     C                             pDLM+'DEST. CITY'                 +pDLM+pCSV+
059700170828     C                             pDLM+'DEST. PROVINCE'             +pDLM+pCSV+
059800170828     C                             pDLM+'DEST. COUNTRY'              +pDLM+pCSV+
059900170828     C                             pDLM+'CURRENCY OF SUM TO INSURE'  +pDLM+pCSV+
060000170828     C                             pDLM+'SUM TO INSURE'              +pDLM+pCSV+
060100170828     C                             pDLM+'TYPE OF WEIGHT READ'        +pDLM+pCSV+
060200170828     C                             pDLM+'WEIGHT READ IN KG.'         +pDLM+pCSV+
060300170828     C                             pDLM+'TYPE OF VOLUME READ'        +pDLM+pCSV+
060400170828     C                             pDLM+'VOLUME READ'                +pDLM
060500161214     C*
060600161214     C                   if        iEXT
060700161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
060800161214     C                             pDLM+'CONSIGNEE COMPANY NAME'     +pDLM+pCSV+
060900161214     C                             pDLM+'CONSIGNEE ADDRESS'          +pDLM+pCSV+
061000161214     C                             pDLM+'SENDER COMPANY NAME'        +pDLM+pCSV+
061100161214     C                             pDLM+'SENDER ADDRESS'             +pDLM+pCSV+
061200161214     C                             pDLM+'SENDER ZIP CODE'            +pDLM+pCSV+
061300161214     C                             pDLM+'SENDER TOWN'                +pDLM+pCSV+
061400161214     C                             pDLM+'SENDER PROVINCE'            +pDLM+pCSV+
061500161214     C                             pDLM+'SENDER COUNTRY'             +pDLM+pCSV+
061600161214     C                             pDLM+'ORIGINAL SENDER NAME'       +pDLM+pCSV+
061700161214     C                             pDLM+'ORIGINAL SENDER ZIP CODE'   +pDLM+pCSV+
061800170118     C                             pDLM+'ORIGINAL SENDER COUNTRY'    +pDLM
061900161214     C                   endif
062000170130     C*
062100170130     C                   if        iPVL
062200170130     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
062300170130     C                             pDLM+'CALCULATED WEIGHT'          +pDLM
062400170130     C                   endif
062500170118     C*
062600170118     C                   if        iDCM
062700170118     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
062800170118     C                             pDLM+'DELIVEY DATE'               +pDLM+pCSV+
062900170118     C                             pDLM+'SPECIFIC DELIVERY CODE'     +pDLM
063000170118     C                   endif
063100161214     C*
063200161214     C                   if        iFAT
063300161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
063400161214     C                             pDLM+'FLAG INVOICED WEIGHT'       +pDLM+pCSV+
063500161214     C                             pDLM+'INVOICED WEIGHT'            +pDLM+pCSV+
063600161214     C                             pDLM+'FLAG INVOICED VOLUME'       +pDLM+pCSV+
063700161214     C                             pDLM+'INVOICED VOLUME'            +pDLM
063800161214     C                   endif
063900161214     C*
064000161214     C                   if        iSEGN
064100161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
064200161214     C                             pDLM+'PARCEL ID'                  +pDLM
064300161214     C                   endif
064400161214     C                   endif
064500030325     C*
064600161214     C                   write     TIVAW000
064700161214     C                   endif
064800991027     C*
064900161214     C* Scorro tutto il file
065000161214     C                   read      tivgdtmp
065100161214     C                   dow       not %eof
065200161214     C*
065300161214     C* Inizializzazioni campi "dati"
065400161214     C                   exsr      inzVAR
065500161214     C*
065600161214     C* Valorizzo dati "corrrenti"
065700161214     C                   movel     vgddta        tivafds
065800030709     C*
065900030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
066000030709     C     KEYtas30C     CHAIN     titas30c
066100030709     C                   IF        %found(titas30c)
066200161214     C*
066300030709     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
066400030709     C                   EVAL      VHFRSD = TASRSD
066500030709     C                   EVAL      VHFIND = TASIND
066600161214     C                   MOVEL     TASDCM        wDCM
066700161214     C                   MOVEL     TASCCA        wCCA
066800160926     C                   EVAL      PVL = TASPVL
066900161214     C*
067000030709     C* X reperire i dati del mittente verifico il codice del cliente mittente:
067100030709     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
067200030709     C                   MOVE(P)   tasCCM        wCLI              4 0
067300030709     C                   IF        wCLI =    0 OR
067400030709     C                             wCLI = 8888 OR
067500030709     C                             wCLI = 9999
067600030709     C                   EVAL      taaTRC = 'M'
067700030709     C     KEYtaa30C     CHAIN     titaa30c
067800030709     C                   IF        %found(titaa30c)
067900030709     C                   EVAL      VHFRSM = TAARSC
068000030709     C                   EVAL      VHFINM = TAAIND
068100030709     C                   EVAL      VHFCAM = TAACAP
068200030709     C                   EVAL      VHFLOM = TAALOC
068300030709     C                   EVAL      VHFPRM = TAAPRV
068400070219     C                   EVAL      VHFNZM = TAANAZ
068500030709     C                   ENDIF
068600161214     C*
068700030709     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
068800030709     C                   ELSE
068900030709     C                   CLEAR                   BS69DS
069000030709     C                   CLEAR                   ACODS
069100030709     C                   CLEAR                   INDDS
069200030709     C                   CLEAR                   CLPDS
069300030709     C                   CLEAR                   CLSDS
069400030709     C                   Z-ADD     tasCCM        I69KAC
069500030709     C                   Z-ADD     tasCCM        I69KIN
069600030709     C                   CALL      'TIBS69R'
069700030709     C                   PARM                    BS69DS
069800030709     C                   PARM                    ACODS
069900030709     C                   PARM                    INDDS
070000030709     C                   PARM                    CLPDS
070100030709     C                   PARM                    CLSDS
070200030709     C     O69ERR        IFNE      '1'
070300030709     C                   EVAL      VHFRSM = ACORAG
070400030709     C                   EVAL      VHFINM = INDVIA
070500030709     C                   MOVEL(P)  INDCAP        VHFCAM
070600030709     C                   EVAL      VHFLOM = INDCIT
070700030709     C                   EVAL      VHFPRM = INDPRV
070800030709     C                   EVAL      VHFNZM = INDSTA
070900030709     C                   ENDIF
071000030709     C*
071100030709     C                   ENDIF
071200161214     C*
071300030709     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
071400030709     C                   EVAL      taaTRC = 'O'
071500030709     C     KEYtaa30C     CHAIN     titaa30c
071600030709     C                   IF        %found(titaa30c)
071700030709     C                   EVAL      VHFRMO = TAARSC
071800030709     C                   EVAL      VHFCMO = TAACAP
071900030709     C                   EVAL      VHFNMO = TAANAZ
072000030709     C                   ENDIF
072100030709     C                   ENDIF
072200161214     C*
072300161214     C* Se richiesto reperisco tipo e valore peso/volume fatturati
072400161214     C                   if        iFAT
072500161214     C                   exsr      RTVAR5FAT
072600161214     C                   endif
072700161214     C*
072800161214     C* Se richiesto indico PRIMO segnacollo cliente ("chi sono")
072900161214     C                   if        iSEGN
073000161214     C                   exsr      RTVSEGN
073100161214     C                   endif
073200160926     C*
073300160926     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
073400160926     C  N60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 , '))
073500160926     C  N60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 ,   '))
073600160926     C  N60              EVAL      wQFT = %trim(%editw(VAFQFT:'        0 ,   '))
073700160926     C  N60              EVAL      wPOR = %trim(%editw(VAFPOR:'      0 ,   '))
073800160926     C  N60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 ,   '))
073900160926     C  N60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 ,   '))
074000160926     C  N60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 ,   '))
074100160926     C  N60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 ,   '))
074200160926     C  N60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 ,   '))
074300160926     C  N60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 ,   '))
074400160926     C  N60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 ,   '))
074500160926     C  N60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 ,   '))
074600160926     C  N60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 ,   '))
074700160926     C  N60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 ,   '))
074800160926     C  N60              EVAL      wIAS = %trim(%editw(VAFIAS:'        0 ,   '))
074900160926     C  N60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 , '))
075000160926     C  N60              EVAL      wVLC = %trim(%editw(VAFVLC:'0 ,   '))
075100160926     C  N60              EVAL      wPVL = %trim(%editw(PVL:'    0 , '))
075200161214     C  N60              EVAL      wPKTAS = %trim(%editw(§AR5PKTAS:'    0 , '))
075300161214     C  N60              EVAL      wVLTAS = %trim(%editw(§AR5VLTAS:'0 ,   '))
075400160926     C   60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 . '))
075500160926     C   60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 .   '))
075600160926     C   60              EVAL      wQFT = %trim(%editw(VAFQFT:'        0 .   '))
075700160926     C   60              EVAL      wPOR = %trim(%editw(VAFPOR:'      0 .   '))
075800160926     C   60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 .   '))
075900160926     C   60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 .   '))
076000160926     C   60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 .   '))
076100160926     C   60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 .   '))
076200160926     C   60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 .   '))
076300160926     C   60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 .   '))
076400160926     C   60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 .   '))
076500160926     C   60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 .   '))
076600160926     C   60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 .   '))
076700160926     C   60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 .   '))
076800160926     C   60              EVAL      wIAS = %trim(%editw(VAFIAS:'        0 .   '))
076900160926     C   60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 . '))
077000160926     C   60              EVAL      wVLC = %trim(%editw(VAFVLC:'0 .   '))
077100160926     C   60              EVAL      wPVL = %trim(%editw(PVL:'    0 . '))
077200161214     C   60              EVAL      wPKTAS = %trim(%editw(§AR5PKTAS:'    0 . '))
077300161214     C   60              EVAL      wVLTAS = %trim(%editw(§AR5VLTAS:'0 .   '))
077400170828     C*
077500170828     C* Se richiesto di ritornare l'ID spedizione BRT come unico campo
077600170828     C                   select
077700170828     C                   when      *in62
077800170828     C                   eval      wIDSPED = %subst(%editc(VAFAAS:'X'):3:2) +
077900170828     C                                              %editc(VAFLNP:'X') +
078000170828     C                                              %editc(VAFNRS:'X') +
078100170828     C                                              %editc(VAFNSP:'X')
078200170828     C                   when      *in61
078300170828     C                   eval      wIDSPED = %editc(VAFLNP:'X') +
078400170828     C                                       %editc(VAFNRS:'X') +
078500170828     C                                       %editc(VAFNSP:'X')
078600170828     C                   endsl
078700030325     C*
078800170828     C  N61              eval      VAWDTA  = %trim(%editc(VAFAAS:'Q'))    +pCSV+
078900161214     C                                       %trim(%editc(VAFLNP:'Q'))    +pCSV+
079000161214     C                                       %trim(%editc(VAFNRS:'Q'))    +pCSV+
079100161214     C                                       %trim(%editc(VAFNSP:'Q'))    +pCSV+
079200161214     C                                       pDLM  +%trim(VAFTBL)+pDLM    +pCSV+
079300161214     C                                       %trim(%editc(VAFMGS:'Q'))    +pCSV+
079400161214     C                                       %trim(%editc(VAFKSC:'Q'))    +pCSV+
079500161214     C                                       %trim(%editc(VAFLNA:'Q'))    +pCSV+
079600161214     C                                       %trim(%editc(VAFNCL:'Q'))    +pCSV+
079700161214     C                                                %trim(wPKB)         +pCSV+
079800161214     C                                                %trim(wVLF)         +pCSV+
079900161214     C                                                %trim(wQFT)         +pCSV+
080000161214     C                                                %trim(wPOR)         +pCSV+
080100161214     C                                       pDLM  +%trim(VAFSV1)+pDLM    +pCSV+
080200161214     C                                                %trim(wVA1)         +pCSV+
080300161214     C                                       pDLM  +%trim(VAFSV2)+pDLM    +pCSV+
080400161214     C                                                %trim(wVA2)         +pCSV+
080500161214     C                                       pDLM  +%trim(VAFSV3)+pDLM    +pCSV+
080600161214     C                                                %trim(wVA3)         +pCSV+
080700161214     C                                       pDLM  +%trim(VAFSV4)+pDLM    +pCSV+
080800161214     C                                                %trim(wVA4)         +pCSV+
080900161214     C                                       pDLM  +%trim(VAFSV5)+pDLM    +pCSV+
081000161214     C                                                %trim(wVA5)         +pCSV+
081100161214     C                                       pDLM  +%trim(VAFSV6)+pDLM    +pCSV+
081200161214     C                                                %trim(wVA6)         +pCSV+
081300161214     C                                       pDLM  +%trim(VAFSV7)+pDLM    +pCSV+
081400161214     C                                                %trim(wVA7)         +pCSV+
081500161214     C                                       pDLM  +%trim(VAFSV8)+pDLM    +pCSV+
081600161214     C                                                %trim(wVA8)         +pCSV+
081700161214     C                                                %trim(wVAX)         +pCSV+
081800161214     C                                                %trim(wIMV)         +pCSV+
081900161214     C                                       %trim(%editc(VAFDFT:'Q'))    +pCSV+
082000161214     C                                       %trim(%editc(VAFNFT:'Q'))    +pCSV+
082100161214     C                                       %trim(%editc(VAFFIV:'Q'))    +pCSV+
082200161214     C                                       pDLM  +%trim(VAFDIV)+pDLM    +pCSV+
082300161214     C                                       %trim(%editc(VAFDRT:'Q'))    +pCSV+
082400161214     C                                       %trim(%editc(VAFRMN:'Q'))    +pCSV+
082500161214     C                                       pDLM  +%trim(VAFRMA)+pDLM    +pCSV+
082600161214     C                                       pDLM  +%trim(VAFRMX)+pDLM    +pCSV+
082700161214     C                                       pDLM  +%trim(VAFFTC)+pDLM    +pCSV+
082800161214     C                                       pDLM  +%trim(VAFTC2)+pDLM    +pCSV+
082900161214     C                                       pDLM  +%trim(VAFTSP)+pDLM    +pCSV+
083000161214     C                                       pDLM  +%trim(VAFFAP)+pDLM    +pCSV+
083100161214     C                                       pDLM  +%trim(VAFFIN)+pDLM    +pCSV+
083200161214     C                                       %trim(%editc(VAFCTR:'Q'))    +pCSV+
083300161214     C                                       pDLM  +%trim(VAFNAS)+pDLM    +pCSV+
083400161214     C                                       pDLM  +%trim(VAFCTS)+pDLM    +pCSV+
083500161214     C                                       pDLM  +%trim(VAFCAD)+pDLM    +pCSV+
083600161214     C                                       pDLM  +%trim(VAFLOD)+pDLM    +pCSV+
083700161214     C                                       pDLM  +%trim(VAFPRD)+pDLM    +pCSV+
083800161214     C                                       pDLM  +%trim(VAFNZD)+pDLM    +pCSV+
083900161214     C                                       pDLM  +%trim(VAFVAS)+pDLM    +pCSV+
084000161214     C                                                %trim(wIAS)         +pCSV+
084100161214     C                                       pDLM  +%trim(VAFFPC)+pDLM    +pCSV+
084200161214     C                                                %trim(wPKC)         +pCSV+
084300161214     C                                       pDLM  +%trim(VAFFVC)+pDLM    +pCSV+
084400161214     C                                                %trim(wVLC)
084500170828     C   61              eval      VAWDTA  = %trim(%editc(VAFAAS:'Q'))    +pCSV+
084600170828     C                                                %trim(wIDSPED)      +pCSV+
084700170828     C                                       pDLM  +%trim(VAFTBL)+pDLM    +pCSV+
084800170828     C                                       %trim(%editc(VAFMGS:'Q'))    +pCSV+
084900170828     C                                       %trim(%editc(VAFKSC:'Q'))    +pCSV+
085000170828     C                                       %trim(%editc(VAFLNA:'Q'))    +pCSV+
085100170828     C                                       %trim(%editc(VAFNCL:'Q'))    +pCSV+
085200170828     C                                                %trim(wPKB)         +pCSV+
085300170828     C                                                %trim(wVLF)         +pCSV+
085400170828     C                                                %trim(wQFT)         +pCSV+
085500170828     C                                                %trim(wPOR)         +pCSV+
085600170828     C                                       pDLM  +%trim(VAFSV1)+pDLM    +pCSV+
085700170828     C                                                %trim(wVA1)         +pCSV+
085800170828     C                                       pDLM  +%trim(VAFSV2)+pDLM    +pCSV+
085900170828     C                                                %trim(wVA2)         +pCSV+
086000170828     C                                       pDLM  +%trim(VAFSV3)+pDLM    +pCSV+
086100170828     C                                                %trim(wVA3)         +pCSV+
086200170828     C                                       pDLM  +%trim(VAFSV4)+pDLM    +pCSV+
086300170828     C                                                %trim(wVA4)         +pCSV+
086400170828     C                                       pDLM  +%trim(VAFSV5)+pDLM    +pCSV+
086500170828     C                                                %trim(wVA5)         +pCSV+
086600170828     C                                       pDLM  +%trim(VAFSV6)+pDLM    +pCSV+
086700170828     C                                                %trim(wVA6)         +pCSV+
086800170828     C                                       pDLM  +%trim(VAFSV7)+pDLM    +pCSV+
086900170828     C                                                %trim(wVA7)         +pCSV+
087000170828     C                                       pDLM  +%trim(VAFSV8)+pDLM    +pCSV+
087100170828     C                                                %trim(wVA8)         +pCSV+
087200170828     C                                                %trim(wVAX)         +pCSV+
087300170828     C                                                %trim(wIMV)         +pCSV+
087400170828     C                                       %trim(%editc(VAFDFT:'Q'))    +pCSV+
087500170828     C                                       %trim(%editc(VAFNFT:'Q'))    +pCSV+
087600170828     C                                       %trim(%editc(VAFFIV:'Q'))    +pCSV+
087700170828     C                                       pDLM  +%trim(VAFDIV)+pDLM    +pCSV+
087800170828     C                                       %trim(%editc(VAFDRT:'Q'))    +pCSV+
087900170828     C                                       %trim(%editc(VAFRMN:'Q'))    +pCSV+
088000170828     C                                       pDLM  +%trim(VAFRMA)+pDLM    +pCSV+
088100170828     C                                       pDLM  +%trim(VAFRMX)+pDLM    +pCSV+
088200170828     C                                       pDLM  +%trim(VAFFTC)+pDLM    +pCSV+
088300170828     C                                       pDLM  +%trim(VAFTC2)+pDLM    +pCSV+
088400170828     C                                       pDLM  +%trim(VAFTSP)+pDLM    +pCSV+
088500170828     C                                       pDLM  +%trim(VAFFAP)+pDLM    +pCSV+
088600170828     C                                       pDLM  +%trim(VAFFIN)+pDLM    +pCSV+
088700170828     C                                       %trim(%editc(VAFCTR:'Q'))    +pCSV+
088800170828     C                                       pDLM  +%trim(VAFNAS)+pDLM    +pCSV+
088900170828     C                                       pDLM  +%trim(VAFCTS)+pDLM    +pCSV+
089000170828     C                                       pDLM  +%trim(VAFCAD)+pDLM    +pCSV+
089100170828     C                                       pDLM  +%trim(VAFLOD)+pDLM    +pCSV+
089200170828     C                                       pDLM  +%trim(VAFPRD)+pDLM    +pCSV+
089300170828     C                                       pDLM  +%trim(VAFNZD)+pDLM    +pCSV+
089400170828     C                                       pDLM  +%trim(VAFVAS)+pDLM    +pCSV+
089500170828     C                                                %trim(wIAS)         +pCSV+
089600170828     C                                       pDLM  +%trim(VAFFPC)+pDLM    +pCSV+
089700170828     C                                                %trim(wPKC)         +pCSV+
089800170828     C                                       pDLM  +%trim(VAFFVC)+pDLM    +pCSV+
089900170828     C                                                %trim(wVLC)
090000161214     C*
090100161214     C                   if        iEXT
090200161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
090300161214     C                                       pDLM  +%trim(VHFRSD)+pDLM    +pCSV+
090400161214     C                                       pDLM  +%trim(VHFIND)+pDLM    +pCSV+
090500161214     C                                       pDLM  +%trim(VHFRSM)+pDLM    +pCSV+
090600161214     C                                       pDLM  +%trim(VHFINM)+pDLM    +pCSV+
090700161214     C                                       pDLM  +%trim(VHFCAM)+pDLM    +pCSV+
090800161214     C                                       pDLM  +%trim(VHFLOM)+pDLM    +pCSV+
090900161214     C                                       pDLM  +%trim(VHFPRM)+pDLM    +pCSV+
091000161214     C                                       pDLM  +%trim(VHFNZM)+pDLM    +pCSV+
091100161214     C                                       pDLM  +%trim(VHFRMO)+pDLM    +pCSV+
091200161214     C                                       pDLM  +%trim(VHFCMO)+pDLM    +pCSV+
091300170118     C                                       pDLM  +%trim(VHFNMO)+pDLM
091400161214     C                   endif
091500170130     C*
091600170130     C                   if        iPVL
091700170130     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
091800170130     C                                                %trim(wPVL)
091900170130     C                   endif
092000170118     C*
092100170118     C                   if        iDCM
092200170118     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
092300170118     C                                                %trim(wDCM)         +pCSV+
092400170118     C                                       pDLM    +%trim(wCCA)+pDLM
092500170118     C                   endif
092600161214     C*
092700161214     C                   if        iFAT
092800161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
092900161214     C                                       pDLM  +%trim(§AR5FPTAS)+pDLM +pCSV+
093000161214     C                                              %trim(wPKTAS)         +pCSV+
093100161214     C                                       pDLM  +%trim(§AR5FVTAS)+pDLM +pCSV+
093200161214     C                                              %trim(wVLTAS)
093300161214     C                   endif
093400161214     C*
093500161214     C                   if        iSEGN
093600161214     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
093700161214     C                                       pDLM  +%trim(wNNC)+pDLM
093800161214     C                   ENDIF
093900930409     C*
094000161214     C                   write     TIVAW000
094100991027     C*
094200161214     C                   read      tivgdtmp
094300161214     C                   enddo
094400991027     C*
094500161214     C                   eval      wrkesito = '0'
094600991027     C*
094700910830     C                   ENDSR
094800161214
094900161214
095000161214
095100161214     C     inzVAR        BEGSR
095200161214     C*
095300161214     C* Inizializzazioni campi "dati" di wrk
095400161214     C                   clear                   wPKB
095500161214     C                   clear                   wVLF
095600161214     C                   clear                   wQFT
095700161214     C                   clear                   wPOR
095800161214     C                   clear                   wVA1
095900161214     C                   clear                   wVA2
096000161214     C                   clear                   wVA3
096100161214     C                   clear                   wVA4
096200161214     C                   clear                   wVA5
096300161214     C                   clear                   wVA6
096400161214     C                   clear                   wVA7
096500161214     C                   clear                   wVA8
096600161214     C                   clear                   wVAX
096700161214     C                   clear                   wIMV
096800161214     C                   clear                   wIAS
096900161214     C                   clear                   wPKC
097000161214     C                   clear                   wVLC
097100161214     C                   clear                   wNNC
097200161214     C                   clear                   wPVL
097300161214     C                   clear                   wDCM
097400161214     C                   clear                   wCCA
097500170828     C                   clear                   wIDSPED
097600161214     C*
097700161214     C* Inizializzo i campi "estensione"
097800161214     C                   clear                   VHFRSD
097900161214     C                   clear                   VHFIND
098000161214     C                   clear                   VHFRSM
098100161214     C                   clear                   VHFINM
098200161214     C                   clear                   VHFCAM
098300161214     C                   clear                   VHFLOM
098400161214     C                   clear                   VHFPRM
098500161214     C                   clear                   VHFNZM
098600161214     C                   clear                   VHFRMO
098700161214     C                   clear                   VHFCMO
098800161214     C                   clear                   VHFNMO
098900161214     C                   z-add     *zeros        PVL               7 1
099000161214     C                   clear                   wNNC
099100161214     C                   clear                   wPKTAS
099200161214     C                   clear                   wVLTAS
099300161214     C                   clear                   DAR5FAT
099400161214     C*
099500161214     C                   ENDSR
099600161214
099700161214
099800161214
099900161214     C     RTVAR5FAT     BEGSR
100000161214     C*
100100161214     C                   EVAL      ar5TRD = 'FAT'
100200161214     C                   CLEAR                   DAR5FAT
100300161214     C     KEYar531C_P   CHAIN     FIAR531C
100400161214     C                   IF        %found(FIAR531C)
100500161214     C                   MOVEL     AR5UNI        DAR5FAT
100600161214     C                   ENDIF
100700161214     C*
100800161214     C                   ENDSR
100900161214
101000161214
101100161214
101200161214     C     RTVSEGN       BEGSR
101300161214     C*
101400161214     C                   SETOFF                                       70
101500161214     C*
101600161214     C* Aggancio il dettaglio colli
101700161214     C                   EVAL      tahTRC = 'C'
101800161214     C     KEYtah30_C    SETLL     titah30c
101900161214     C                   IF        %equal(titah30c)
102000161214     C     KEYtah30_C    READE     titah30c
102100161214     C                   DOW       not %eof(titah30c) AND not *IN70
102200161214     C                   EVAL      wNNC = tahNOT
102300161214     C                   SETON                                        70
102400161214     C     KEYtah30_C    READE     titah30c
102500161214     C                   ENDDO
102600161214     C                   ENDIF
102700161214     C*
102800161214     C                   ENDSR
102900161214
103000161214
103100991027
103200991027      /TITLE Operazioni iniziali.
103300991027     C     *inzsr        BEGSR
103400991027     C*
103500991027     C     *ENTRY        PLIST
103600991027     C                   parm                    prmppt
103700991027     C     wrkesito      parm      wrkesito      prmesito
103800030709     C*
103900030709     C* Definizione chiavi
104000030709     C*
104100030709     C* Chiave su TITAS30C - Completa
104200030709     C     KEYtas30C     KLIST
104300030709     C                   KFLD                    vafAAS
104400030709     C                   KFLD                    vafLNP
104500030709     C                   KFLD                    vafNRS
104600030709     C                   KFLD                    vafNSP
104700030709     C                   KFLD                    vafTBL
104800030709     C*
104900030709     C* Chiave su TITAA30C - Completa
105000030709     C     KEYtaa30C     KLIST
105100030709     C                   KFLD                    vafAAS
105200030709     C                   KFLD                    vafLNP
105300030709     C                   KFLD                    vafNRS
105400030709     C                   KFLD                    vafNSP
105500030709     C                   KFLD                    taaTRC
105600161214     C*
105700161214     C* Chiave su FIAR531C - Completa
105800161214     C     KEYar531C_P   KLIST
105900161214     C                   KFLD                    vafAAS
106000161214     C                   KFLD                    vafLNP
106100161214     C                   KFLD                    vafNRS
106200161214     C                   KFLD                    vafNSP
106300161214     C                   KFLD                    ar5TRD
106400161214     C*
106500161214     C* Chiave su TITAH30C - completa
106600161214     C     KEYtah30_C    KLIST
106700161214     C                   KFLD                    vafAAS
106800161214     C                   KFLD                    vafLNP
106900161214     C                   KFLD                    vafNRS
107000161214     C                   KFLD                    vafNSP
107100161214     C                   KFLD                    tahTRC
107200991027     C*
107300991027     C                   ENDSR
107400161214
