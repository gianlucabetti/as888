000100170124     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400170124     Ftivgdtmp  if   E             DISK
000500170124     Ftivawwwt  uf a E             DISK    commit
000600170124     Ftitah30c  if   E           K DISK
000700991027
000800170124     D tnvasDS       e ds                  extname(tnvas00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200170124     D wNNC            s             35    inz
001300170124     D wCAS            s             14    inz
001400170124     D wDTE            s             10    inz
001500170124     D wDDP            s             10    inz
001600170124     D wDVA            s             10    inz
001700170124     D pCSV            s              1    inz
001800170124     D pDLM            s              1    inz varying
001900170124
002000170124     D iINTI           s               N   inz
002100170124     D iINTD           s               N   inz
002200170124     D iINTE           s               N   inz
002300170124     D iSEGN           s               N   inz
002400170124     D iDAT            s               N   inz
002500060720
002600991027
002700170124
002800170124     C*
002900170124     C* Avvio il monitoring del intero flusso
003000170124     C                   monitor
003100170124     C*
003200170124     C* Verifica esistenza record nel membro di output
003300170124     C                   EXSR      chkRECFILMBR
003400170124     C*
003500170124     C* Effettuo traduzione
003600991027     C                   EXSR      traduci
003700170124     C*
003800170124     C* Gestisco eventuale errore
003900170124     C                   on-error
004000170124     C*
004100170124     C* Imposto esito transazione a errore
004200170124     C                   EVAL      wrkesito = '2'
004300170124     C*
004400170124     C* Se errore => emetto operazione d ROLLBACK
004500170124     C                   ROLBK
004600170124     C*
004700170124     C* Arresto il monitoring
004800170124     C                   endmon
004900170124     C*
005000921023     C                   SETON                                        LR
005100150929
005200150929
005300030908
005400030908     C     chkRECFILMBR  BEGSR
005500030908     C*
005600100610     C                   READ      TIVAWWWT                               55
005700030908     C*
005800030908     C                   ENDSR
005900150929
006000150929
006100991027
006200991027     C     traduci       BEGSR
006300170124     C*
006400170124     C* Inizializzazione indicatori di procedure per parametri input
006500170124     C                   eval      iINTI = *off
006600170124     C                   eval      iINTD = *off
006700170124     C                   eval      iINTE = *off
006800170124     C                   eval      iSEGN = *off
006900170124     C                   eval      iDAT  = *off
007000170124     C*
007100170124     C* Se richiesta intestazione
007200170124     C                   select
007300170124     C                   when      %subst(prmppt:1:1) = 'I'
007400170124     C                   eval      iINTI = *on
007500170124     C                   when      %subst(prmppt:1:1) = 'D'
007600170124     C                   eval      iINTD = *on
007700170124     C                   when      %subst(prmppt:1:1) = 'E'
007800170124     C                   eval      iINTE = *on
007900170124     C                   endsl
008000170124     C*
008100170124     C* Verifico il carattere delimitatore test indicato nei parametri del cliente/tipo file
008200170124     C                   select
008300170124     C                   when      %subst(prmppt:2:1) = *blanks
008400170124     C                   eval      pDLM = '"'
008500170124     C                   when      %subst(prmppt:2:1) = 'N'
008600170124     C                   eval      pDLM = ''
008700170124     C                   other
008800170124     C                   eval      pDLM = %subst(prmppt:2:1)
008900170124     C                   endsl
009000170124     C*
009100170124     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
009200170124     C                   eval      pCSV = %subst(prmppt:3:1)
009300170124     C                   if        pCSV = *blanks
009400170124     C                   eval      pCSV = ','
009500170124     C                   endif
009600170124     C*
009700170124     C* Verifico se richiesto un particolare carattere d separatore decimale
009800170124     C                   if        %subst(prmppt:4:1) = ','
009900170124     C                   setoff                                       60
010000170124     C                   else
010100170124     C                   seton                                        60
010200170124     C                   endif
010300170124     C*
010400170124     C* Se richiesti dati di estensione relativi ai segnacolli
010500170124     C                   if        %subst(prmppt:5:1) = 'S'
010600170124     C                   eval      iSEGN = *on
010700170124     C                   endif
010800170124     C*
010900170124     C* Verifico se richiesta formatttazione data (con barre)
011000170124     C                   if        %subst(prmppt:6:1) = 'D'
011100170124     C                   eval      iDAT = *on
011200170124     C                   else
011300170124     C                   eval      iDAT = *off
011400170124     C                   endif
011500170124     C*
011600170124     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
011700170124     C                   if        (iINTI OR iINTD OR iINTE) AND *in55 = *on
011800170124     C                   if        iINTI
011900170124     C                   eval      VAWDTA  =
012000170124     C                             pDLM+'VASLNP'                     +pDLM+pCSV+
012100170124     C                             pDLM+'VASLNA'                     +pDLM+pCSV+
012200170124     C                             pDLM+'VASAAS'                     +pDLM+pCSV+
012300170124     C                             pDLM+'VASNRS'                     +pDLM+pCSV+
012400170124     C                             pDLM+'VASNSP'                     +pDLM+pCSV+
012500170124     C                             pDLM+'VASRMN'                     +pDLM+pCSV+
012600170124     C                             pDLM+'VASCAS'                     +pDLM+pCSV+
012700170124     C                             pDLM+'VASVCA'                     +pDLM+pCSV+
012800170124     C                             pDLM+'VASNRA'                     +pDLM+pCSV+
012900170124     C                             pDLM+'VASDTE'                     +pDLM+pCSV+
013000170124     C                             pDLM+'VASTPI'                     +pDLM+pCSV+
013100170124     C                             pDLM+'VASABI'                     +pDLM+pCSV+
013200170124     C                             pDLM+'VASCAI'                     +pDLM+pCSV+
013300170124     C                             pDLM+'VASNDP'                     +pDLM+pCSV+
013400170124     C                             pDLM+'VASDDP'                     +pDLM+pCSV+
013500170124     C                             pDLM+'VASCCB'                     +pDLM+pCSV+
013600170124     C                             pDLM+'VASABP'                     +pDLM+pCSV+
013700170124     C                             pDLM+'VASCAP'                     +pDLM+pCSV+
013800170124     C                             pDLM+'VASFPC'                     +pDLM+pCSV+
013900170124     C                             pDLM+'VASDVA'                     +pDLM+pCSV+
014000170124     C                             pDLM+'VASKSC'                     +pDLM+pCSV+
014100170124     C                             pDLM+'VASRMA'                     +pDLM
014200170124     C*
014300170124     C                   if        iSEGN
014400170124     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
014500170124     C                             pDLM+'SEGNAC.'                    +pDLM
014600170124     C                   endif
014700170124     C                   endif
014800170124     C*
014900170124     C                   if        iINTD
015000170124     C                   eval      VAWDTA  =
015100170124     C                             pDLM+'FILIALE PARTENZA'           +pDLM+pCSV+
015200170124     C                             pDLM+'FILIALE ARRIVO'             +pDLM+pCSV+
015300170124     C                             pDLM+'ANNO SPEDIZIONE'            +pDLM+pCSV+
015400170124     C                             pDLM+'NUMERO SERIE'               +pDLM+pCSV+
015500170124     C                             pDLM+'NUMERO SPEDIZIONE'          +pDLM+pCSV+
015600170124     C                             pDLM+'RIFERIM. MITTENTE NUMERICO' +pDLM+pCSV+
015700170124     C                             pDLM+'CONTRASSEGNO'               +pDLM+pCSV+
015800170124     C                             pDLM+'DIVISA IMPORTO C/ASSEGNO'   +pDLM+pCSV+
015900170124     C                             pDLM+'NR. RIF. ASSEGNO INCASSATO' +pDLM+pCSV+
016000170124     C                             pDLM+'DATA ASSEGNO INCASSATO'     +pDLM+pCSV+
016100170124     C                             pDLM+'TIPO INTEST. M=MITTENTE'    +pDLM+pCSV+
016200170124     C                             pDLM+'COD. ABI ASSEGNO INC.'      +pDLM+pCSV+
016300170124     C                             pDLM+'COD. CAB ASSEGNO INC.'      +pDLM+pCSV+
016400170124     C                             pDLM+'NR. DOCUM.PAGAM. C/ASSEGNO' +pDLM+pCSV+
016500170124     C                             pDLM+'DATA PAGAMENTO C/ASSEGNO'   +pDLM+pCSV+
016600170124     C                             pDLM+'C/C BONIFICO CONTRASSEGNO'  +pDLM+pCSV+
016700170124     C                             pDLM+'COD. ABI PAGAM.'            +pDLM+pCSV+
016800170124     C                             pDLM+'COD. CAB PAGAM.'            +pDLM+pCSV+
016900170124     C                             pDLM+'TIPO PAGAM. C/A'            +pDLM+pCSV+
017000170124     C                             pDLM+'DATA VALUTA'                +pDLM+pCSV+
017100170124     C                             pDLM+'CODICE CLIENTE MITTENTE'    +pDLM+pCSV+
017200170124     C                             pDLM+'RIFERIM. MITTENTE ALFANUM.' +pDLM
017300170124     C*
017400170124     C                   if        iSEGN
017500170124     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
017600170124     C                             pDLM+'ID SEGNACOLLO'              +pDLM
017700170124     C                   endif
017800170124     C                   endif
017900170124     C*
018000170124     C                   if        iINTE
018100170124     C                   eval      VAWDTA  =
018200170124     C                             pDLM+'DEPARTURE DEPOT CODE'       +pDLM+pCSV+
018300170124     C                             pDLM+'ARRIVAL DEPOT CODE'         +pDLM+pCSV+
018400170124     C                             pDLM+'YEAR OF SHIPMENT'           +pDLM+pCSV+
018500170124     C                             pDLM+'SERIAL NUMBER'              +pDLM+pCSV+
018600170124     C                             pDLM+'SHIPMENT NUMBER'            +pDLM+pCSV+
018700170124     C                             pDLM+'NUMERICAL SENDER REF.'      +pDLM+pCSV+
018800170124     C                             pDLM+'C.O.D. AMOUNT'              +pDLM+pCSV+
018900170124     C                             pDLM+'C.O.D. SUM CURRENCY'        +pDLM+pCSV+
019000170124     C                             pDLM+'CASHED CHEQUE REF. NO.'     +pDLM+pCSV+
019100170124     C                             pDLM+'CASHED CHEQUE DATE'         +pDLM+pCSV+
019200170124     C                             pDLM+'HEADING TYPE M=SENDER'      +pDLM+pCSV+
019300170124     C                             pDLM+'ABI BANK COORDINATES CODE'  +pDLM+pCSV+
019400170124     C                             pDLM+'CAB BANK COORDINATES CODE'  +pDLM+pCSV+
019500170124     C                             pDLM+'NR. C.O.D. PAYMENT DOC.'    +pDLM+pCSV+
019600170124     C                             pDLM+'C.O.D. PAYMENT DATE'        +pDLM+pCSV+
019700170124     C                             pDLM+'C.O.D. BANK TRANSFER C/ACC' +pDLM+pCSV+
019800170124     C                             pDLM+'ABI CODE PAYMENT'           +pDLM+pCSV+
019900170124     C                             pDLM+'CAB PAYMENT CODE'           +pDLM+pCSV+
020000170124     C                             pDLM+'C.O.D. PAYMENT TYPE'        +pDLM+pCSV+
020100170124     C                             pDLM+'VALIDITY DATE'              +pDLM+pCSV+
020200170124     C                             pDLM+'SENDER CLIENT CODE'         +pDLM+pCSV+
020300170124     C                             pDLM+'ALPHABETICAL SENDER REF.'   +pDLM
020400170124     C*
020500170124     C                   if        iSEGN
020600170124     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
020700170124     C                             pDLM+'PARCEL ID'                  +pDLM
020800170124     C                   endif
020900170124     C                   endif
021000170124     C*
021100170124     C                   write     TIVAW000
021200170124     C                   endif
021300170124     C*
021400170124     C* Scorro tutto il file
021500170124     C                   read      tivgdtmp
021600170124     C                   dow       not %eof(tivgdtmp)
021700170124     C*
021800170124     C* Inizializzo buffer output
021900170124     C                   clear                   TIVAW000
022000170124     C*
022100170124     C* Valorizzo dati "correnti"
022200170124     C                   movel     vgddta        tnvasDS
022300170124     C*
022400170124     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
022500170124     C  N60              EVAL      wCAS = %trim(%editw(VASCAS:'        0 ,   '))
022600170124     C   60              EVAL      wCAS = %trim(%editw(VASCAS:'        0 .   '))
022700170124     C*
022800170124     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
022900170124     C                   if        iDAT
023000170124     C                   eval      wDTE = %trim(%editw(VASDTE:'    /  /  '))
023100170124     C                   eval      wDDP = %trim(%editw(VASDDP:'    /  /  '))
023200170124     C                   eval      wDVA = %trim(%editw(VASDVA:'    /  /  '))
023300170124     C                   else
023400170124     C                   eval      wDTE = %trim(%editc(VASDTE:'Q'))
023500170124     C                   eval      wDDP = %trim(%editc(VASDDP:'Q'))
023600170124     C                   eval      wDVA = %trim(%editc(VASDVA:'Q'))
023700170124     C                   endif
023800170124     C*
023900170124     C                   EVAL      VAWDTA  =
024000170124     C                                      %trim(%editc(VASLNP:'Q'))     +pCSV+
024100170124     C                                      %trim(%editc(VASLNA:'Q'))     +pCSV+
024200170124     C                                      %trim(%editc(VASAAS:'Q'))     +pCSV+
024300170124     C                                      %trim(%editc(VASNRS:'Q'))     +pCSV+
024400170124     C                                      %trim(%editc(VASNSP:'Q'))     +pCSV+
024500170124     C                                      %trim(%editc(VASRMN:'Q'))     +pCSV+
024600170124     C                                             %trim(wCAS)            +pCSV+
024700170124     C                             pDLM+           %trim(VASVCA)     +pDLM+pCSV+
024800170124     C                             pDLM+           %trim(VASNRA)     +pDLM+pCSV+
024900170124     C                                             %trim(wDTE)            +pCSV+
025000170124     C                             pDLM+           %trim(VASTPI)     +pDLM+pCSV+
025100170124     C                                      %trim(%editc(VASABI:'Q'))     +pCSV+
025200170124     C                                      %trim(%editc(VASCAI:'Q'))     +pCSV+
025300170124     C                                      %trim(%editc(VASNDP:'Q'))     +pCSV+
025400170124     C                                             %trim(wDDP)            +pCSV+
025500170124     C                             pDLM+           %trim(VASCCB)     +pDLM+pCSV+
025600170124     C                                      %trim(%editc(VASABP:'Q'))     +pCSV+
025700170124     C                                      %trim(%editc(VASCAP:'Q'))     +pCSV+
025800170124     C                             pDLM+           %trim(VASFPC)     +pDLM+pCSV+
025900170124     C                                             %trim(wDVA)            +pCSV+
026000170124     C                                      %trim(%editc(VASKSC:'Q'))     +pCSV+
026100170124     C                             pDLM+           %trim(VASRMA)     +pDLM
026200170124     C*
026300170124     C* Se richiesto nei parametri di ingresso ritorno anche i dati del segnacollo cliente
026400170124     C                   if        iSEGN
026500170124     C                   setoff                                       70
026600170124     C                   clear                   wNNC
026700170124     C*
026800170124     C* Aggancio il dettaglio colli
026900170124     C                   eval      tahTRC = 'C'
027000170124     C     KEYtah30_C    setll     titah30c
027100170124     C                   if        %equal(titah30c)
027200170124     C     KEYtah30_C    reade     titah30c
027300170124     C                   dow       not %eof(titah30c) AND not *in70
027400170124     C                   eval      wNNC = tahNOT
027500170124     C                   seton                                        70
027600170124     C     KEYtah30_C    reade     titah30c
027700170124     C                   enddo
027800170124     C                   endif
027900170124     C*
028000170124     C                   eval      VAWDTA  = %trim(VAWDTA)                +pCSV+
028100170124     C                             pDLM+           %trim(wNNC)       +pDLM
028200170124     C                   endif
028300170124     C*
028400170124     C* Infine scarico il buffer di output
028500170124     C                   write     TIVAW000
028600170124     C*
028700170124     C                   read      tivgdtmp
028800170124     C                   enddo
028900170124     C*
029000170124     C                   EVAL      wrkesito = '0'
029100170124     C*
029200170124     C                   ENDSR
029300150929
029400150929
029500991027
029600991027      /TITLE Operazioni iniziali.
029700991027     C     *inzsr        BEGSR
029800991027     C*
029900991027     C     *ENTRY        PLIST
030000991027     C                   parm                    prmppt
030100991027     C     wrkesito      parm      wrkesito      prmesito
030200150929     C*
030300150929     C* Chiave su TITAH30C - completa
030400150929     C     KEYtah30_C    KLIST
030500150929     C                   KFLD                    vasAAS
030600150929     C                   KFLD                    vasLNP
030700150929     C                   KFLD                    vasNRS
030800150929     C                   KFLD                    vasNSP
030900150929     C                   KFLD                    tahTRC
031000991027     C*
031100991027     C                   ENDSR
