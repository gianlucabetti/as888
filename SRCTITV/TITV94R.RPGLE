000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400040216     FTABEL00F  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600040205     FTNCSB03L  IF   E           K DISK
000700090323     FTISTB02L  UF   E           K DISK    commit
000800090323     FTIVAWWWT  UF A E             DISK    commit
000900030924
001000031103     D prmppi          S             50
001100030924     D prmesito        S              1
001200030924     D wrkesito        S                   like(prmesito)
001300040205     D wNum5_0         S              5  0
001400040205     D wNum7_2         S              7  2
001500040205     D wNum11_2        S             11  2
001600040305     D wCausaleNoOK    S              4    varying inz(*all'0')
001700040216
001800040216     D ds2a          e ds
001900080903     D sk2Agiac        s              3    dim(200)
002000031103
002100031103     D*-----------
002200031103     D* VARIABILI RIFERITE AL DATA-BASE
002300031103     D*-----------
002400031104     D wKstbKSU        S                   like(stbKSU)
002500031103     D wKstbFTR        S                   like(stbFTR)
002600030924
002700030924     D*-----------
002800030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
002900030924     D*-----------
003000030925     D C_DS_Cli        DS
003100040205     D  C_IDRec                       1    INZ(*blanks)
003200040205     D  C_IDCons                     10    INZ(*blanks)
003300040205     D  C_DataEsito                   8    INZ(*blanks)
003400040205     D  C_OraEsito                    6    INZ(*blanks)
003500040205     D  C_CodEsito                    1    INZ(*blanks)
003600040205     D  C_CodTerzista                10    INZ(*blanks)
003700040205     D  C_DataSped                    8    INZ(*blanks)
003800040205     D  C_DataCons                    8    INZ(*blanks)
003900040302     D  C_CausaleNoOK                 4    INZ('0000')
004000040205     D  C_CodPiatTerz                 4    INZ(*blanks)
004100040216     D  C_PesoNetto                   8    INZ('00000.00')
004200040216     D  C_PesoTot                     8    INZ('00000.00')
004300040216     D  C_Contrass                   12    INZ('000000000.00')
004400040205     D  C_CodMaterial                11    INZ(*blanks)
004500040216     D  C_NumColli                    5    INZ(*zeros)
004600040205     D  C_CodEsitoDet                 2    INZ(*blanks)
004700031103
004800031103     D*-----------
004900031103     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
005000031103     D*-----------
005100031103     D                 DS
005200031103     D  TASAAS                 1      4  0
005300031103     D  TASMGS                 5      8  0
005400031103     D DS_DATABOLLA            1      8
005500031103
005600031103     D*-----------
005700031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
005800031103     D*-----------
005900031103     D DSPPI           DS
006000031104     D  w§PPIKSU                      7  0
006100030924
006200030325
006300030924     C* MAIN/
006400040216     C                   EXSR      cartab
006500991027     C                   EXSR      traduci
006600921023     C                   SETON                                        LR
006700991027
006800030924
006900030924
007000991027     C     traduci       BEGSR
007100031103     C*
007200031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
007300031103     C* di traduzione (trattasi d "post-traduzione")
007400031104     C                   EVAL      wKstbKSU = w§PPIKSU
007500031103     C                   EVAL      wKstbFTR = *blanks
007600031103     C     KEYstb02P     SETLL     tistb02l
007700031103     C                   IF        %equal(tistb02l)
007800031103     C     KEYstb02P     READE     tistb02l
007900031103     C                   SETOFF                                       50
008000991027     C*
008100040205     C* Ciclo x tutti i record del cliente da trasmettere
008200031103     C                   DOW       not %eof(tistb02l)
008300031014     C*
008400030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
008500031014     C                   RESET                   C_DS_Cli
008600031103     C*
008700031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
008800031103     C* deve restituire al cliente nel file tradotto
008900031103     C                   IF        stbFTR = *blanks                             * ridondante
009000031103     C*
009100031103     C* Innanzitutto chaino la bolla direttamente su TITAS
009200031103     C     KEYtas30P     CHAIN     titas30c
009300031103     C                   IF        %found(titas30c)
009400031103     C*
009500040205     C* Gestisco unicamente gli stati "standard"
009600031103     C                   IF        stbTIS = '1'
009700040205     C*
009800040205     C* X stati provenienti dagli eventi
009900040205     C                   IF        stbPRS = '2'
010000040205     C*
010100040205     C* Eventi d NN consegna (esito cliente = '3')
010200040216     C* Per lo stato cliente '3' considero solo gli eventi d tipo giacenza
010300040216     C                   MOVEL     stbCOS        wCOS              3
010400040216     C     wCOS          LOOKUP    sk2Agiac                               20
010500040216     C                   IF        *IN20 = *ON
010600040205     C* Valorizzo buffer record ritorno al cliente
010700040205     C                   EVAL      C_CodEsito = '3'
010800040305     C                   EVAL      wCausaleNoOK = %trim(stbCOS)
010900040305     C                   MOVE      wCausaleNoOK  C_CausaleNoOK
011000040205     C                   EXSR      WRIREC
011100040216     C                   ENDIF
011200040216     C                   ENDIF
011300040216     C*
011400040205     C* X stati provenienti dagli esiti
011500040205     C                   IF        stbPRS = '4'
011600040205     C*
011700040205     C* CONSEGNA OK (esito cliente = '7')
011800040205     C                   IF        stbCOS = 'CONSOK'
011900040205     C* Valorizzo buffer record ritorno al cliente
012000040205     C                   EVAL      C_CodEsito = '7'
012100040205     C                   MOVE      tasDCM        C_DataCons
012200040205     C* Se la bolla ha il contrassegno lo recupero dal file contrassegni (TNCSB)
012300040205     C                   IF        tasFCA = 'S'
012400040205     C     KEYtas30P     CHAIN     tncsb03l
012500040205     C                   IF        %found(tncsb03l)
012600040205     C                   Z-ADD     csbCAS        wNum11_2
012700040205     C                   EVAL      C_Contrass =
012800040216     C                             %editw(wNum11_2:'         .  ')
012900040205     C                   ENDIF
013000040205     C                   ENDIF
013100040205     C                   Z-ADD     tasNCL        wNum5_0
013200040205     C                   EVAL      C_NumColli =
013300040205     C                             %editw(wNum5_0:'     ')
013400040216     C                   Z-ADD     tasPKB        wNum7_2
013500040305     C                   EVAL      C_PesoNetto   = %editw(wNum7_2:'     .  ')
013600040205     C                   EXSR      WRIREC
013700040205     C                   ENDIF
013800040205     C*
013900040205     C* RESO (esito cliente = '1')
014000040205     C                   IF        stbCOS = 'RESO'
014100040322     C* Scrivo il record d reso solo se il n° distinta consegna sulla bolla è <> 9999999
014200040322     C                   IF        tasNDC <> *all'9'
014300040205     C* Valorizzo buffer record ritorno al cliente
014400040205     C                   EVAL      C_CodEsito = '1'
014500040205     C                   EXSR      WRIREC
014600040322     C                   ENDIF
014700040205     C                   ENDIF
014800040205     C                   ENDIF
014900040210     C                   ENDIF
015000031103     C*
015100031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
015200031103     C                   EVAL      stbFTR = 'T'
015300031103     C                   UPDATE    tistb000
015400031103     C                   ENDIF
015500040205     C                   ENDIF
015600991027     C*
015700031103     C     KEYstb02P     READE     tistb02l
015800031103     C                   SETOFF                                       50
015900030325     C                   ENDDO
016000031103     C                   ENDIF
016100991027     C*
016200030325     C                   EVAL      wrkesito = '0'
016300991027     C*
016400910830     C                   ENDSR
016500031103
016600031103
016700031103
016800031103     C     wrirec        BEGSR
016900031103     C*
017000040205     C                   EVAL      C_IDRec       = 'T'
017100150629     C***                MOVE      tasRMN        C_IDCons
017200150701     C                   MOVEL     stbRMA        C_IDCons
017300040205     C                   MOVE      stbDAS        C_DataEsito
017400040216     C                   MOVE      *all'0'       C_OraEsito
017500040216     C                   MOVEL     stbORS        C_OraEsito
017600160218     C***                EVAL      C_CodTerzista = '00212142'
017700160218     C                   EVAL      C_CodTerzista = '0000307126'
017800040205     C                   EVAL      C_DataSped    = DS_DATABOLLA
017900031103     C*
018000031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
018100031103     C                   EVAL      VAWDTA = C_DS_Cli
018200040302     C                   EVAL      %subst(VAWDTA:220:1) = '*'
018300031103     C                   WRITE     TIVAW000
018400031103     C*
018500031103     C                   ENDSR
018600040216
018700040216
018800040216     C     CARTAB        BEGSR
018900040216     C*
019000040216     C                   clear                   sk2Agiac
019100040216     C                   z-add     *zeros        i                 3 0
019200040216     C                   eval      tblKUT = 1
019300040216     C                   eval      tblCOD = '2A'
019400040216     C     KEYtabP       setll     tabel00f
019500040216     C     KEYtabP       reade     tabel00f
019600040216     C                   dow       not %eof(tabel00f)
019700040216     C                   movel(p)  tblUNI        ds2a
019800040216     C                   if        §2aFTC = 'G'
019900040216     C                   add       1             i
020000040216     C                   eval      sk2Agiac(i) = tblKEY
020100040216     C                   endif
020200040216     C     KEYtabP       reade     tabel00f
020300040216     C                   enddo
020400040216     C*
020500040216     C                   ENDSR
020600031103
020700031103
020800991027
020900991027      /TITLE Operazioni iniziali.
021000991027     C     *inzsr        BEGSR
021100991027     C*
021200991027     C     *ENTRY        PLIST
021300031103     C                   parm                    prmppi
021400991027     C     wrkesito      parm      wrkesito      prmesito
021500031103     C*
021600031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
021700031103     C                   EVAL      DSPPI = prmppi
021800030709     C*
021900030709     C* Definizione chiavi
022000030924     C*
022100031103     C* Chiave su TISTB02L - Parziale
022200031103     C     KEYstb02P     KLIST
022300031104     C                   KFLD                    wKstbKSU
022400031103     C                   KFLD                    wKstbFTR
022500031103     C*
022600031103     C* Chiave su TITAS30C - Parziale
022700031103     C     KEYtas30P     KLIST
022800031103     C                   KFLD                    stbAAS
022900031103     C                   KFLD                    stbLNP
023000031103     C                   KFLD                    stbNRS
023100031103     C                   KFLD                    stbNSP
023200040216     C*
023300040216     C* Chiave su TABEL00F - parziale
023400040216     C     KEYtabP       KLIST
023500040216     C                   KFLD                    tblKUT
023600040216     C                   KFLD                    tblCOD
023700991027     C*
023800991027     C                   ENDSR
