000100081111     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400090128     FTITAS30C  IF   E           K DISK
000500090303     FTABEL00F  IF   E           K DISK
000600090323     FTISTB02L  UF   E           K DISK    commit
000700090323     FTIVAWWWT  UF A E             DISK    commit
000800031103
000900041015     D*-----------
001000041015     D* VARIABILI D WRK
001100041015     D*-----------
001200031103     D prmppi          S             50
001300030924     D prmesito        S              1
001400030924     D wrkesito        S                   like(prmesito)
001500090128     D wBufferOut      S           2048    varying
001600090303     D DS2Z          E DS                  inz
001700090303     D skEventi        S              3    DIM(999)
001800031103
001900031103     D*-----------
002000031103     D* VARIABILI RIFERITE AL DATA-BASE
002100031103     D*-----------
002200080618     D wKstbKSU        S                   like(stbKSU)
002300080618     D wKstbFTR        S                   like(stbFTR)
002400030924
002500030924     D*-----------
002600030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
002700030924     D*-----------
002800030925     D C_DS_Cli        DS
002900090129     D  C_LDV                        25A   INZ('x')
003000090129     D  C_DataStato                   8A   INZ('0')
003100090129     D  C_Stato                      15A   INZ('x')
003200090129     D  C_Colli                       5A   INZ('0')
003300090129     D  C_NumeroFatt                 15A   INZ('x')
003400090129     D  C_Filler001                   5A   INZ('x')
003500090129     D  C_Filler002                  15A   INZ('x')
003600090129     D  C_LNA                        15A   INZ('0')
003700090129     D  C_OraStato                    6A   INZ('0')
003800090129     D  C_Note                      100A   INZ('x')
003900090129     D  C_DataSped                    8A   INZ('0')
004000090129     D  C_DataIncasso                 8A   INZ('0')
004100090129     D  C_DataCons                    8A   INZ('0')
004200090129     D  C_OraCons                     6A   INZ('0')
004300090129     D  C_RSD                        40A   INZ('x')
004400090128     D  C_PesoRil                     8A   INZ('0000,000')
004500090128     D  C_VolumeRil                   8A   INZ('00000,00')
004600031103
004700031103     D*-----------
004800031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
004900031103     D*-----------
005000031103     D DSPPI           DS
005100080618     D  w§PPIKSU                      7  0
005200090128     D  w§PPISEPFLD                   1
005300090128     D  w§PPIDELTXT                   1
005400080901
005500080901     D*-----------
005600080901     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
005700080901     D*-----------
005800090128     D                 DS
005900090128     D  tasLNP
006000090128     D  tasNRS
006100090128     D  tasNSP
006200090128     D DS_LDV                  1     12A
006300081111
006400081111
006500081111     D*------------------
006600081111     D* LINKING A DEFINIZIONI ESTERNE
006700081111     D*------------------
006800090128     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006900081111     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007000081111
007100081111
007200030924
007300030325
007400030924     C* MAIN/
007500090303     C*
007600090303     C* Caricamento tabelle
007700090303     C                   EXSR      carTAB
007800041015     C*
007900041015     C* Esegue traduzione
008000991027     C                   EXSR      traduci
008100090303     C*
008200921023     C                   SETON                                        LR
008300090303
008400090303
008500090303
008600090303     C     carTAB        BEGSR
008700090303     C*
008800090303     C     KEYtab_P      KLIST
008900090303     C                   KFLD                    tblKUT
009000090303     C                   KFLD                    tblCOD
009100090303     C*
009200090303     C                   eval      tblKUT = 1
009300090303     C                   eval      tblCOD = '2Z'
009400090303     C*
009500090303     C* Carico gli eventi particolari
009600090303     C                   CLEAR                   xx                4 0
009700090303     C     KEYtab_P      SETLL     TABEL00F
009800090303     C     KEYtab_P      READE     TABEL00F
009900090303     C                   DOW       not %eof(TABEL00F)
010000090303     C*
010100090303     C                   MOVEL     TBLUNI        DS2Z
010200090303     C                   IF        §2ZCEP <> *blanks
010300090303     C                   ADD       1             xx
010400090303     C                   MOVEL     TBLKEY        skEventi(xx)
010500090303     C                   ENDIF
010600090303     C*
010700090303     C     KEYtab_P      READE     TABEL00F
010800090303     C                   ENDDO
010900090303     C*
011000090303     C                   ENDSR
011100991027
011200030924
011300030924
011400991027     C     traduci       BEGSR
011500031103     C*
011600031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
011700031103     C* di traduzione (trattasi d "post-traduzione")
011800031104     C                   EVAL      wKstbKSU = w§PPIKSU
011900031103     C                   EVAL      wKstbFTR = *blanks
012000090128     C     KEYstb02P_P   SETLL     tistb02l
012100031103     C                   IF        %equal(tistb02l)
012200090128     C     KEYstb02P_P   READE     tistb02l
012300991027     C*
012400090128     C* Ciclo x tutti record del cliente da trasmettere
012500031103     C                   DOW       not %eof(tistb02l)
012600031103     C*
012700031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
012800031103     C* deve restituire al cliente nel file tradotto
012900031103     C                   IF        stbFTR = *blanks                             * ridondante
013000031103     C*
013100080618     C* Gestisco solamente gli stati standard
013200080618     C                   IF        stbTIS = '1'
013300081111     C*
013400081111     C* Inizializzo i campi d wrk
013500090128     C                   movel     *blanks       wEsito1           1
013600080902     C*
013700081111     C* Occorre ignorare le bolle figlie (chiamata metodo GetLblTyp)
013800081111     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
013900081111     C                                                STBAAS
014000081111     C                                               :STBLNP
014100081111     C                                               :STBNRS
014200081111     C                                               :STBNSP
014300090128     C                                               :pOutLblTyp
014400090128     C                                               :pOutAnnoBO
014500090128     C                                               :pOutLineaParBO
014600090128     C                                               :pOutSerieBO
014700090128     C                                               :pOutNumSpedBO
014800090128     C                                               :pOutDcmBO
014900090128     C                                               :pOutCcaBO
015000090128     C                                               :pOutRblBO))
015100081111     C*
015200081111     C* Se trattasi d bolla originale proseguo con le verifiche
015300090128     C                   IF        pOutLblTyp = 'O'
015400090128     C*
015500090128     C* Aggancio la bolla
015600090128     C     KEYtas30c_P   CHAIN     TITAS30C
015700090128     C                   IF        %found(TITAS30C)
015800090303     C*
015900090303     C* Inizializzo indicatore condizionamento scrittura buffer output
016000090303     C                   SETOFF                                       55
016100090303     C*
016200090303     C* Verifico se trattasi d evento particolare autogenerato e se data/ora evento
016300090303     C* >= data consegna reale bolla => escludo
016400090303     C                   MOVEL     stbCOS        wEvento           3
016500090303     C     wEvento       LOOKUP    skEventi                               20
016600090303     C                   IF        *in20 = *on
016700090303     C                   IF        stbDAS*10000+stbORS >=
016800090303     C                             tasDCM*10000+tasHMC
016900090303     C                   SETON                                        55
017000090303     C                   ENDIF
017100090303     C                   ENDIF
017200080618     C*
017300080618     C* Scarico il buffer d output
017400090303     C  N55              EXSR      WRIREC
017500080901     C*
017600090128     C                   ENDIF
017700080902     C                   ENDIF
017800090128     C                   ENDIF
017900090128     C                   ENDIF
018000031103     C*
018100031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
018200031103     C                   EVAL      stbFTR = 'T'
018300031103     C                   UPDATE    tistb000
018400991027     C*
018500090128     C     KEYstb02P_P   READE     tistb02l
018600030325     C                   ENDDO
018700031103     C                   ENDIF
018800991027     C*
018900030325     C                   EVAL      wrkesito = '0'
019000991027     C*
019100910830     C                   ENDSR
019200031103
019300031103
019400031103
019500080618     C     wrirec        BEGSR
019600090128     C*
019700090128     C                   RESET                   C_DS_Cli
019800090128     C*
019900090128     C                   EVAL      C_LDV         = DS_LDV
020000090128     C                   MOVEL     stbDAS        C_DataStato
020100090128     C                   MOVEL     stbCOS        C_Stato
020200090128     C                   MOVEL     tasNCL        C_Colli
020300090128     C                   MOVEL     stbRMA        C_NumeroFatt
020400090128     C                   MOVEL     tasLNA        C_LNA
020500090128     C                   MOVEL     stbORS        C_OraStato
020600090303     C                   MOVE      '00'          C_OraStato
020700090303     C                   MOVEL     stbDSP        C_DataSped
020800090128     C                   MOVEL     tasDCM        C_DataCons
020900090128     C                   MOVEL     tasHMC        C_OraCons
021000090128     C                   MOVEL     tasRSD        C_RSD
021100031103     C*
021200090128     C                   EVAL      wBufferOut=
021300090129     C                             w§PPIDELTXT+%trim(C_LDV)          +
021400090129     C                             w§PPIDELTXT+w§PPISEPFLD+
021500090129     C                             w§PPIDELTXT+%trim(C_DataStato)    +
021600090129     C                             w§PPIDELTXT+w§PPISEPFLD+
021700090129     C                             w§PPIDELTXT+%trim(C_Stato)        +
021800090129     C                             w§PPIDELTXT+w§PPISEPFLD+
021900090129     C                             w§PPIDELTXT+%trim(C_Colli)        +
022000090129     C                             w§PPIDELTXT+w§PPISEPFLD+
022100090129     C                             w§PPIDELTXT+%trim(C_NumeroFatt)   +
022200090129     C                             w§PPIDELTXT+w§PPISEPFLD+
022300090129     C                             w§PPIDELTXT+%trim(C_Filler001)    +
022400090129     C                             w§PPIDELTXT+w§PPISEPFLD+
022500090129     C                             w§PPIDELTXT+%trim(C_Filler002)    +
022600090129     C                             w§PPIDELTXT+w§PPISEPFLD+
022700090129     C                             w§PPIDELTXT+%trim(C_LNA)          +
022800090129     C                             w§PPIDELTXT+w§PPISEPFLD+
022900090129     C                             w§PPIDELTXT+%trim(C_OraStato)     +
023000090129     C                             w§PPIDELTXT+w§PPISEPFLD+
023100090129     C                             w§PPIDELTXT+%trim(C_Note)         +
023200090129     C                             w§PPIDELTXT+w§PPISEPFLD+
023300090129     C                             w§PPIDELTXT+%trim(C_DataSped)     +
023400090129     C                             w§PPIDELTXT+w§PPISEPFLD+
023500090129     C                             w§PPIDELTXT+%trim(C_DataIncasso)  +
023600090129     C                             w§PPIDELTXT+w§PPISEPFLD+
023700090129     C                             w§PPIDELTXT+%trim(C_DataCons)     +
023800090129     C                             w§PPIDELTXT+w§PPISEPFLD+
023900090129     C                             w§PPIDELTXT+%trim(C_OraCons)      +
024000090129     C                             w§PPIDELTXT+w§PPISEPFLD+
024100090129     C                             w§PPIDELTXT+%trim(C_RSD)          +
024200090129     C                             w§PPIDELTXT+w§PPISEPFLD+
024300090129     C                             w§PPIDELTXT+%trim(C_PesoRil)      +
024400090129     C                             w§PPIDELTXT+w§PPISEPFLD+
024500090129     C                             w§PPIDELTXT+%trim(C_VolumeRil)    +
024600090129     C                             w§PPIDELTXT
024700031103     C*
024800031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
024900090128     C                   EVAL      VAWDTA = %trim(wBufferOut)
025000031103     C                   WRITE     TIVAW000
025100031103     C*
025200031103     C                   ENDSR
025300031103
025400031103
025500991027
025600991027      /TITLE Operazioni iniziali.
025700991027     C     *inzsr        BEGSR
025800991027     C*
025900991027     C     *ENTRY        PLIST
026000031103     C                   parm                    prmppi
026100991027     C     wrkesito      parm      wrkesito      prmesito
026200031103     C*
026300031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
026400031103     C                   EVAL      DSPPI = prmppi
026500030709     C*
026600030709     C* Definizione chiavi
026700030924     C*
026800031103     C* Chiave su TISTB02L - Parziale
026900090128     C     KEYstb02P_P   KLIST
027000031104     C                   KFLD                    wKstbKSU
027100031103     C                   KFLD                    wKstbFTR
027200090128     C*
027300090128     C* Chiave su TITAS30C - Parziale
027400090128     C     KEYtas30c_P   KLIST
027500090128     C                   KFLD                    stbAAS
027600090128     C                   KFLD                    stbLNP
027700090128     C                   KFLD                    stbNRS
027800090128     C                   KFLD                    stbNSP
027900991027     C*
028000991027     C                   ENDSR
