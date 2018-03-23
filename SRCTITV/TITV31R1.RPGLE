000100081111     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400090128     FTITAS30C  IF   E           K DISK
000500090303     FTABEL00F  IF   E           K DISK
000600100806     FTNTBE01L  IF   E           K DISK
000700100806     FFNORN02L  IF   E           K DISK
000800090323     FTISTB02L  UF   E           K DISK    commit
000900090323     FTIVAWWWT  UF A E             DISK    commit
001000031103
001100041015     D*-----------
001200041015     D* VARIABILI D WRK
001300041015     D*-----------
001400031103     D prmppi          S             50
001500030924     D prmesito        S              1
001600030924     D wrkesito        S                   like(prmesito)
001700090128     D wBufferOut      S           2048    varying
001800090303     D DS2Z          E DS                  inz
001900100806     D dfar          E DS                  inz
002000100806     D dcmr          E DS                  inz
002100090303     D skEventi        S              3    DIM(999)
002200031103
002300031103     D*-----------
002400031103     D* VARIABILI RIFERITE AL DATA-BASE
002500031103     D*-----------
002600080618     D wKstbKSU        S                   like(stbKSU)
002700080618     D wKstbFTR        S                   like(stbFTR)
002800030924
002900030924     D*-----------
003000030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
003100030924     D*-----------
003200030925     D C_DS_Cli        DS
003300090129     D  C_LDV                        25A   INZ('x')
003400090129     D  C_DataStato                   8A   INZ('0')
003500090129     D  C_Stato                      15A   INZ('x')
003600090129     D  C_Colli                       5A   INZ('0')
003700090129     D  C_NumeroFatt                 15A   INZ('x')
003800090129     D  C_Filler001                   5A   INZ('x')
003900090129     D  C_Filler002                  15A   INZ('x')
004000090129     D  C_LNA                        15A   INZ('0')
004100090129     D  C_OraStato                    6A   INZ('0')
004200090129     D  C_Note                      100A   INZ('x')
004300090129     D  C_DataSped                    8A   INZ('0')
004400090129     D  C_DataIncasso                 8A   INZ('0')
004500090129     D  C_DataCons                    8A   INZ('0')
004600090129     D  C_OraCons                     6A   INZ('0')
004700090129     D  C_RSD                        40A   INZ('x')
004800090128     D  C_PesoRil                     8A   INZ('0000,000')
004900090128     D  C_VolumeRil                   8A   INZ('00000,00')
005000031103
005100031103     D*-----------
005200031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
005300031103     D*-----------
005400031103     D DSPPI           DS
005500080618     D  w§PPIKSU                      7  0
005600090128     D  w§PPISEPFLD                   1
005700090128     D  w§PPIDELTXT                   1
005800080901
005900080901     D*-----------
006000080901     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
006100080901     D*-----------
006200090128     D                 DS
006300090128     D  tasLNP
006400090128     D  tasNRS
006500090128     D  tasNSP
006600090128     D DS_LDV                  1     12A
006700100806
006800100806     D*-----------
006900100806     D* RIDEFINIZIONE CHIAVE ORM
007000100806     D*-----------
007100100806     D                 DS
007200100806     D  vapPOE                        3  0
007300100806     D  vapNSR                        2  0
007400100806     D  vapNOR                        7  0
007500100806     D  vapNRV                        2  0
007600100806     D DS_KEY_ORM              1     14  0
007700100806
007800100806     D*-----------
007900100806     D* RIDEFINIZIONE FASE/CAUSALE ORM
008000100806     D*-----------
008100100806     D                 DS
008200100806     D  far_filler01                  1
008300100806     D  vapFAR                        3  0
008400100806     D  vapCAR                        2
008500100806     D DS_FAR_ORM              1      6A
008600100806
008700100806     D*-----------
008800100806     D* RIDEFINIZIONE RIFERIMENTO ORM DI TIPO "RC"
008900100806     D*-----------
009000100806     D                 DS
009100100806     D  krc_B                         1
009200100806     D  krc_AAS                       2  0
009300100806     D  krc_LNP                       3  0
009400100806     D  krc_NRS                       2  0
009500100806     D  krc_NSP                       7  0
009600100806     D DS_KRC_ORM              1     15A
009700081111
009800081111
009900081111     D*------------------
010000081111     D* LINKING A DEFINIZIONI ESTERNE
010100081111     D*------------------
010200090128     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
010300081111     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
010400081111
010500081111
010600030924
010700030325
010800030924     C* MAIN/
010900090303     C*
011000090303     C* Caricamento tabelle
011100090303     C                   EXSR      carTAB
011200041015     C*
011300041015     C* Esegue traduzione
011400991027     C                   EXSR      traduci
011500090303     C*
011600921023     C                   SETON                                        LR
011700090303
011800090303
011900090303
012000090303     C     carTAB        BEGSR
012100090303     C*
012200090303     C                   eval      tblKUT = 1
012300090303     C                   eval      tblCOD = '2Z'
012400090303     C*
012500090303     C* Carico gli eventi particolari
012600090303     C                   CLEAR                   xx                4 0
012700090303     C     KEYtab_P      SETLL     TABEL00F
012800090303     C     KEYtab_P      READE     TABEL00F
012900090303     C                   DOW       not %eof(TABEL00F)
013000090303     C*
013100090303     C                   MOVEL     TBLUNI        DS2Z
013200090303     C                   IF        §2ZCEP <> *blanks
013300090303     C                   ADD       1             xx
013400090303     C                   MOVEL     TBLKEY        skEventi(xx)
013500090303     C                   ENDIF
013600090303     C*
013700090303     C     KEYtab_P      READE     TABEL00F
013800090303     C                   ENDDO
013900090303     C*
014000090303     C                   ENDSR
014100991027
014200030924
014300030924
014400991027     C     traduci       BEGSR
014500031103     C*
014600031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
014700031103     C* di traduzione (trattasi d "post-traduzione")
014800031104     C                   EVAL      wKstbKSU = w§PPIKSU
014900031103     C                   EVAL      wKstbFTR = *blanks
015000090128     C     KEYstb02P_P   SETLL     tistb02l
015100031103     C                   IF        %equal(tistb02l)
015200090128     C     KEYstb02P_P   READE     tistb02l
015300991027     C*
015400090128     C* Ciclo x tutti record del cliente da trasmettere
015500031103     C                   DOW       not %eof(tistb02l)
015600100806     C*
015700100806     C                   SETOFF                                       50
015800100806     C                   CLEAR                   tasLNP
015900100806     C                   CLEAR                   tasNRS
016000100806     C                   CLEAR                   tasNSP
016100100806     C                   CLEAR                   tasNCL
016200100806     C                   CLEAR                   tasRMN
016300100806     C                   CLEAR                   tasLNA
016400100806     C                   CLEAR                   tasDCM
016500100806     C                   CLEAR                   tasHMC
016600100806     C                   CLEAR                   tasRSD
016700031103     C*
016800031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
016900031103     C* deve restituire al cliente nel file tradotto
017000031103     C                   IF        stbFTR = *blanks                             * ridondante
017100031103     C*
017200080618     C* Gestisco solamente gli stati standard
017300100806     C                   IF        stbTIS = '1'
017400100806     C*
017500100806     C* Gestisco le differenti origini stato corrente
017600100806     C                   SELECT
017700100806     C                   WHEN      stbPRS <> '6'
017800081111     C*
017900100806     C* Inizializzazioni d wrk
018000100806     C                   MOVEL     *blanks       wEsito1           1
018100100806     C                   SETON                                        50
018200080902     C*
018300081111     C* Occorre ignorare le bolle figlie (chiamata metodo GetLblTyp)
018400100806     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
018500081111     C                                                STBAAS
018600081111     C                                               :STBLNP
018700081111     C                                               :STBNRS
018800081111     C                                               :STBNSP
018900090128     C                                               :pOutLblTyp
019000090128     C                                               :pOutAnnoBO
019100090128     C                                               :pOutLineaParBO
019200090128     C                                               :pOutSerieBO
019300090128     C                                               :pOutNumSpedBO
019400090128     C                                               :pOutDcmBO
019500090128     C                                               :pOutCcaBO
019600090128     C                                               :pOutRblBO))
019700081111     C*
019800081111     C* Se trattasi d bolla originale proseguo con le verifiche
019900090128     C                   IF        pOutLblTyp = 'O'
020000090128     C*
020100090128     C* Aggancio la bolla
020200100806     C                   EVAL      tasAAS = stbAAS
020300100806     C                   EVAL      tasLNP = stbLNP
020400100806     C                   EVAL      tasNRS = stbNRS
020500100806     C                   EVAL      tasNSP = stbNSP
020600100806     C     KEYtas30c_P   CHAIN     TITAS30C
020700090128     C                   IF        %found(TITAS30C)
020800090303     C*
020900090303     C* Inizializzo indicatore condizionamento scrittura buffer output
021000090303     C                   SETOFF                                       55
021100090303     C*
021200090303     C* Verifico se trattasi d evento particolare autogenerato e se data/ora evento
021300090303     C* >= data consegna reale bolla => escludo
021400090303     C                   MOVEL     stbCOS        wEvento           3
021500090303     C     wEvento       LOOKUP    skEventi                               20
021600090303     C                   IF        *in20 = *on
021700090303     C                   IF        stbDAS*10000+stbORS >=
021800090303     C                             tasDCM*10000+tasHMC
021900090303     C                   SETON                                        55
022000090303     C                   ENDIF
022100090303     C                   ENDIF
022200080618     C*
022300080618     C* Scarico il buffer d output
022400090303     C  N55              EXSR      WRIREC
022500080901     C*
022600090128     C                   ENDIF
022700080902     C                   ENDIF
022800100806     C*
022900100806     C                   WHEN      stbPRS  = '6'
023000100922     C*
023100100922     C                   SETOFF                                       70
023200100806     C*
023300100806     C* Ridefinisco i dati
023400100806     C                   EVAL      DS_KEY_ORM = stbRMN
023500100806     C                   EVAL      DS_FAR_ORM = stbCOS
023600100806     C*
023700100806     C* Se orm di tipo "RC" => aggancio la bolla originale
023800100806     C                   IF        stbNRS > *zeros
023900100806     C                   EVAL      DS_KRC_ORM = stbRMA
024000100806     C                   EVAL      tasAAS = 2000 + krc_AAS
024100100806     C                   EVAL      tasLNP = krc_LNP
024200100806     C                   EVAL      tasNRS = krc_NRS
024300100806     C                   EVAL      tasNSP = krc_NSP
024400100806     C     KEYtas30c_P   CHAIN     TITAS30C
024500100922     C                   IF        not %found(TITAS30C)
024600100922     C                   SETON                                        70
024700100922     C                   ENDIF
024800100806     C                   ENDIF
024900100806     C*
025000100806     C* Inizializzazioni d wrk
025100100806     C                   MOVEL     *blanks       wNOTE           100
025200100806     C*
025300100806     C* Reperisco la descrizione della fase da tabella
025400100806     C                   MOVEL(p)  'FAR'         tbeCOD
025500100806     C                   MOVEL(p)  vapFAR        tbeKE1
025600100806     C     KEYtbe01_P    CHAIN     tntbe01l
025700100806     C                   IF        %found(tntbe01l)
025800100806     C                   MOVEL(P)  tbeuni        dfar
025900100806     C                   ELSE
026000100806     C                   CLEAR                   dfar
026100100806     C                   ENDIF
026200100806     C*
026300100806     C                   EVAL      wNOTE = D§FARDVA
026400100806     C*
026500100806     C* Reperisco la descrizione della causale mancato ritiro
026600100806     C                   IF        vapCAR <> *blanks
026700100806     C                   MOVEL(p)  'CMR'         tbeCOD
026800100806     C                   MOVEL(p)  vapCAR        tbeKE1
026900100806     C     KEYtbe01_P    CHAIN     tntbe01l
027000100806     C                   IF        %found(tntbe01l)
027100100806     C                   MOVEL(P)  tbeuni        dcmr
027200100806     C                   ELSE
027300100806     C                   CLEAR                   dcmr
027400100806     C                   ENDIF
027500100806     C*
027600100806     C                   EVAL      wNOTE = %trim(wNOTE) + '-' + D§CMRDVA
027700100806     C                   ENDIF
027800100806     C*
027900100806     C* Aggancio il file FNORN x reperire le eventuali note fasi da ritornare al cliente
028000100806     C     KEYorn02_P    SETLL     fnorn02l
028100100806     C                   IF        %equal(fnorn02l)
028200100806     C                   EVAL      wNOTE = %trim(wNOTE) + '-'
028300100806     C*
028400100806     C* Inizializzo contatore x ritorno max 4 note
028500100806     C                   Z-ADD     1             wContaORN         1 0
028600100806     C*
028700100806     C* Lettura note
028800100806     C     KEYorn02_P    READE     fnorn02l
028900100806     C                   DOW       not %eof(fnorn02l) AND
029000100806     C                             wContaORN <= 4
029100100806     C*
029200100806     C* Solo note con data/ora > data/ora esecuzione fase relativa
029300100806     C                   IF        ornDAI*1000000+ornORI >=
029400100806     C                             stbDAS*1000000+stbORS
029500100806     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
029600100806     C                                     %trim(ornNOT)
029700100806     C                   ADD       1             wContaORN
029800100806     C                   ENDIF
029900100806     C*
030000100806     C     KEYorn02_P    READE     fnorn02l
030100100806     C                   ENDDO
030200100806     C                   ENDIF
030300100806     C*
030400100806     C* Scarico il buffer d output
030500100922     C  N70              EXSR      WRIREC
030600100806     C*
030700100806     C                   ENDSL
030800100806     C*
030900090128     C                   ENDIF
031000090128     C                   ENDIF
031100031103     C*
031200031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
031300100922     C  N70              EVAL      stbFTR = 'T'
031400100922     C  N70              UPDATE    tistb000
031500991027     C*
031600090128     C     KEYstb02P_P   READE     tistb02l
031700030325     C                   ENDDO
031800031103     C                   ENDIF
031900991027     C*
032000030325     C                   EVAL      wrkesito = '0'
032100991027     C*
032200910830     C                   ENDSR
032300031103
032400031103
032500031103
032600080618     C     wrirec        BEGSR
032700090128     C*
032800090128     C                   RESET                   C_DS_Cli
032900090128     C*
033000100806     C                   EVAL      C_LDV         = DS_LDV
033100090128     C                   MOVEL     stbDAS        C_DataStato
033200090128     C                   MOVEL     stbCOS        C_Stato
033300090128     C                   MOVEL     tasNCL        C_Colli
033400100806     C   50              MOVEL     stbRMA        C_NumeroFatt
033500100922     C  N50              EVAL      C_NumeroFatt = %char(tasRMN)
033600100806     C                   MOVEL     tasLNA        C_LNA
033700090128     C                   MOVEL     stbORS        C_OraStato
033800090303     C                   MOVE      '00'          C_OraStato
033900100806     C  N50              MOVEL     wNOTE         C_Note
034000090303     C                   MOVEL     stbDSP        C_DataSped
034100100806     C   50              MOVEL     tasDCM        C_DataCons
034200100806     C   50              MOVEL     tasHMC        C_OraCons
034300100806     C                   MOVEL     tasRSD        C_RSD
034400031103     C*
034500090128     C                   EVAL      wBufferOut=
034600090129     C                             w§PPIDELTXT+%trim(C_LDV)          +
034700090129     C                             w§PPIDELTXT+w§PPISEPFLD+
034800090129     C                             w§PPIDELTXT+%trim(C_DataStato)    +
034900090129     C                             w§PPIDELTXT+w§PPISEPFLD+
035000090129     C                             w§PPIDELTXT+%trim(C_Stato)        +
035100090129     C                             w§PPIDELTXT+w§PPISEPFLD+
035200090129     C                             w§PPIDELTXT+%trim(C_Colli)        +
035300090129     C                             w§PPIDELTXT+w§PPISEPFLD+
035400090129     C                             w§PPIDELTXT+%trim(C_NumeroFatt)   +
035500090129     C                             w§PPIDELTXT+w§PPISEPFLD+
035600090129     C                             w§PPIDELTXT+%trim(C_Filler001)    +
035700090129     C                             w§PPIDELTXT+w§PPISEPFLD+
035800090129     C                             w§PPIDELTXT+%trim(C_Filler002)    +
035900090129     C                             w§PPIDELTXT+w§PPISEPFLD+
036000090129     C                             w§PPIDELTXT+%trim(C_LNA)          +
036100090129     C                             w§PPIDELTXT+w§PPISEPFLD+
036200090129     C                             w§PPIDELTXT+%trim(C_OraStato)     +
036300090129     C                             w§PPIDELTXT+w§PPISEPFLD+
036400090129     C                             w§PPIDELTXT+%trim(C_Note)         +
036500090129     C                             w§PPIDELTXT+w§PPISEPFLD+
036600090129     C                             w§PPIDELTXT+%trim(C_DataSped)     +
036700090129     C                             w§PPIDELTXT+w§PPISEPFLD+
036800090129     C                             w§PPIDELTXT+%trim(C_DataIncasso)  +
036900090129     C                             w§PPIDELTXT+w§PPISEPFLD+
037000090129     C                             w§PPIDELTXT+%trim(C_DataCons)     +
037100090129     C                             w§PPIDELTXT+w§PPISEPFLD+
037200090129     C                             w§PPIDELTXT+%trim(C_OraCons)      +
037300090129     C                             w§PPIDELTXT+w§PPISEPFLD+
037400090129     C                             w§PPIDELTXT+%trim(C_RSD)          +
037500090129     C                             w§PPIDELTXT+w§PPISEPFLD+
037600090129     C                             w§PPIDELTXT+%trim(C_PesoRil)      +
037700090129     C                             w§PPIDELTXT+w§PPISEPFLD+
037800090129     C                             w§PPIDELTXT+%trim(C_VolumeRil)    +
037900090129     C                             w§PPIDELTXT
038000031103     C*
038100031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
038200090128     C                   EVAL      VAWDTA = %trim(wBufferOut)
038300031103     C                   WRITE     TIVAW000
038400031103     C*
038500031103     C                   ENDSR
038600031103
038700031103
038800991027
038900991027      /TITLE Operazioni iniziali.
039000991027     C     *inzsr        BEGSR
039100991027     C*
039200991027     C     *ENTRY        PLIST
039300031103     C                   parm                    prmppi
039400991027     C     wrkesito      parm      wrkesito      prmesito
039500031103     C*
039600031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
039700031103     C                   EVAL      DSPPI = prmppi
039800030709     C*
039900030709     C* Definizione chiavi
040000030924     C*
040100031103     C* Chiave su TISTB02L - Parziale
040200090128     C     KEYstb02P_P   KLIST
040300031104     C                   KFLD                    wKstbKSU
040400031103     C                   KFLD                    wKstbFTR
040500090128     C*
040600090128     C* Chiave su TITAS30C - Parziale
040700100806     C     KEYtas30c_P   KLIST
040800100806     C                   KFLD                    tasAAS
040900100806     C                   KFLD                    tasLNP
041000100806     C                   KFLD                    tasNRS
041100100806     C                   KFLD                    tasNSP
041200100806     C*
041300100806     C* Chiave su TABEL00F - Parziale
041400100806     C     KEYtab_P      KLIST
041500100806     C                   KFLD                    tblKUT
041600100806     C                   KFLD                    tblCOD
041700100806     C*
041800100806     C* Chiave su TNTBE01L - Parziale
041900100806     C     KEYtbe01_P    KLIST
042000100806     C                   KFLD                    tbeCOD
042100100806     C                   KFLD                    tbeKE1
042200100806     C                   KFLD                    tbeKE2
042300100806     C                   KFLD                    tbeLIN
042400100806     C*
042500100806     C* Chiave su FNORN02L - Parziale
042600100806     C     KEYorn02_P    KLIST
042700100806     C                   KFLD                    vapPOE
042800100806     C                   KFLD                    vapNSR
042900100806     C                   KFLD                    vapNOR
043000100806     C                   KFLD                    vapNRV
043100100806     C                   KFLD                    vapFAR
043200991027     C*
043300991027     C                   ENDSR
