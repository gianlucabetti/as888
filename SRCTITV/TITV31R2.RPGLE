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
002200130129     D sk3M            S              1    DIM(999)
002300031103
002400031103     D*-----------
002500031103     D* VARIABILI RIFERITE AL DATA-BASE
002600031103     D*-----------
002700080618     D wKstbKSU        S                   like(stbKSU)
002800080618     D wKstbFTR        S                   like(stbFTR)
002900030924
003000030924     D*-----------
003100030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
003200030924     D*-----------
003300030925     D C_DS_Cli        DS
003400090129     D  C_LDV                        25A   INZ('x')
003500090129     D  C_DataStato                   8A   INZ('0')
003600090129     D  C_Stato                      15A   INZ('x')
003700090129     D  C_Colli                       5A   INZ('0')
003800090129     D  C_NumeroFatt                 15A   INZ('x')
003900090129     D  C_Filler001                   5A   INZ('x')
004000090129     D  C_Filler002                  15A   INZ('x')
004100090129     D  C_LNA                        15A   INZ('0')
004200090129     D  C_OraStato                    6A   INZ('0')
004300090129     D  C_Note                      100A   INZ('x')
004400090129     D  C_DataSped                    8A   INZ('0')
004500090129     D  C_DataIncasso                 8A   INZ('0')
004600090129     D  C_DataCons                    8A   INZ('0')
004700090129     D  C_OraCons                     6A   INZ('0')
004800090129     D  C_RSD                        40A   INZ('x')
004900090128     D  C_PesoRil                     8A   INZ('0000,000')
005000090128     D  C_VolumeRil                   8A   INZ('00000,00')
005100031103
005200031103     D*-----------
005300031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
005400031103     D*-----------
005500031103     D DSPPI           DS
005600080618     D  w§PPIKSU                      7  0
005700090128     D  w§PPISEPFLD                   1
005800090128     D  w§PPIDELTXT                   1
005900080901
006000080901     D*-----------
006100080901     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
006200080901     D*-----------
006300090128     D                 DS
006400090128     D  tasLNP
006500090128     D  tasNRS
006600090128     D  tasNSP
006700090128     D DS_LDV                  1     12A
006800100806
006900100806     D*-----------
007000100806     D* RIDEFINIZIONE CHIAVE ORM
007100100806     D*-----------
007200100806     D                 DS
007300100806     D  vapPOE                        3  0
007400100806     D  vapNSR                        2  0
007500100806     D  vapNOR                        7  0
007600100806     D  vapNRV                        2  0
007700100806     D DS_KEY_ORM              1     14  0
007800100806
007900100806     D*-----------
008000100806     D* RIDEFINIZIONE FASE/CAUSALE ORM
008100100806     D*-----------
008200100806     D                 DS
008300100806     D  far_filler01                  1
008400100806     D  vapFAR                        3  0
008500100806     D  vapCAR                        2
008600100806     D DS_FAR_ORM              1      6A
008700100806
008800100806     D*-----------
008900100806     D* RIDEFINIZIONE RIFERIMENTO ORM DI TIPO "RC"
009000100806     D*-----------
009100100806     D                 DS
009200100806     D  krc_B                         1
009300100806     D  krc_AAS                       2  0
009400100806     D  krc_LNP                       3  0
009500100806     D  krc_NRS                       2  0
009600100806     D  krc_NSP                       7  0
009700100806     D DS_KRC_ORM              1     15A
009800081111
009900081111
010000081111     D*------------------
010100081111     D* LINKING A DEFINIZIONI ESTERNE
010200081111     D*------------------
010300090128     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
010400081111     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
010500081111
010600081111
010700030924
010800030325
010900030924     C* MAIN/
011000090303     C*
011100090303     C* Caricamento tabelle
011200090303     C                   EXSR      carTAB
011300041015     C*
011400041015     C* Esegue traduzione
011500991027     C                   EXSR      traduci
011600090303     C*
011700921023     C                   SETON                                        LR
011800090303
011900090303
012000090303
012100090303     C     carTAB        BEGSR
012200090303     C*
012300090303     C                   eval      tblKUT = 1
012400090303     C                   eval      tblCOD = '2Z'
012500090303     C*
012600090303     C* Carico gli eventi particolari
012700090303     C                   CLEAR                   xx                4 0
012800090303     C     KEYtab_P      SETLL     TABEL00F
012900090303     C     KEYtab_P      READE     TABEL00F
013000090303     C                   DOW       not %eof(TABEL00F)
013100090303     C*
013200090303     C                   MOVEL     TBLUNI        DS2Z
013300090303     C                   IF        §2ZCEP <> *blanks
013400090303     C                   ADD       1             xx
013500090303     C                   MOVEL     TBLKEY        skEventi(xx)
013600090303     C                   ENDIF
013700090303     C*
013800090303     C     KEYtab_P      READE     TABEL00F
013900090303     C                   ENDDO
014000130129     C*
014100130129     C                   eval      tblKUT = 1
014200130129     C                   eval      tblCOD = '3M'
014300130129     C*
014400130129     C* Carico gli eventi particolari
014500130129     C                   CLEAR                   x                 4 0
014600130129     C     KEYtab_P      SETLL     TABEL00F
014700130129     C     KEYtab_P      READE     TABEL00F
014800130129     C                   DOW       not %eof(TABEL00F)
014900130129     C*
015000130129     C                   ADD       1             x
015100130129     C                   MOVEL     TBLKEY        sk3M(x)
015200130129     C*
015300130129     C     KEYtab_P      READE     TABEL00F
015400130129     C                   ENDDO
015500090303     C*
015600090303     C                   ENDSR
015700991027
015800030924
015900030924
016000991027     C     traduci       BEGSR
016100031103     C*
016200031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
016300031103     C* di traduzione (trattasi d "post-traduzione")
016400031104     C                   EVAL      wKstbKSU = w§PPIKSU
016500031103     C                   EVAL      wKstbFTR = *blanks
016600090128     C     KEYstb02P_P   SETLL     tistb02l
016700031103     C                   IF        %equal(tistb02l)
016800090128     C     KEYstb02P_P   READE     tistb02l
016900991027     C*
017000090128     C* Ciclo x tutti record del cliente da trasmettere
017100031103     C                   DOW       not %eof(tistb02l)
017200100806     C*
017300100806     C                   SETOFF                                       50
017400100806     C                   CLEAR                   tasLNP
017500100806     C                   CLEAR                   tasNRS
017600100806     C                   CLEAR                   tasNSP
017700100806     C                   CLEAR                   tasNCL
017800100806     C                   CLEAR                   tasRMN
017900100806     C                   CLEAR                   tasLNA
018000100806     C                   CLEAR                   tasDCM
018100100806     C                   CLEAR                   tasHMC
018200100806     C                   CLEAR                   tasRSD
018300031103     C*
018400031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
018500031103     C* deve restituire al cliente nel file tradotto
018600031103     C                   IF        stbFTR = *blanks                             * ridondante
018700031103     C*
018800080618     C* Gestisco solamente gli stati standard
018900100806     C                   IF        stbTIS = '1'
019000100806     C*
019100100806     C* Gestisco le differenti origini stato corrente
019200100806     C                   SELECT
019300100806     C                   WHEN      stbPRS <> '6'
019400081111     C*
019500100806     C* Inizializzazioni d wrk
019600100806     C                   MOVEL     *blanks       wEsito1           1
019700100806     C                   SETON                                        50
019800080902     C*
019900081111     C* Occorre ignorare le bolle figlie (chiamata metodo GetLblTyp)
020000100806     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
020100081111     C                                                STBAAS
020200081111     C                                               :STBLNP
020300081111     C                                               :STBNRS
020400081111     C                                               :STBNSP
020500090128     C                                               :pOutLblTyp
020600090128     C                                               :pOutAnnoBO
020700090128     C                                               :pOutLineaParBO
020800090128     C                                               :pOutSerieBO
020900090128     C                                               :pOutNumSpedBO
021000090128     C                                               :pOutDcmBO
021100090128     C                                               :pOutCcaBO
021200090128     C                                               :pOutRblBO))
021300081111     C*
021400081111     C* Se trattasi d bolla originale proseguo con le verifiche
021500090128     C                   IF        pOutLblTyp = 'O'
021600090128     C*
021700090128     C* Aggancio la bolla
021800100806     C                   EVAL      tasAAS = stbAAS
021900100806     C                   EVAL      tasLNP = stbLNP
022000100806     C                   EVAL      tasNRS = stbNRS
022100100806     C                   EVAL      tasNSP = stbNSP
022200100806     C     KEYtas30c_P   CHAIN     TITAS30C
022300090128     C                   IF        %found(TITAS30C)
022400090303     C*
022500090303     C* Inizializzo indicatore condizionamento scrittura buffer output
022600090303     C                   SETOFF                                       55
022700090303     C*
022800090303     C* Verifico se trattasi d evento particolare autogenerato e se data/ora evento
022900090303     C* >= data consegna reale bolla => escludo
023000090303     C                   MOVEL     stbCOS        wEvento           3
023100090303     C     wEvento       LOOKUP    skEventi                               20
023200090303     C                   IF        *in20 = *on
023300090303     C                   IF        stbDAS*10000+stbORS >=
023400090303     C                             tasDCM*10000+tasHMC
023500090303     C                   SETON                                        55
023600090303     C                   ENDIF
023700090303     C                   ENDIF
023800080618     C*
023900080618     C* Scarico il buffer d output
024000090303     C  N55              EXSR      WRIREC
024100080901     C*
024200090128     C                   ENDIF
024300080902     C                   ENDIF
024400100806     C*
024500100806     C                   WHEN      stbPRS  = '6'
024600100922     C*
024700100922     C                   SETOFF                                       70
024800100806     C*
024900100806     C* Ridefinisco i dati
025000100806     C                   EVAL      DS_KEY_ORM = stbRMN
025100100806     C                   EVAL      DS_FAR_ORM = stbCOS
025200100806     C*
025300100806     C* Se orm di tipo "RC" => aggancio la bolla originale
025400100806     C                   IF        stbNRS > *zeros
025500100806     C                   EVAL      DS_KRC_ORM = stbRMA
025600100806     C                   EVAL      tasAAS = 2000 + krc_AAS
025700100806     C                   EVAL      tasLNP = krc_LNP
025800100806     C                   EVAL      tasNRS = krc_NRS
025900100806     C                   EVAL      tasNSP = krc_NSP
026000100806     C     KEYtas30c_P   CHAIN     TITAS30C
026100100922     C                   IF        not %found(TITAS30C)
026200100922     C                   SETON                                        70
026300100922     C                   ENDIF
026400100806     C                   ENDIF
026500100806     C*
026600100806     C* Inizializzazioni d wrk
026700100806     C                   MOVEL     *blanks       wNOTE           100
026800100806     C*
026900100806     C* Reperisco la descrizione della fase da tabella
027000100806     C                   MOVEL(p)  'FAR'         tbeCOD
027100100806     C                   MOVEL(p)  vapFAR        tbeKE1
027200100806     C     KEYtbe01_P    CHAIN     tntbe01l
027300100806     C                   IF        %found(tntbe01l)
027400100806     C                   MOVEL(P)  tbeuni        dfar
027500100806     C                   ELSE
027600100806     C                   CLEAR                   dfar
027700100806     C                   ENDIF
027800100806     C*
027900100806     C                   EVAL      wNOTE = D§FARDVA
028000100806     C*
028100100806     C* Reperisco la descrizione della causale mancato ritiro
028200100806     C                   IF        vapCAR <> *blanks
028300100806     C                   MOVEL(p)  'CMR'         tbeCOD
028400100806     C                   MOVEL(p)  vapCAR        tbeKE1
028500100806     C     KEYtbe01_P    CHAIN     tntbe01l
028600100806     C                   IF        %found(tntbe01l)
028700100806     C                   MOVEL(P)  tbeuni        dcmr
028800100806     C                   ELSE
028900100806     C                   CLEAR                   dcmr
029000100806     C                   ENDIF
029100100806     C*
029200100806     C                   EVAL      wNOTE = %trim(wNOTE) + '-' + D§CMRDVA
029300100806     C                   ENDIF
029400100806     C*
029500100806     C* Aggancio il file FNORN x reperire le eventuali note fasi da ritornare al cliente
029600100806     C     KEYorn02_P    SETLL     fnorn02l
029700100806     C                   IF        %equal(fnorn02l)
029800100806     C                   EVAL      wNOTE = %trim(wNOTE) + '-'
029900100806     C*
030000100806     C* Inizializzo contatore x ritorno max 4 note
030100100806     C                   Z-ADD     1             wContaORN         1 0
030200100806     C*
030300100806     C* Lettura note
030400100806     C     KEYorn02_P    READE     fnorn02l
030500100806     C                   DOW       not %eof(fnorn02l) AND
030600100806     C                             wContaORN <= 4
030700100806     C*
030800100806     C* Solo note con data/ora > data/ora esecuzione fase relativa
030900100806     C                   IF        ornDAI*1000000+ornORI >=
031000100806     C                             stbDAS*1000000+stbORS
031100100806     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
031200100806     C                                     %trim(ornNOT)
031300100806     C                   ADD       1             wContaORN
031400100806     C                   ENDIF
031500100806     C*
031600100806     C     KEYorn02_P    READE     fnorn02l
031700100806     C                   ENDDO
031800100806     C                   ENDIF
031900100806     C*
032000100806     C* Scarico il buffer d output
032100100922     C  N70              EXSR      WRIREC
032200100806     C*
032300100806     C                   ENDSL
032400100806     C*
032500090128     C                   ENDIF
032600090128     C                   ENDIF
032700031103     C*
032800031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
032900100922     C  N70              EVAL      stbFTR = 'T'
033000100922     C  N70              UPDATE    tistb000
033100991027     C*
033200090128     C     KEYstb02P_P   READE     tistb02l
033300030325     C                   ENDDO
033400031103     C                   ENDIF
033500991027     C*
033600030325     C                   EVAL      wrkesito = '0'
033700991027     C*
033800910830     C                   ENDSR
033900031103
034000031103
034100031103
034200080618     C     wrirec        BEGSR
034300090128     C*
034400130129     C* Forzatura concordata con il cliente => ritorniamo solamente:
034500130129     C* - solo se sigla operatore "da conferma"
034600130129     C* - solo se RMA valorizzato
034700130927     C* - solo se DATASTATO valorizzata
034800130129     C     tasSOP        LOOKUP    sk3M                                   20
034900130129     C*
035000130927     C                   IF        stbRMA > *blanks AND *in20 AND
035100130927     C                             stbDAS > *zeros
035200130129     C*
035300090128     C                   RESET                   C_DS_Cli
035400090128     C*
035500100806     C                   EVAL      C_LDV         = DS_LDV
035600090128     C                   MOVEL     stbDAS        C_DataStato
035700090128     C                   MOVEL     stbCOS        C_Stato
035800090128     C                   MOVEL     tasNCL        C_Colli
035900100806     C   50              MOVEL     stbRMA        C_NumeroFatt
036000100922     C  N50              EVAL      C_NumeroFatt = %char(tasRMN)
036100100806     C                   MOVEL     tasLNA        C_LNA
036200090128     C                   MOVEL     stbORS        C_OraStato
036300090303     C                   MOVE      '00'          C_OraStato
036400100806     C  N50              MOVEL     wNOTE         C_Note
036500090303     C                   MOVEL     stbDSP        C_DataSped
036600100806     C   50              MOVEL     tasDCM        C_DataCons
036700100806     C   50              MOVEL     tasHMC        C_OraCons
036800100806     C                   MOVEL     tasRSD        C_RSD
036900031103     C*
037000090128     C                   EVAL      wBufferOut=
037100090129     C                             w§PPIDELTXT+%trim(C_LDV)          +
037200090129     C                             w§PPIDELTXT+w§PPISEPFLD+
037300090129     C                             w§PPIDELTXT+%trim(C_DataStato)    +
037400090129     C                             w§PPIDELTXT+w§PPISEPFLD+
037500090129     C                             w§PPIDELTXT+%trim(C_Stato)        +
037600090129     C                             w§PPIDELTXT+w§PPISEPFLD+
037700090129     C                             w§PPIDELTXT+%trim(C_Colli)        +
037800090129     C                             w§PPIDELTXT+w§PPISEPFLD+
037900090129     C                             w§PPIDELTXT+%trim(C_NumeroFatt)   +
038000090129     C                             w§PPIDELTXT+w§PPISEPFLD+
038100090129     C                             w§PPIDELTXT+%trim(C_Filler001)    +
038200090129     C                             w§PPIDELTXT+w§PPISEPFLD+
038300090129     C                             w§PPIDELTXT+%trim(C_Filler002)    +
038400090129     C                             w§PPIDELTXT+w§PPISEPFLD+
038500090129     C                             w§PPIDELTXT+%trim(C_LNA)          +
038600090129     C                             w§PPIDELTXT+w§PPISEPFLD+
038700090129     C                             w§PPIDELTXT+%trim(C_OraStato)     +
038800090129     C                             w§PPIDELTXT+w§PPISEPFLD+
038900090129     C                             w§PPIDELTXT+%trim(C_Note)         +
039000090129     C                             w§PPIDELTXT+w§PPISEPFLD+
039100090129     C                             w§PPIDELTXT+%trim(C_DataSped)     +
039200090129     C                             w§PPIDELTXT+w§PPISEPFLD+
039300090129     C                             w§PPIDELTXT+%trim(C_DataIncasso)  +
039400090129     C                             w§PPIDELTXT+w§PPISEPFLD+
039500090129     C                             w§PPIDELTXT+%trim(C_DataCons)     +
039600090129     C                             w§PPIDELTXT+w§PPISEPFLD+
039700090129     C                             w§PPIDELTXT+%trim(C_OraCons)      +
039800090129     C                             w§PPIDELTXT+w§PPISEPFLD+
039900090129     C                             w§PPIDELTXT+%trim(C_RSD)          +
040000090129     C                             w§PPIDELTXT+w§PPISEPFLD+
040100090129     C                             w§PPIDELTXT+%trim(C_PesoRil)      +
040200090129     C                             w§PPIDELTXT+w§PPISEPFLD+
040300090129     C                             w§PPIDELTXT+%trim(C_VolumeRil)    +
040400090129     C                             w§PPIDELTXT
040500031103     C*
040600031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
040700090128     C                   EVAL      VAWDTA = %trim(wBufferOut)
040800031103     C                   WRITE     TIVAW000
040900130129     C*
041000130129     C                   ENDIF
041100031103     C*
041200031103     C                   ENDSR
041300031103
041400031103
041500991027
041600991027      /TITLE Operazioni iniziali.
041700991027     C     *inzsr        BEGSR
041800991027     C*
041900991027     C     *ENTRY        PLIST
042000031103     C                   parm                    prmppi
042100991027     C     wrkesito      parm      wrkesito      prmesito
042200031103     C*
042300031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
042400031103     C                   EVAL      DSPPI = prmppi
042500030709     C*
042600030709     C* Definizione chiavi
042700030924     C*
042800031103     C* Chiave su TISTB02L - Parziale
042900090128     C     KEYstb02P_P   KLIST
043000031104     C                   KFLD                    wKstbKSU
043100031103     C                   KFLD                    wKstbFTR
043200090128     C*
043300090128     C* Chiave su TITAS30C - Parziale
043400100806     C     KEYtas30c_P   KLIST
043500100806     C                   KFLD                    tasAAS
043600100806     C                   KFLD                    tasLNP
043700100806     C                   KFLD                    tasNRS
043800100806     C                   KFLD                    tasNSP
043900100806     C*
044000100806     C* Chiave su TABEL00F - Parziale
044100100806     C     KEYtab_P      KLIST
044200100806     C                   KFLD                    tblKUT
044300100806     C                   KFLD                    tblCOD
044400100806     C*
044500100806     C* Chiave su TNTBE01L - Parziale
044600100806     C     KEYtbe01_P    KLIST
044700100806     C                   KFLD                    tbeCOD
044800100806     C                   KFLD                    tbeKE1
044900100806     C                   KFLD                    tbeKE2
045000100806     C                   KFLD                    tbeLIN
045100100806     C*
045200100806     C* Chiave su FNORN02L - Parziale
045300100806     C     KEYorn02_P    KLIST
045400100806     C                   KFLD                    vapPOE
045500100806     C                   KFLD                    vapNSR
045600100806     C                   KFLD                    vapNOR
045700100806     C                   KFLD                    vapNRV
045800100806     C                   KFLD                    vapFAR
045900991027     C*
046000991027     C                   ENDSR
