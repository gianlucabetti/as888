000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400041015     FTNTBE01L  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600090323     FTISTB02L  UF   E           K DISK    commit
000700090323     FTIVAWWWT  UF A E             DISK    commit
000800031103
000900031103     D*-----------
001000031103     D* SCHIERE A PROGRAMMA
001100031103     D*-----------
001200060613     D CAUBAR          S             10    DIM(35) CTDATA PERRCD(1)
001300060613     D CAUCLI          S             10    DIM(35) CTDATA PERRCD(1)
001400091222     D WDCTBAR         S              1    DIM(4)  CTDATA PERRCD(1)
001500091222     D WDCTCLI         S             10    DIM(4)  CTDATA PERRCD(1)
001600041015
001700041015     D*-----------
001800041015     D* SCHIERE
001900041015     D*-----------
002000041015     D skTADCod        S              2    DIM(500)
002100041015     D skTADRag        S              1    DIM(500)
002200041015     D jTAD            S              3  0 INZ(*zeros)
002300030924
002400041015     D*-----------
002500041015     D* VARIABILI D WRK
002600041015     D*-----------
002700031103     D prmppi          S             50
002800030924     D prmesito        S              1
002900030924     D wrkesito        S                   like(prmesito)
003000031103     D j               S              2  0
003100041015     D wTADCod         S              2
003200041015
003300041015     D*-----------
003400041015     D* DEFINIZIONE DS ESTERNE
003500041015     D*-----------
003600041015     D DTAD          E DS
003700031103
003800031103     D*-----------
003900031103     D* VARIABILI RIFERITE AL DATA-BASE
004000031103     D*-----------
004100031104     D wKstbKSU        S                   like(stbKSU)
004200031103     D wKstbFTR        S                   like(stbFTR)
004300030924
004400030924     D*-----------
004500030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
004600030924     D*-----------
004700030925     D C_DS_Cli        DS
004800030925     D  C_CodVettore                 20    INZ(*blanks)
004900030925     D  C_CodDepPart                 20    INZ(*blanks)
005000030925     D  C_TipoRecord                  1    INZ(*blanks)
005100030925     D  C_IDSped                     20    INZ(*blanks)
005200030925     D  C_DataTrasm                  12    INZ(*zeros)
005300030925     D  C_DataEvento                 12    INZ(*zeros)
005400030925     D  C_CodEvento                   5    INZ(*blanks)
005500030925     D  C_DesEvento                 200    INZ(*blanks)
005600030925     D  C_CodAddeb                    5    INZ(*blanks)
005700030925     D  C_DesAddeb                   35    INZ(*blanks)
005800030925     D  C_ImpAddeb                   13    INZ(*zeros)
005900030925     D  C_Criterio                    1    INZ(*blanks)
006000030925     D  C_QtaRif                     13    INZ(*zeros)
006100030925     D  C_ImpRif                     13    INZ(*zeros)
006200030925     D  C_RifSpedVet                 50    INZ(*blanks)
006300030925     D  C_DataSped                   12    INZ(*blanks)
006400030925     D  C_FlgFatt                     1    INZ(*blanks)
006500030925     D  C_NumBordero                 20    INZ(*blanks)
006600030925     D  C_NumBancali                 13    INZ(*zeros)
006700030925     D  C_CodCliente                 20    INZ(*blanks)
006800031014     D  C_Causale                     5    INZ(*blanks)
006900030925     D  C_Note                      200    INZ(*blanks)
007000031103
007100030924     D*-----------
007200030924     D* RIDEFINIZIONE CHIAVE BOLLA IN FORMATO ALFA
007300030924     D*-----------
007400030924     D                 DS
007500031103     D  STBAAS                 1      4  0
007600031103     D  STBLNP                 5      7  0
007700031103     D  STBNRS                 8      9  0
007800031103     D  STBNSP                10     16  0
007900030924     D DS_KEYBOLLA             1     16
008000031103
008100031103     D*-----------
008200031103     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
008300031103     D*-----------
008400031103     D                 DS
008500031103     D  TASAAS                 1      4  0
008600031103     D  TASMGS                 5      8  0
008700031103     D DS_DATABOLLA            1      8
008800031103
008900031103     D*-----------
009000031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
009100031103     D*-----------
009200031103     D DSPPI           DS
009300031104     D  w§PPIKSU                      7  0
009400031103
009500031103     D*------------------
009600031103     D* DS "XSRDA8" - CONTROLLA DATA (8)
009700031103     D*------------------
009800031103     D WLBDA8          DS                  INZ
009900031103     D  G08DAT                 1      8  0
010000031103     D  G08INV                 9     16  0
010100031103     D  G08ERR                17     17
010200031103     D  G08TGI                18     22  0
010300030924
010400030325
010500030924     C* MAIN/
010600041015     C*
010700041015     C* Carica i dati tabellati
010800041015     C                   EXSR      cartab
010900041015     C*
011000041015     C* Esegue traduzione
011100991027     C                   EXSR      traduci
011200921023     C                   SETON                                        LR
011300041015
011400041015
011500041015
011600041015     C     cartab        BEGSR
011700041015     C*
011800041015     C* Caricamento tipi anomalia C/A
011900041015     C     'TAD'         SETLL     tntbe01l
012000041015     C                   IF        %equal(tntbe01l)
012100041015     C     'TAD'         READE     tntbe01l
012200041015     C                   DOW       not %eof(tntbe01l)
012300041015     C                   EVAL      DTAD = tbeUNI
012400041015     C                   ADD       1             jTAD
012500041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
012600041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
012700041015     C     'TAD'         READE     tntbe01l
012800041015     C                   ENDDO
012900041015     C                   ENDIF
013000041015     C*
013100041015     C                   ENDSR
013200991027
013300030924
013400030924
013500991027     C     traduci       BEGSR
013600031103     C*
013700031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
013800031103     C* di traduzione (trattasi d "post-traduzione")
013900031104     C                   EVAL      wKstbKSU = w§PPIKSU
014000031103     C                   EVAL      wKstbFTR = *blanks
014100031103     C     KEYstb02P     SETLL     tistb02l
014200031103     C                   IF        %equal(tistb02l)
014300031103     C     KEYstb02P     READE     tistb02l
014400031103     C                   SETOFF                                       50
014500991027     C*
014600031103     C* Ciclo x tuttii record del cliente da trasmettere
014700031103     C                   DOW       not %eof(tistb02l)
014800031014     C*
014900030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
015000031014     C                   RESET                   C_DS_Cli
015100041014     C                   SETOFF                                       60
015200031103     C*
015300031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
015400031103     C* deve restituire al cliente nel file tradotto
015500031103     C                   IF        stbFTR = *blanks                             * ridondante
015600031103     C*
015700031103     C* Innanzitutto chaino la bolla direttamente su TITAS
015800031103     C     KEYtas30P     CHAIN     titas30c
015900031103     C                   IF        %found(titas30c)
016000031103     C*
016100031103     C* Se trattasi d stato "personalizzato" => eseguo comportamento diverso stato x stato
016200031103     C                   IF        stbTIS = '9'
016300041015     C*
016400031103     C* X stato 'INCARICO1' eseguo inserimento degli stati cliente: A0 e B0
016500031103     C                   IF        stbCOS = 'INCARICO1'
016600041014     C                   SETON                                        60
016700031103     C* - A0
016800041014     C                   MOVEL     tasDRT        C_DataEvento
016900041014     C                   MOVE      tasHRT        C_DataEvento
017000031103     C                   EVAL      C_CodEvento    = 'A0'
017100031103     C                   EVAL      C_DesEvento    = 'RICEVUTA DISP. CONSEGNA'
017200031103     C                   MOVEL     stbLNP        C_CodDepPart
017300031103     C                   EVAL      C_TipoRecord   = '2'
017400031103     C  N50              EXSR      WRIREC
017500031103     C* - B0
017600041014     C                   MOVEL     tasDUC        C_DataEvento
017700041014     C                   MOVE      '0000'        C_DataEvento
017800031103     C                   EVAL      C_CodEvento    = 'B0'
017900031103     C                   EVAL      C_DesEvento    = 'PRESA IN CARICO VETTORE'
018000031103     C                   MOVEL     stbLNP        C_CodDepPart
018100031103     C                   EVAL      C_TipoRecord   = '2'
018200031103     C  N50              EXSR      WRIREC
018300031103     C                   ENDIF
018400041015     C*
018500031103     C* X stato 'INCARICO2' eseguo inserimento degli stati cliente: A0/B0/B3/C0
018600031103     C                   IF        stbCOS = 'INCARICO2'
018700041014     C                   SETON                                        60
018800031103     C* - A0
018900041014     C                   MOVEL     tasDRT        C_DataEvento
019000041014     C                   MOVE      tasHRT        C_DataEvento
019100031103     C                   EVAL      C_CodEvento    = 'A0'
019200031103     C                   EVAL      C_DesEvento    = 'RICEVUTA DISP. CONSEGNA'
019300031103     C                   MOVEL     stbLNP        C_CodDepPart
019400031103     C                   EVAL      C_TipoRecord   = '2'
019500031103     C  N50              EXSR      WRIREC
019600031103     C* - B0
019700041014     C                   MOVEL     tasDUC        C_DataEvento
019800041014     C                   MOVE      '0000'        C_DataEvento
019900031103     C                   EVAL      C_CodEvento    = 'B0'
020000031103     C                   EVAL      C_DesEvento    = 'PRESA IN CARICO VETTORE'
020100031103     C                   MOVEL     stbLNP        C_CodDepPart
020200031103     C                   EVAL      C_TipoRecord   = '2'
020300031103     C  N50              EXSR      WRIREC
020400041014     C* - B3
020500041014     C                   MOVEL     tasDUC        C_DataEvento
020600041014     C                   MOVE      '0000'        C_DataEvento
020700031103     C                   EVAL      C_CodEvento    = 'B3'
020800031103     C                   EVAL      C_DesEvento    = 'SPEDITA P.O. DI COMPETENZA'
020900031103     C                   MOVEL     stbLNP        C_CodDepPart
021000031103     C                   EVAL      C_TipoRecord   = '2'
021100031103     C  N50              EXSR      WRIREC
021200031103     C* - C0
021300041014     C                   MOVEL     tasDTI        C_DataEvento
021400041014     C                   MOVE      tasHTI        C_DataEvento
021500031103     C                   EVAL      C_CodEvento    = 'C0'
021600031103     C                   EVAL      C_DesEvento    = 'PRESA IN CARICO P.O. DI ' +
021700031103     C                                              'COMPETENZA'
021800031103     C                   MOVEL     tasLNA        C_CodDepPart
021900031103     C                   EVAL      C_TipoRecord   = '2'
022000031103     C  N50              EXSR      WRIREC
022100031103     C                   ENDIF
022200041015     C*
022300031103     C* X stato 'GIACNODIS1' eseguo inserimento degli stati cliente: B4+R01/B5+G01
022400031103     C                   IF        stbCOS = 'GIACNODIS1'
022500031103     C* - B4+R01
022600031103     C                   EVAL      C_CodEvento    = 'B4'
022700080128     C                   EVAL      C_DesEvento    = 'RIENTRATA PER MANCATA '   +
022800080128     C                                              'CONSEGNA E IN ATTESA DI ' +
022900031103     C                                              'DISPOSIZIONI'
023000031103     C                   EVAL      C_Causale = 'R01'
023100031103     C                   MOVEL     stbLNP        C_CodDepPart
023200031103     C                   EVAL      C_TipoRecord   = '2'
023300031103     C  N50              EXSR      WRIREC
023400031103     C* - B5+G01
023500031103     C                   EVAL      C_CodEvento    = 'B5'
023600031103     C                   EVAL      C_DesEvento    = 'ENTRATA IN GIACENZA'
023700031103     C                   EVAL      C_Causale = 'G01'
023800031103     C                   MOVEL     stbLNP        C_CodDepPart
023900031103     C                   EVAL      C_TipoRecord   = '2'
024000031103     C  N50              EXSR      WRIREC
024100031103     C                   ENDIF
024200041015     C*
024300031103     C* X stato 'GIACNODIS2' eseguo inserimento degli stati cliente: C4+R01/C5+G01
024400031103     C                   IF        stbCOS = 'GIACNODIS2'
024500031103     C* - C4+R01
024600031103     C                   EVAL      C_CodEvento    = 'C4'
024700050614     C                   EVAL      C_DesEvento    = 'RIENTRATA PER MANCATA '  +
024800031103     C                                              'CONSEGNA E IN ATESA DI ' +
024900031103     C                                              'DISPOSIZIONI'
025000031103     C                   EVAL      C_Causale = 'R01'
025100031103     C                   MOVEL     tasLNA        C_CodDepPart
025200031103     C                   EVAL      C_TipoRecord   = '2'
025300031103     C  N50              EXSR      WRIREC
025400031103     C* - C5+G01
025500031103     C                   EVAL      C_CodEvento    = 'C5'
025600031103     C                   EVAL      C_DesEvento    = 'ENTRATA IN GIACENZA'
025700031103     C                   EVAL      C_Causale = 'G01'
025800031103     C                   MOVEL     tasLNA        C_CodDepPart
025900031103     C                   EVAL      C_TipoRecord   = '2'
026000031103     C  N50              EXSR      WRIREC
026100031103     C                   ENDIF
026200041015     C*
026300031103     C* X stato 'GIACSIDIS1' eseguo inserimento degli stati cliente: B5+G02
026400031103     C                   IF        stbCOS = 'GIACSIDIS1'
026500031103     C* - B5+G02
026600031103     C                   EVAL      C_CodEvento    = 'B5'
026700031103     C                   EVAL      C_DesEvento    = 'MERCE ESCE GIACENZA'
026800031103     C                   EVAL      C_Causale = 'G02'
026900031103     C                   MOVEL     stbLNP        C_CodDepPart
027000031103     C                   EVAL      C_TipoRecord   = '2'
027100031103     C  N50              EXSR      WRIREC
027200031103     C                   ENDIF
027300041015     C*
027400031103     C* X stato 'GIACSIDIS2' eseguo inserimento degli stati cliente: C5+G02
027500031103     C                   IF        stbCOS = 'GIACSIDIS2'
027600031103     C* - C5+G02
027700031103     C                   EVAL      C_CodEvento    = 'C5'
027800031103     C                   EVAL      C_DesEvento    = 'MERCE ESCE GIACENZA'
027900031103     C                   EVAL      C_Causale = 'G02'
028000031103     C                   MOVEL     tasLNA        C_CodDepPart
028100031103     C                   EVAL      C_TipoRecord   = '2'
028200031103     C  N50              EXSR      WRIREC
028300031103     C                   ENDIF
028400041015     C*
028500041015     C* X tutti gli stati d tipo 'CONSOK*' verifico se c'è stata precedentemente consegna parziale
028600041015     C                   IF        %subst(stbCOS:1:6) = 'CONSOK'
028700041015     C* Verifico la consegna parziale (almeno 1)
028800041015     C                   IF        tasDCP > *zeros
028900041015     C* - C7
029000041015     C                   EVAL      C_CodEvento    = 'C7'
029100041015     C                   EVAL      C_DesEvento    = 'CONSEGNA DEFINITIVA DOPO '+
029200041015     C                                              'CONSEGNA PARZIALE'
029300041015     C                   MOVEL     tasLNA        C_CodDepPart
029400041015     C                   EVAL      C_TipoRecord   = '1'
029500041015     C  N50              EXSR      WRIREC
029600041015     C*
029700041015     C                   ELSE
029800041015     C*
029900031103     C* X stato 'CONSOK1' eseguo inserimento degli stati cliente: B2
030000031103     C                   IF        stbCOS = 'CONSOK1'
030100031103     C* - B2
030200031103     C                   EVAL      C_CodEvento    = 'B2'
030300031103     C                   EVAL      C_DesEvento    = 'CONSEGNA'
030400031103     C                   MOVEL     stbLNP        C_CodDepPart
030500031103     C                   EVAL      C_TipoRecord   = '1'
030600031103     C  N50              EXSR      WRIREC
030700031103     C                   ENDIF
030800041015     C*
030900031103     C* X stato 'CONSOK2' eseguo inserimento degli stati cliente: C2
031000031104     C                   IF        stbCOS = 'CONSOK2'
031100031103     C* - C2
031200031103     C                   EVAL      C_CodEvento    = 'C2'
031300031103     C                   EVAL      C_DesEvento    = 'CONSEGNA'
031400031103     C                   MOVEL     tasLNA        C_CodDepPart
031500031103     C                   EVAL      C_TipoRecord   = '1'
031600031103     C  N50              EXSR      WRIREC
031700031103     C                   ENDIF
031800041015     C                   ENDIF
031900031103     C                   ENDIF
032000041015     C                   ENDIF
032100031103     C*
032200031103     C* Se trattasi d stato "standard" => gestisco diversamente
032300031103     C                   IF        stbTIS = '1'
032400060619     C*
032500060621     C* X stati provenienti dagli eventi, se trattasi d eventi 'MIC' o 'RIC'
032600060619     C                   IF        stbPRS='2' AND (stbCOS='MIC' OR stbCOS='RIC')
032700060619     C* - DATA CONSEGNA RICHIESTA
032800060619     C                   IF        tasDCR <> *zeros OR tasHCR <> *zeros
032900060621     C                   EVAL      C_CodEvento    = 'C2'
033000060619     C                   EVAL      C_DesEvento    = 'PRESENTE DATA CONSEGNA ' +
033100060619     C                                              'RICHIESTA'
033200060619     C                   EVAL      C_Causale = 'X75'
033300060619     C                   MOVEL     tasLNA        C_CodDepPart
033400060619     C                   EVAL      C_TipoRecord   = '2'
033500060619     C  N50              EXSR      WRIREC
033600060619     C                   ENDIF
033700060619     C* - TURNO DI CHIUSURA
033800060619     C                   IF        tasGC1 <> *blanks OR tasGC2 <> *blanks
033900060621     C                   EVAL      C_CodEvento    = 'C2'
034000060619     C                   EVAL      C_DesEvento    = 'PRESENTE GIORNO CHIUSURA '+
034100060619     C                                              'TURNO'
034200060619     C                   EVAL      C_Causale = 'X72'
034300060619     C                   MOVEL     tasLNA        C_CodDepPart
034400060619     C                   EVAL      C_TipoRecord   = '2'
034500060619     C  N50              EXSR      WRIREC
034600060619     C                   ENDIF
034700060619     C* - SUPERMERCATO
034800060619     C                   IF        tasFTC = 'S' OR tasTC2 = 'S'
034900060621     C                   EVAL      C_CodEvento    = 'C2'
035000060619     C                   EVAL      C_DesEvento    = 'DESTINATARIO SUPERMERCATO'
035100060619     C                   EVAL      C_Causale = 'X77'
035200060619     C                   MOVEL     tasLNA        C_CodDepPart
035300060619     C                   EVAL      C_TipoRecord   = '2'
035400060619     C  N50              EXSR      WRIREC
035500060619     C                   ENDIF
035600060619     C*
035700060619     C                   ENDIF
035800041015     C*
035900031103     C* X stati provenienti dagli eventi, se trattasi d eventi 'MIC'
036000031103     C                   IF        stbPRS = '2' AND
036100031103     C                             stbCOS = 'MIC'
036200031103     C* Se LNP=LNA => inserimento dello stato cliente: B1
036300031103     C                   IF        stbLNP = tasLNA
036400031103     C* - B1
036500031103     C                   EVAL      C_CodEvento    = 'B1'
036600031103     C                   EVAL      C_DesEvento    = 'IN DISTRIBUZIONE'
036700031103     C                   MOVEL     stbLNP        C_CodDepPart
036800031103     C                   EVAL      C_TipoRecord   = '2'
036900031103     C  N50              EXSR      WRIREC
037000031103     C                   ENDIF
037100031103     C* Se LNP<>LNA => inserimento dello stato cliente: C1
037200031103     C                   IF        stbLNP <> tasLNA
037300031103     C* - C1
037400031103     C                   EVAL      C_CodEvento    = 'C1'
037500031103     C                   EVAL      C_DesEvento    = 'IN DISTRIBUZIONE'
037600031103     C                   MOVEL     tasLNA        C_CodDepPart
037700031103     C                   EVAL      C_TipoRecord   = '2'
037800031103     C  N50              EXSR      WRIREC
037900031103     C                   ENDIF
038000031103     C                   ENDIF
038100041015     C*
038200031103     C* X stati provenienti dagli eventi, se trattasi d eventi diversi da 'MIC'
038300031103     C                   IF        stbPRS =  '2' AND
038400031103     C                             stbCOS <> 'MIC'
038500031103     C* Se LNP=LNA => inserimento dello stato cliente: B2+X**
038600031103     C                   IF        stbLNP = tasLNA
038700031103     C* - B2+X**
038800031103     C                   EVAL      C_CodEvento    = 'B2'
038900031103     C                   EVAL      C_DesEvento    = 'CONSEGNA'
039000031103     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente Buffetti
039100031103     C                   Z-ADD     1             j
039200031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
039300031103     C                   IF        %found
039400031103     C                   EVAL      C_Causale = CAUCLI(j)
039500031103     C                   SETOFF                                       50
039600031103     C                   ELSE
039700031103     C                   SETON                                        50
039800031103     C                   ENDIF
039900031103     C                   MOVEL     stbLNP        C_CodDepPart
040000031103     C                   EVAL      C_TipoRecord   = '1'
040100031103     C  N50              EXSR      WRIREC
040200031103     C                   ENDIF
040300031103     C* Se LNP<>LNA => inserimento dello stato cliente: C2+X**
040400031103     C                   IF        stbLNP <> tasLNA
040500031103     C* - C2+X**
040600031103     C                   EVAL      C_CodEvento    = 'C2'
040700031103     C                   EVAL      C_DesEvento    = 'CONSEGNA'
040800031103     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente Buffetti
040900031103     C                   Z-ADD     1             j
041000031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
041100031103     C                   IF        %found
041200031103     C                   EVAL      C_Causale = CAUCLI(j)
041300031103     C                   SETOFF                                       50
041400031103     C                   ELSE
041500031103     C                   SETON                                        50
041600031103     C                   ENDIF
041700031103     C                   MOVEL     tasLNA        C_CodDepPart
041800031103     C                   EVAL      C_TipoRecord   = '1'
041900031103     C  N50              EXSR      WRIREC
042000031103     C                   ENDIF
042100031103     C                   ENDIF
042200041015     C*
042300041015     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
042400041015     C                   IF        stbPRS =  '5'
042500041015     C* - C8+X**
042600041015     C                   EVAL      C_CodEvento    = 'C8'
042700041015     C                   EVAL      C_DesEvento    = 'APERTURA PRATICA'
042800041015     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente Buffetti
042900041015     C                   Z-ADD     1             jTAD
043000041015     C                   MOVEL(P)  stbCOS        wTADCod
043100041015     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
043200041015     C                   IF        %found
043300041015     C                   Z-ADD     1             j
043400041015     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
043500041015     C                   IF        %found
043600041015     C                   EVAL      C_Causale = WDCTCLI(j)
043700041015     C                   SETOFF                                       50
043800041015     C                   ELSE
043900041015     C                   SETON                                        50
044000041015     C                   ENDIF
044100041015     C                   ELSE
044200041015     C                   SETON                                        50
044300041015     C                   ENDIF
044400041015     C                   MOVEL     tasLNA        C_CodDepPart
044500041015     C                   EVAL      C_TipoRecord   = '2'
044600041015     C  N50              EXSR      WRIREC
044700041015     C                   ENDIF
044800041111     C                   ENDIF
044900031103     C*
045000031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
045100031103     C                   EVAL      stbFTR = 'T'
045200031103     C                   UPDATE    tistb000
045300031103     C                   ENDIF
045400041015     C                   ENDIF
045500991027     C*
045600031103     C     KEYstb02P     READE     tistb02l
045700031103     C                   SETOFF                                       50
045800030325     C                   ENDDO
045900031103     C                   ENDIF
046000991027     C*
046100030325     C                   EVAL      wrkesito = '0'
046200991027     C*
046300910830     C                   ENDSR
046400031103
046500031103
046600031103
046700031103     C     wrirec        BEGSR
046800031103     C*
046900031103     C                   EVAL      C_CodVettore   = 'BA'
047000031103     C                   MOVEL     tasRMN        C_IDSped
047100031103     C                   MOVEL     *all'0'       C_DataTrasm
047200031103     C                   MOVEL     datcor        C_DataTrasm
047300041015     C  N60              MOVEL     stbDAS        C_DataEvento
047400041015     C  N60              MOVE      stbORS        C_DataEvento
047500031103     C                   EVAL      C_RifSpedVet   = DS_KEYBOLLA
047600031103     C                   EVAL      C_DataSped     = DS_DATABOLLA
047700031103     C                   MOVEL     stbKSC        C_CodCliente
047800031103     C*
047900031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
048000031103     C                   EVAL      VAWDTA = C_DS_Cli
048100031103     C                   WRITE     TIVAW000
048200031103     C*
048300031103     C                   ENDSR
048400031103
048500031103
048600991027
048700991027      /TITLE Operazioni iniziali.
048800991027     C     *inzsr        BEGSR
048900991027     C*
049000991027     C     *ENTRY        PLIST
049100031103     C                   parm                    prmppi
049200991027     C     wrkesito      parm      wrkesito      prmesito
049300031103     C*
049400031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
049500031103     C                   EVAL      DSPPI = prmppi
049600031103     C*
049700031103     C* Calcola la data corrente
049800031103     C                   time                    wn14             14 0
049900031103     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
050000031103     C                   z-add     wn8           g08dat
050100031103     C                   z-add     *zeros        g08inv
050200031103     C                   movel     '0'           g08err
050300031103     C                   call      'XSRDA8'
050400031103     C                   parm                    wlbda8
050500031103     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
050600030709     C*
050700030709     C* Definizione chiavi
050800030924     C*
050900031103     C* Chiave su TISTB02L - Parziale
051000031103     C     KEYstb02P     KLIST
051100031104     C                   KFLD                    wKstbKSU
051200031103     C                   KFLD                    wKstbFTR
051300031103     C*
051400031103     C* Chiave su TITAS30C - Parziale
051500031103     C     KEYtas30P     KLIST
051600031103     C                   KFLD                    stbAAS
051700031103     C                   KFLD                    stbLNP
051800031103     C                   KFLD                    stbNRS
051900031103     C                   KFLD                    stbNSP
052000991027     C*
052100991027     C                   ENDSR
052200031103** CAUBAR - CAUSALI BARTOLINI
052300031103R
052400120216RS
052500031103T
052600031103001
052700031103002
052800031103003
052900031103004
053000031103005
053100031103006
053200031103007
053300031103008
053400031103009
053500031103012
053600031103014
053700031103016
053800031103017
053900031103018
054000031103019
054100031103020
054200031103021
054300031103023
054400031103024
054500031103025
054600031103026
054700031103027
054800031103034
054900031103035
055000031103036
055100031111022
055200051109RIC
055300060613DIR
055400060613A16
055500140128P
055600060613
055700060613
055800031103** CAUCLI - CAUSALI CLIENTE BUFFETTI
055900031103X20
056000120216X20
056100031103X72
056200031103X06
056300031103X04
056400031103X05
056500031103X09
056600031103X19
056700031103X26
056800031103X27
056900031103X08
057000031103X28
057100031103X30
057200031103X21
057300031103X13
057400031103X15
057500031103X16
057600031103X17
057700031103X18
057800031103X11
057900031103X62
058000031103X07
058100031103X62
058200031103X22
058300031103X04
058400031103X73
058500031103X38
058600031103X20
058700031111X56
058800051109X15
058900060613X78
059000060613X13
059100140128X23
059200060613
059300060613
059400041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
059500041015A
059600041015M
059700041015V
059800091222E
059900041015** WDCTCLI - CAUSALI CLIENTE BUFFETTI
060000041015X45
060100041015X45
060200041015X46
060300091222
