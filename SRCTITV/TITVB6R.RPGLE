000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400040216     FTABEL00F  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600050310     FTIGCP51L  IF   E           K DISK
000700090323     FTISTB02L  UF   E           K DISK    commit
000800090323     FTIVAWWWT  UF A E             DISK    commit
000900030924
001000031103     D prmppi          S             50
001100030924     D prmesito        S              1
001200030924     D wrkesito        S                   like(prmesito)
001300040216
001400040216     D ds2a          e ds
001500080903     D sk2Agiac        s              3    dim(200)
001600080903     D sk2Alav         s              3    dim(200)
001700031103
001800031103     D*-----------
001900031103     D* VARIABILI RIFERITE AL DATA-BASE
002000031103     D*-----------
002100031104     D wKstbKSU        S                   like(stbKSU)
002200031103     D wKstbFTR        S                   like(stbFTR)
002300030924
002400030924     D*-----------
002500040503     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO DEL CLIENTE
002600030924     D*-----------
002700030925     D C_DS_Cli        DS
002800040503     D  C_RifMit                     10    INZ(*blanks)
002900040503     D  C_NumCol                      5    INZ(*zeros)
003000040503     D  C_Filler1                     7    INZ(*blanks)
003100040503     D  C_Filler2                     5    INZ(*zeros)
003200040503     D  C_PesoKg                      7    INZ('0000.00')
003300040503     D  C_Filler3                     7    INZ('0000.00')
003400040503     D  C_VolumeDc                    8    INZ('00000.00')
003500040503     D  C_Filler4                    12    INZ('000000000.00')
003600040503     D  C_Filler5                     5    INZ(*zeros)
003700040503     D  C_Filler6                     5    INZ(*zeros)
003800040503     D  C_CodCliente                  7    INZ(*zeros)
003900040503     D  C_IDSped                     12    INZ(*zeros)
004000040503     D  C_DataSped                    6    INZ(*zeros)
004100040503     D  C_CodEsito                   10    INZ(*blanks)
004200040503     D  C_CodEvento                   3    INZ(*blanks)
004300040503     D  C_DataEvento                  6    INZ(*zeros)
004400040503     D  C_CodGiac                     3    INZ(*blanks)
004500040503     D  C_DataApGiac                  6    INZ(*zeros)
004600040503     D  C_DataFinGiac                 6    INZ(*zeros)
004700040503     D  C_DataReso                    6    INZ(*zeros)
004800040503     D  C_Filler7                     6    INZ(*zeros)
004900040503     D  C_Filler8                     5    INZ(*zeros)
005000040503     D  C_CodRitardo                  3    INZ(*blanks)
005100031103
005200031103     D*-----------
005300031103     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
005400031103     D*-----------
005500031103     D                 DS
005600040503     D  wAAS                   1      4  0
005700040503     D  wMGS                   5      8  0
005800040503     D DS_DATA                 1      8
005900040503
006000040503     D*-----------
006100040503     D* RIDEFINIZIONE CHIAVE BOLLA IN FORMATO ALFA
006200040503     D*-----------
006300040503     D                 DS
006400040503     D  TASLNP                 1      3  0
006500040503     D  TASNRS                 4      5  0
006600040503     D  TASNSP                 6     12  0
006700040503     D DS_KEYBOLLA             1     12
006800031103
006900031103     D*-----------
007000031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
007100031103     D*-----------
007200031103     D DSPPI           DS
007300031104     D  w§PPIKSU                      7  0
007400030924
007500030325
007600030924     C* MAIN/
007700040216     C                   EXSR      cartab
007800991027     C                   EXSR      traduci
007900921023     C                   SETON                                        LR
008000991027
008100030924
008200030924
008300991027     C     traduci       BEGSR
008400031103     C*
008500031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
008600031103     C* di traduzione (trattasi d "post-traduzione")
008700031104     C                   EVAL      wKstbKSU = w§PPIKSU
008800031103     C                   EVAL      wKstbFTR = *blanks
008900031103     C     KEYstb02P     SETLL     tistb02l
009000031103     C                   IF        %equal(tistb02l)
009100031103     C     KEYstb02P     READE     tistb02l
009200031103     C                   SETOFF                                       50
009300991027     C*
009400040205     C* Ciclo x tutti i record del cliente da trasmettere
009500031103     C                   DOW       not %eof(tistb02l)
009600031014     C*
009700030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
009800031014     C                   RESET                   C_DS_Cli
009900031103     C*
010000031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
010100031103     C* deve restituire al cliente nel file tradotto
010200031103     C                   IF        stbFTR = *blanks                             * ridondante
010300031103     C*
010400031103     C* Innanzitutto chaino la bolla direttamente su TITAS
010500031103     C     KEYtas30P     CHAIN     titas30c
010600031103     C                   IF        %found(titas30c)
010700031103     C*
010800040205     C* Gestisco unicamente gli stati "standard"
010900031103     C                   IF        stbTIS = '1'
011000040205     C*
011100040205     C* X stati provenienti dagli eventi
011200040205     C                   IF        stbPRS = '2'
011300040205     C*
011400040503     C* Eventi d NN consegna (esito cliente = '0')
011500040503     C* Verifico se l'evento d "NN consegna" è d tipo giacenza o d tipo lasciato avviso
011600040503     C* (in quanto sono gli unici tipi evento derivanti da 1 tentativo d consegna)
011700040216     C                   MOVEL     stbCOS        wCOS              3
011800040503     C* ...tipo "giacenza"
011900040503     C     wCOS          LOOKUP    sk2Agiac                               20
012000040216     C                   IF        *IN20 = *ON
012100040205     C* Valorizzo buffer record ritorno al cliente
012200040503     C                   EVAL      C_CodEsito = '1'
012300040503     C                   EVAL      C_CodEvento = wCOS
012400040503     C                   MOVE      stbDAS        DS_DATA
012500040503     C                   EVAL      C_DataEvento = %subst(DS_DATA:7:2)+
012600040503     C                                            %subst(DS_DATA:5:2)+
012700040503     C                                            %subst(DS_DATA:3:2)
012800040503     C*
012900040503     C* Se trattasi d evento d tipo giacenza reperisco i dato della giacenza...
013000050310     C* Costruisco la chiave sul file TIGCP51L
013100040503     C                   EVAL      gcpFRG = *zeros
013200040503     C* Reperisco la giacenza attiva relativa alla bolla corrente
013300050310     C     KEYgcp51      CHAIN     tigcp51l
013400050310     C                   IF        %found(tigcp51l)
013500040503     C* Se giacenza NN annullata la considero
013600040503     C                   IF        gcpATB = *blanks
013700040503     C                   MOVE      gcpDUR        DS_DATA
013800040503     C                   EVAL      C_DataApGiac = %subst(DS_DATA:7:2)+
013900040503     C                                            %subst(DS_DATA:5:2)+
014000040503     C                                            %subst(DS_DATA:3:2)
014100040503     C                   MOVE      gcpDCG        DS_DATA
014200040503     C                   EVAL      C_DataFinGiac= %subst(DS_DATA:7:2)+
014300040503     C                                            %subst(DS_DATA:5:2)+
014400040503     C                                            %subst(DS_DATA:3:2)
014500040503     C                   MOVE      gcpCMC        C_CodGiac
014600040503     C
014700040503     C                   ENDIF
014800040503     C                   ENDIF
014900040503     C*
015000040205     C                   EXSR      WRIREC
015100040216     C                   ENDIF
015200040503     C* ...tipo "lasciato avviso"
015300040503     C     wCOS          LOOKUP    sk2Alav                                20
015400040503     C                   IF        *IN20 = *ON
015500040503     C* Valorizzo buffer record ritorno al cliente
015600040503     C                   EVAL      C_CodEsito = '1'
015700040503     C                   EVAL      C_CodEvento = wCOS
015800040503     C                   MOVE      stbDAS        DS_DATA
015900040503     C                   EVAL      C_DataEvento = %subst(DS_DATA:7:2)+
016000040503     C                                            %subst(DS_DATA:5:2)+
016100040503     C                                            %subst(DS_DATA:3:2)
016200040503     C                   EXSR      WRIREC
016300040503     C                   ENDIF
016400040216     C                   ENDIF
016500040216     C*
016600040205     C* X stati provenienti dagli esiti
016700040205     C                   IF        stbPRS = '4'
016800040205     C*
016900040503     C* CONSEGNA OK (esito cliente = '0')
017000040205     C                   IF        stbCOS = 'CONSOK'
017100040205     C* Valorizzo buffer record ritorno al cliente
017200040503     C                   EVAL      C_CodEsito = '0'
017300040205     C                   EXSR      WRIREC
017400040205     C                   ENDIF
017500040205     C*
017600040503     C* RESO (esito cliente = '2')
017700040205     C                   IF        stbCOS = 'RESO'
017800040322     C* Scrivo il record d reso solo se il n° distinta consegna sulla bolla è <> 9999999
017900040322     C                   IF        tasNDC <> *all'9'
018000040205     C* Valorizzo buffer record ritorno al cliente
018100040503     C                   EVAL      C_CodEsito = '2'
018200040503     C                   MOVE      tasDCM        DS_DATA
018300040503     C                   EVAL      C_DataReso = %subst(DS_DATA:7:2)+
018400040503     C                                          %subst(DS_DATA:5:2)+
018500040503     C                                          %subst(DS_DATA:3:2)
018600040205     C                   EXSR      WRIREC
018700040322     C                   ENDIF
018800040205     C                   ENDIF
018900040205     C                   ENDIF
019000040210     C                   ENDIF
019100031103     C*
019200031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
019300031103     C                   EVAL      stbFTR = 'T'
019400031103     C                   UPDATE    tistb000
019500031103     C                   ENDIF
019600040205     C                   ENDIF
019700991027     C*
019800031103     C     KEYstb02P     READE     tistb02l
019900031103     C                   SETOFF                                       50
020000030325     C                   ENDDO
020100031103     C                   ENDIF
020200991027     C*
020300030325     C                   EVAL      wrkesito = '0'
020400991027     C*
020500910830     C                   ENDSR
020600031103
020700031103
020800031103
020900031103     C     wrirec        BEGSR
021000031103     C*
021100040503     C                   MOVE      tasRMN        C_RifMit
021200040503     C                   MOVE      tasNCL        C_NumCol
021300040503     C                   Z-ADD     tasPKB        wPesoKg           6 2
021400040503     C                   EVAL      C_PesoKg=%editw(wPesoKg:'    .  ')
021500040503     C                   Z-ADD     *zeros        wVolumeDc         7 2
021600040503     C                   EVAL      wVolumeDc = tasVLB * 1000
021700040503     C                   EVAL      C_VolumeDc=%editw(wVolumeDc:'     .  ')
021800040503     C                   MOVE      w§PPIKSU      C_CodCliente
021900040503     C                   EVAL      C_IDSped = DS_KEYBOLLA
022000040503     C                   Z-ADD     tasAAS        wAAS
022100040503     C                   Z-ADD     tasMGS        wMGS
022200040503     C                   EVAL      C_DataSped = %subst(DS_DATA:7:2)+
022300040503     C                                          %subst(DS_DATA:5:2)+
022400040503     C                                          %subst(DS_DATA:3:2)
022500031103     C*
022600031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
022700031103     C                   EVAL      VAWDTA = C_DS_Cli
022800031103     C                   WRITE     TIVAW000
022900031103     C*
023000031103     C                   ENDSR
023100040216
023200040216
023300040216     C     CARTAB        BEGSR
023400040216     C*
023500040503     C* Carico i codici eventi d tipo "giacenza" e "lasciato avviso" (ovvero queli significativi
023600040503     C* ai fini del tentativo d consegna).
023700040216     C                   clear                   sk2Agiac
023800040503     C                   clear                   sk2Alav
023900040216     C                   z-add     *zeros        i                 3 0
024000040503     C                   z-add     *zeros        j                 3 0
024100040216     C                   eval      tblKUT = 1
024200040216     C                   eval      tblCOD = '2A'
024300040216     C     KEYtabP       setll     tabel00f
024400040216     C     KEYtabP       reade     tabel00f
024500040216     C                   dow       not %eof(tabel00f)
024600040216     C                   movel(p)  tblUNI        ds2a
024700040216     C                   if        §2aFTC = 'G'
024800040216     C                   add       1             i
024900040216     C                   eval      sk2Agiac(i) = tblKEY
025000040216     C                   endif
025100040503     C                   if        §2aFTC = 'A'
025200040504     C                   add       1             j
025300040503     C                   eval      sk2Alav(j)  = tblKEY
025400040503     C                   endif
025500040216     C     KEYtabP       reade     tabel00f
025600040216     C                   enddo
025700040216     C*
025800040216     C                   ENDSR
025900031103
026000031103
026100991027
026200991027      /TITLE Operazioni iniziali.
026300991027     C     *inzsr        BEGSR
026400991027     C*
026500991027     C     *ENTRY        PLIST
026600031103     C                   parm                    prmppi
026700991027     C     wrkesito      parm      wrkesito      prmesito
026800031103     C*
026900031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
027000031103     C                   EVAL      DSPPI = prmppi
027100030709     C*
027200030709     C* Definizione chiavi
027300030924     C*
027400031103     C* Chiave su TISTB02L - Parziale
027500031103     C     KEYstb02P     KLIST
027600031104     C                   KFLD                    wKstbKSU
027700031103     C                   KFLD                    wKstbFTR
027800031103     C*
027900031103     C* Chiave su TITAS30C - Parziale
028000031103     C     KEYtas30P     KLIST
028100031103     C                   KFLD                    stbAAS
028200031103     C                   KFLD                    stbLNP
028300031103     C                   KFLD                    stbNRS
028400031103     C                   KFLD                    stbNSP
028500040503     C*
028600050310     C* Chiave su TIGCP51L - Completa
028700050310     C     KEYgcp51      KLIST
028800040503     C                   KFLD                    stbAAS
028900040503     C                   KFLD                    stbLNP
029000040503     C                   KFLD                    stbNRS
029100040503     C                   KFLD                    stbNSP
029200040503     C                   KFLD                    gcpFRG
029300040216     C*
029400040216     C* Chiave su TABEL00F - parziale
029500040216     C     KEYtabP       KLIST
029600040216     C                   KFLD                    tblKUT
029700040216     C                   KFLD                    tblCOD
029800991027     C*
029900991027     C                   ENDSR
