000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400110103     FAZORG01L  IF   E           K DISK
000500041015     FTNTBE01L  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700110103     FTNCSB03L  IF   E           K DISK
000800110103     FTISTB03L  UF   E           K DISK    commit
000900090323     FTIVAWWWT  UF A E             DISK    commit
001000031103
001100031103     D*-----------
001200031103     D* SCHIERE A PROGRAMMA
001300031103     D*-----------
001400080618     D CAUBAR          S             10    DIM(90) CTDATA PERRCD(1)
001500080618     D CAUCLI          S             11    DIM(90) CTDATA PERRCD(1)
001600080618     D DESCLI          S             80    DIM(90) CTDATA PERRCD(1)
001700091222     D WDCTBAR         S              1    DIM(4)  CTDATA PERRCD(1)
001800091222     D WDCTCLI         S             10    DIM(4)  CTDATA PERRCD(1)
001900110103     D WCOD9BAR        S             10    DIM(10) CTDATA PERRCD(1)
002000110103     D WCOD9CLI        S             11    DIM(10) CTDATA PERRCD(1)
002100041015
002200041015     D*-----------
002300041015     D* SCHIERE
002400041015     D*-----------
002500041015     D skTADCod        S              2    DIM(500)
002600041015     D skTADRag        S              1    DIM(500)
002700041015     D jTAD            S              3  0 INZ(*zeros)
002800030924
002900041015     D*-----------
003000041015     D* VARIABILI D WRK
003100041015     D*-----------
003200031103     D prmppi          S             50
003300030924     D prmesito        S              1
003400030924     D wrkesito        S                   like(prmesito)
003500031103     D j               S              2  0
003600041015     D wTADCod         S              2
003700041015
003800041015     D*-----------
003900041015     D* DEFINIZIONE DS ESTERNE
004000041015     D*-----------
004100041015     D DTAD          E DS
004200031103
004300031103     D*-----------
004400031103     D* VARIABILI RIFERITE AL DATA-BASE
004500031103     D*-----------
004600080618     D wKstbKSU        S                   like(stbKSU)
004700080618     D wKstbFTR        S                   like(stbFTR)
004800110103     D wCAS            S                   like(csbCAS)
004900110103     D wVCA            S                   like(csbVCA)
005000030924
005100110103
005200110103     D*----------
005300110103     D* DS CLIENTE SHIPMENT'S STATUS
005400110103     D*----------
005500110103     D DS_Cli_Status   DS
005600110103     D  DS_CS_DataSpe                 8  0 inz
005700110103     D  DS_CS_NumSpe                 15    inz
005800110103     D  DS_CS_DocCli                 15  0 inz
005900110103     D  DS_CS_LnpDesc                40    inz
006000110103     D  DS_CS_CodCbo                  2    inz
006100110103     D  DS_CS_LnaDesc                40    inz
006200110103     D  DS_CS_DestRag                40    inz
006300110103     D  DS_CS_DestPrv                 5    inz
006400110103     D  DS_CS_CodTsp                  1    inz
006500110103     D  DS_CS_ImpAss                 13  3 inz
006600110103     D  DS_CS_ImpAssD                 3    inz
006700110103     D  DS_CS_Ncl                     5  0 inz
006800110103     D  DS_CS_Pkb                     7  1 inz
006900110103     D  DS_CS_Vlb                     5  3 inz
007000110103     D  DS_CS_QtaFat                 13  3 inz
007100110103     D  DS_CS_TicCas                  2    inz
007200110103     D  DS_CS_ImpCas                 13  3 inz
007300110103     D  DS_CS_ImpCasD                 3    inz
007400110103     D  DS_CS_Ffd                     1    inz
007500110103     D  DS_CS_Note                  100    inz
007600110103     D  DS_CS_EveData                 8  0 inz
007700110103     D  DS_CS_EveOra                  4  0 inz
007800110103     D  DS_CS_EveCod                  1    inz
007900110103
008000031103
008100031103     D*-----------
008200031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
008300031103     D*-----------
008400031103     D DSPPI           DS
008500080618     D  w§PPIKSU                      7  0
008600031103
008700030924
008800030325
008900030924     C* MAIN/
009000041015     C*
009100041015     C* Carica i dati tabellati
009200041015     C                   EXSR      cartab
009300041015     C*
009400041015     C* Esegue traduzione
009500991027     C                   EXSR      traduci
009600921023     C                   SETON                                        LR
009700041015
009800041015
009900041015
010000041015     C     cartab        BEGSR
010100041015     C*
010200041015     C* Caricamento tipi anomalia C/A
010300041015     C     'TAD'         SETLL     tntbe01l
010400041015     C                   IF        %equal(tntbe01l)
010500041015     C     'TAD'         READE     tntbe01l
010600041015     C                   DOW       not %eof(tntbe01l)
010700041015     C                   EVAL      DTAD = tbeUNI
010800041015     C                   ADD       1             jTAD
010900041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
011000041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
011100041015     C     'TAD'         READE     tntbe01l
011200041015     C                   ENDDO
011300041015     C                   ENDIF
011400041015     C*
011500041015     C                   ENDSR
011600991027
011700030924
011800030924
011900991027     C     traduci       BEGSR
012000031103     C*
012100031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
012200031103     C* di traduzione (trattasi d "post-traduzione")
012300031104     C                   EVAL      wKstbKSU = w§PPIKSU
012400031103     C                   EVAL      wKstbFTR = *blanks
012500110103     C     KEYstb03P     SETLL     tistb03l
012600110103     C                   IF        %equal(tistb03l)
012700110103     C     KEYstb03P     READE     tistb03l
012800991027     C*
012900031103     C* Ciclo x tuttii record del cliente da trasmettere
013000110103     C                   DOW       not %eof(tistb03l)
013100031014     C*
013200030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
013300110103     C                   RESET                   DS_Cli_Status
013400080618     C*
013500080618     C                   SETON                                        50
013600031103     C*
013700031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
013800031103     C* deve restituire al cliente nel file tradotto
013900031103     C                   IF        stbFTR = *blanks                             * ridondante
014000110103     C*
014100110103     C* Gestisco solamente gli stati standard
014200110103     C                   IF         stbTIS =  '1'                    OR
014300110103     C                             (stbTIS <> '1'  AND stbPRS = '1') OR
014400110103     C                             (stbTIS <> '1'  AND stbPRS = '3')
014500110103     C*
014600110103     C                   SETOFF                                       50
014700031103     C*
014800031103     C* Innanzitutto chaino la bolla direttamente su TITAS
014900031103     C     KEYtas30P     CHAIN     titas30c
015000031103     C                   IF        %found(titas30c)
015100110103     C*
015200110103     C                   clear                   wCAS
015300110103     C                   clear                   wVCA
015400110103     C*
015500110103     C* Quindi verifico subito esistenza contrassegno
015600110103     C     KEYcsb03C     CHAIN     tncsb03l
015700110103     C                   IF        %found(tncsb03l)
015800110103     C                   eval      wCAS = csbCAS
015900110103     C                   eval      wVCA = csbVCA
016000110103     C                   ENDIF
016100080618     C*
016200110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
016300110103     C                   IF        stbTIS = '1'
016400031103     C                   Z-ADD     1             j
016500031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
016600110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
016700080618     C                   IF        CAUCLI(j) <> '*'
016800110103     C                   EVAL      DS_CS_EveCod = CAUCLI(j)
016900110103     C                   IF        DS_CS_EveCod = 'Z'
017000110103     C                   EVAL      DS_CS_Note   = DESCLI(j)
017100110103     C                   ENDIF
017200080618     C                   ENDIF
017300031103     C                   ELSE
017400031103     C                   SETON                                        50
017500031103     C                   ENDIF
017600110103     C                   ENDIF
017700110103     C*
017800110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
017900110103     C                   IF        stbTIS <> '1'
018000110103     C                   Z-ADD     1             j
018100110103     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
018200110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
018300110103     C                   EVAL      DS_CS_EveCod = WCOD9CLI(j)
018400110103     C                   ELSE
018500110103     C                   SETON                                        50
018600110103     C                   ENDIF
018700110103     C                   ENDIF
018800041015     C*
018900080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
019000080618     C                   IF        stbPRS =  '5'
019100080618     C*
019200080618     C                   SETOFF                                       50
019300080618     C*
019400080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
019500080618     C                   Z-ADD     1             jTAD
019600080618     C                   MOVEL(P)  stbCOS        wTADCod
019700080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
019800080618     C                   IF        %found
019900080618     C                   Z-ADD     1             j
020000080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
020100080618     C                   IF        %found
020200110103     C                   EVAL      DS_CS_EveCod = WDCTCLI(j)
020300080618     C                   ELSE
020400080618     C                   SETON                                        50
020500080618     C                   ENDIF
020600080618     C                   ELSE
020700080618     C                   SETON                                        50
020800080618     C                   ENDIF
020900080618     C                   ENDIF
021000080618     C*
021100080618     C* Scarico il buffer d output
021200080618     C  N50              EXSR      WRIREC
021300110103     C*
021400110103     C                   ENDIF
021500110103     C                   ENDIF
021600110103     C                   ENDIF
021700031103     C*
021800031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
021900031103     C                   EVAL      stbFTR = 'T'
022000031103     C                   UPDATE    tistb000
022100991027     C*
022200110103     C     KEYstb03P     READE     tistb03l
022300031103     C                   SETOFF                                       50
022400030325     C                   ENDDO
022500031103     C                   ENDIF
022600991027     C*
022700030325     C                   EVAL      wrkesito = '0'
022800991027     C*
022900910830     C                   ENDSR
023000031103
023100031103
023200031103
023300110103     C     wrirec        BEGSR
023400031103     C*
023500110103     C                   eval      DS_CS_DataSpe = tasAAS*10000+tasMGS
023600110103     C                   eval      DS_CS_NumSpe  = %editc(tasLNP:'X')+
023700110103     C                                             %editc(tasNRS:'X')+
023800110103     C                                             %editc(tasNSP:'X')+tasTBL
023900110103     C                   eval      DS_CS_DocCli  = tasRMN
024000110103     C*
024100110103     C     tasLNP        chain     azorg01l
024200110103     C                   if        %found(azorg01l)
024300110103     C                   eval      DS_CS_LnpDesc = orgDES
024400110103     C                   endif
024500110103     C*
024600110103     C                   select
024700110103     C                   when      %subst(tasTBL:1:1) = 'F' and
024800110103     C                             tasIAS = *zeros AND wCAS = *zeros
024900110103     C                   eval      DS_CS_CodCbo  = '1'
025000110103     C*
025100110103     C                   when      %subst(tasTBL:1:1) = 'F' and
025200110103     C                             tasIAS = *zeros AND wCAS > *zeros
025300110103     C                   eval      DS_CS_CodCbo  = '2'
025400110103     C*
025500110103     C                   when      %subst(tasTBL:1:1) = 'F' and
025600110103     C                             tasIAS > *zeros AND wCAS = *zeros
025700110103     C                   eval      DS_CS_CodCbo  = '3'
025800110103     C*
025900110103     C                   when      %subst(tasTBL:1:1) = 'F' and
026000110103     C                             tasIAS > *zeros AND wCAS > *zeros
026100110103     C                   eval      DS_CS_CodCbo  = '4'
026200110103     C*
026300110103     C                   other
026400110103     C                   eval      DS_CS_CodCbo  = '5'
026500110103     C                   endsl
026600110103     C*
026700110103     C     tasLNA        chain     azorg01l
026800110103     C                   if        %found(azorg01l)
026900110103     C                   eval      DS_CS_LnaDesc = orgDES
027000110103     C                   endif
027100110103     C*
027200110103     C                   eval      DS_CS_DestRag = tasRSD
027300110103     C*
027400110103     C                   select
027500110103     C                   when      tasNZD = *blanks
027600110103     C                   eval      DS_CS_DestPrv = tasPRD
027700110103     C*
027800110103     C                   when      tasNZD <> *blanks
027900110103     C                   eval      DS_CS_DestPrv = tasNZD
028000110103     C                   endsl
028100110103     C*
028200110224     C* Considerazioni su codice tariffa e tipo servizio
028300110103     C                   select
028400110224     C                   when      tasCTR = 000 and tasTSP = 'C'
028500120404     C                   eval      DS_CS_CodTsp  = 'A'
028600110224     C*
028700110224     C                   when      tasCTR = 010 and tasTSP = 'C'
028800120404     C                   eval      DS_CS_CodTsp  = 'B'
028900120404     C*
029000120404     C                   when      tasCTR = 100 and tasTSP = 'C'
029100120404     C                   eval      DS_CS_CodTsp  = 'C'
029200110224     C*
029300110224     C                   when      tasCTR = 300 and tasTSP = 'C'
029400120404     C                   eval      DS_CS_CodTsp  = 'D'
029500120404     C*
029600120404     C                   when      tasCTR = 301 and tasTSP = 'C'
029700120404     C                   eval      DS_CS_CodTsp  = 'E'
029800110224     C*
029900120404     C                   when      tasCTR = 000 and tasTSP = 'E'
030000120404     C                   eval      DS_CS_CodTsp  = 'F'
030100110224     C*
030200120404     C                   when      tasCTR = 010 and tasTSP = 'E'
030300120404     C                   eval      DS_CS_CodTsp  = 'G'
030400110224     C*
030500120404     C                   when      tasCTR = 300 and tasTSP = 'E'
030600120404     C                   eval      DS_CS_CodTsp  = 'H'
030700120404     C*
030800120404     C                   when      tasCTR = 000 and tasTSP = 'D'
030900120404     C                   eval      DS_CS_CodTsp  = 'I'
031000120404     C*
031100120404     C                   when      tasCTR = 120 and tasTSP = 'D'
031200120404     C                   eval      DS_CS_CodTsp  = 'L'
031300110224     C*
031400110224     C                   other
031500110224     C                   eval      DS_CS_CodTsp  = 'X'
031600110103     C                   endsl
031700110103     C*
031800110103     C                   eval      DS_CS_ImpAss  = tasIAS
031900110103     C                   eval      DS_CS_ImpAssD = tasVAS
032000110103     C                   eval      DS_CS_Ncl     = tasNCL
032100110103     C                   eval      DS_CS_Pkb     = tasPKB
032200110103     C                   eval      DS_CS_Vlb     = tasVLB
032300110103     C                   eval      DS_CS_QtaFat  = tasQFT
032400110103     C*
032500110103     C                   if        wCAS > *zero
032600110103     C                   eval      DS_CS_TicCas  = 'AB'
032700110103     C                   eval      DS_CS_ImpCas  = wCAS
032800110103     C                   eval      DS_CS_ImpCasD = wVCA
032900110103     C                   endif
033000110103     C*
033100110103     C                   eval      DS_CS_Ffd     = tasFFD
033200110103     C*
033300110103     C                   eval      DS_CS_EveData = stbDAS
033400110103     C                   eval      DS_CS_EveOra  = stbORS
033500031103     C*
033600031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
033700110103     C                   EVAL      VAWDTA = DS_Cli_Status
033800031103     C                   WRITE     TIVAW000
033900031103     C*
034000031103     C                   ENDSR
034100031103
034200031103
034300991027
034400991027      /TITLE Operazioni iniziali.
034500991027     C     *inzsr        BEGSR
034600991027     C*
034700991027     C     *ENTRY        PLIST
034800031103     C                   parm                    prmppi
034900991027     C     wrkesito      parm      wrkesito      prmesito
035000031103     C*
035100031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
035200031103     C                   EVAL      DSPPI = prmppi
035300030709     C*
035400030709     C* Definizione chiavi
035500030924     C*
035600110103     C* Chiave su TISTB03L - Parziale
035700110103     C     KEYstb03P     KLIST
035800031104     C                   KFLD                    wKstbKSU
035900031103     C                   KFLD                    wKstbFTR
036000031103     C*
036100031103     C* Chiave su TITAS30C - Parziale
036200031103     C     KEYtas30P     KLIST
036300031103     C                   KFLD                    stbAAS
036400031103     C                   KFLD                    stbLNP
036500031103     C                   KFLD                    stbNRS
036600031103     C                   KFLD                    stbNSP
036700110103     C*
036800110103     C* Chiave su TNCSB03L - Completa
036900110103     C     KEYcsb03C     KLIST
037000110103     C                   KFLD                    stbAAS
037100110103     C                   KFLD                    stbLNP
037200110103     C                   KFLD                    stbNRS
037300110103     C                   KFLD                    stbNSP
037400991027     C*
037500991027     C                   ENDSR
037600110103** CAUBAR - CAUSALI BARTOLINI
037700110103A                       L.AVVISO
037800110103ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
037900110103AVV                     LASCIATO AVVISO 1GG
038000110103AV2                     LASCIATO AVVISO 2GG
038100110103AV3                     LASCIATO AVVISO 3GG
038200110103AV5                     LASCIATO AVVISO 5GG
038300110103AV7                     LASCIATO AVVISO 7GG
038400110103AV9                     LASCIATO AVVISO 9 GIORNI
038500110103A16                     L.A. RIMANDA LO SVINCOLO
038600110103A23                     L.A. CHIUSO FINO AL
038700110103CPO                     CAMBIO PORTO
038800110103DAN                     COLLO/I DANNEGGIATO/I
038900110103DIR                     DIROTTAMENTO
039000110103F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
039100110103G                       GIACENZA GENERICA TEMPORANEA
039200110103GEN                     GIACENZA GENERICA
039300110103G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
039400110103G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
039500110103G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
039600110103G09                     ATTENDERE 9 GIORNI
039700110103IDD                     CHIUSURA SPEDIZIONE CON IDD
039800110103MAN                     COLLO/I MANCANTE/I
039900110103MIC                     MESSA IN CONSEGNA
040000110103N                       NON FATTA
040100110103NAV                     COLLO SCONDIZIONATO
040200110103NIC                     TOLTA DALLA CONSEGNA
040300110103P                       CONSEGNA PARZIALE
040400110103PAT                     FESTIVITA PATRONALE
040500110103R                       RISERVA
040600110103RDC                     RIPRISTINO
040700110103RFD                     RIFIUTATO
040800110103RFM                     RIFIUTATO
040900110103RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
041000110103RIP                     RIPRISTINO
041100110103RS                      RISERVA
041200110103T                       CHIUSO PER TURNO
041300110103WA                      AMMANCO SENZA EVENTO
041400110103WAE                     AMMANCO CON EVENTO
041500110103WM                      MANCANZA SENZA EVENTO
041600110103WME                     MANCANZA CON EVENTO
041700110103WV                      AVARIA SENZA EVENTO
041800110103WVE                     AVARIA CON EVENTO
041900110103ZAL                     ALLUVIONE
042000110103ZBS                     BLOCCO STRADALE
042100110103ZFR                     FESTIVITÀ REGIONALE
042200110103ZMM                     MARE MOLTO MOSSO
042300110103ZMP                     MANIFESTAZIONE PUBBLICA
042400110103ZNV                     NEVICATA ECCEZIONALE
042500110103ZSC                     SCIOPERO
042600110103001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
042700110103002                     MERCE NON ORDINATA O NON CONFORME
042800110103003                     MERCE SPEDITA IN RITARDO
042900110103004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
043000110103005                     MERCE SPEDITA CON TROPPO ANTICIPO
043100110103006                     MERCE RESA SENZA AUTORIZZAZIONE
043200110103007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
043300110103008                     DESTINATARIO NON PAGA IL C/ASSEGNO
043400110103009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
043500110103010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
043600110103012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
043700110103013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
043800110103014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
043900110103016                     DESTINATARIO RIMANDA LO SVINCOLO AL
044000110103017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
044100110103018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
044200110103019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
044300110103020                     DESTINATARIO RISULTA TRASFERITO
044400110103021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
044500110103022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
044600110103023                     IL DESTINATARIO E' CHIUSO FINO AL
044700110103024                     IL DESTINATARIO E' CHIUSO PER FERIE
044800110103025                     IL DESTINATARIO E' CHIUSO
044900110103026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
045000110103027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
045100110103028                     ESERCIZIO NON ANCORA IN ATTIVITA
045200110103029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
045300110103030                     IL DESTINATARIO RIFIUTA LA MERCE
045400110103031                     IL DESTINATARIO NON INTENDE
045500110103032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
045600110103033                     IL DESTINATARIO RICHIEDE
045700110103034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
045800110103035                     DOCUMENTI INCOMPLETI O MANCANTI
045900110103036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
046000110103037                     RIFIUTA LA CONSEGNA TASSATIVA
046100110103100                     -
046200110103101                     -
046300110103999                     -
046400110103CONSOK                  CONSEGNA OK
046500110103RESO                    RESO
046600110103
046700110103** CAUCLI - CAUSALI CLIENTE
046800110103A                       L.AVVISO
046900110103A                       AVVISATO-RIMANDA SVINCOLO 1GG
047000110103A                       LASCIATO AVVISO 1GG
047100110103A                       LASCIATO AVVISO 2GG
047200110103A                       LASCIATO AVVISO 3GG
047300110103A                       LASCIATO AVVISO 5GG
047400110103A                       LASCIATO AVVISO 7GG
047500110103A                       LASCIATO AVVISO 9 GIORNI
047600110103A                       L.A. RIMANDA LO SVINCOLO
047700110103A                       L.A. CHIUSO FINO AL
047800110103Z                       CAMBIO PORTO
047900110103S                       COLLO/I DANNEGGIATO/I
048000110103Z                       DIROTTAMENTO
048100110103Z                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
048200110103G                       GIACENZA GENERICA TEMPORANEA
048300110103G                       GIACENZA GENERICA
048400110103Z                       ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
048500110103Z                       ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
048600110103Z                       ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
048700110103Z                       ATTENDERE 9 GIORNI
048800110103D                       CHIUSURA SPEDIZIONE CON IDD
048900110103D                       COLLO/I MANCANTE/I
049000110103M                       MESSA IN CONSEGNA
049100110103Z                       NON FATTA
049200110103S                       COLLO SCONDIZIONATO
049300110103Z                       TOLTA DALLA CONSEGNA
049400110103Z                       CONSEGNA PARZIALE
049500110103Z                       FESTIVITA PATRONALE
049600110103Z                       RISERVA
049700110103Z                       RIPRISTINO
049800110103Z                       RIFIUTATO
049900110103Z                       RIFIUTATO
050000110103A                       RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
050100110103Z                       RIPRISTINO
050200110103Z                       RISERVA
050300110103A                       CHIUSO PER TURNO
050400110103D                       AMMANCO SENZA EVENTO
050500110103D                       AMMANCO CON EVENTO
050600110103D                       MANCANZA SENZA EVENTO
050700110103D                       MANCANZA CON EVENTO
050800110103S                       AVARIA SENZA EVENTO
050900110103S                       AVARIA CON EVENTO
051000110103Z                       ALLUVIONE
051100110103Z                       BLOCCO STRADALE
051200110103Z                       FESTIVITÀ REGIONALE
051300110103Z                       MARE MOLTO MOSSO
051400110103Z                       MANIFESTAZIONE PUBBLICA
051500110103Z                       NEVICATA ECCEZIONALE
051600110103Z                       SCIOPERO
051700110103Z                       RIFIUTA SENZA SPECIFICARE IL MOTIVO
051800110103Z                       MERCE NON ORDINATA O NON CONFORME
051900110103Z                       MERCE SPEDITA IN RITARDO
052000110103Z                       MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
052100110103Z                       MERCE SPEDITA CON TROPPO ANTICIPO
052200110103Z                       MERCE RESA SENZA AUTORIZZAZIONE
052300110103Z                       DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
052400110103I                       DESTINATARIO NON PAGA IL C/ASSEGNO
052500110103Z                       CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
052600110103I                       DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
052700110103Z                       IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
052800110103I                       DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
052900110103Z                       DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
053000110103Z                       DESTINATARIO RIMANDA LO SVINCOLO AL
053100110103A                       DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
053200110103Z                       DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
053300110103Z                       DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
053400110103Z                       DESTINATARIO RISULTA TRASFERITO
053500110103Z                       DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
053600110103Z                       INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
053700110103A                       IL DESTINATARIO E' CHIUSO FINO AL
053800110103A                       IL DESTINATARIO E' CHIUSO PER FERIE
053900110103A                       IL DESTINATARIO E' CHIUSO
054000110103Z                       IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
054100110103Z                       DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
054200110103A                       ESERCIZIO NON ANCORA IN ATTIVITA
054300110103Z                       IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
054400110103Z                       IL DESTINATARIO RIFIUTA LA MERCE
054500110103Z                       IL DESTINATARIO NON INTENDE
054600110103Z                       FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
054700110103Z                       IL DESTINATARIO RICHIEDE
054800110103Z                       SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
054900110103Z                       DOCUMENTI INCOMPLETI O MANCANTI
055000110103Z                       IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
055100110103Z                       RIFIUTA LA CONSEGNA TASSATIVA
055200110103NON INVIARE             -
055300110103NON INVIARE             -
055400110103NON INVIARE             -
055500110103C                       CONSEGNA OK
055600110103T                       RESO
055700110103
055800110103** DESCLI - DESCRIZIONI CAUSALI CLIENTE
055900110103L.AVVISO
056000110103AVVISATO-RIMANDA SVINCOLO 1GG
056100110103LASCIATO AVVISO 1GG
056200110103LASCIATO AVVISO 2GG
056300110103LASCIATO AVVISO 3GG
056400110103LASCIATO AVVISO 5GG
056500110103LASCIATO AVVISO 7GG
056600110103LASCIATO AVVISO 9 GIORNI
056700110103L.A. RIMANDA LO SVINCOLO
056800110103L.A. CHIUSO FINO AL
056900110103CAMBIO PORTO
057000110103COLLO/I DANNEGGIATO/I
057100110103DIROTTAMENTO
057200110103FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
057300110103GIACENZA GENERICA TEMPORANEA
057400110103GIACENZA GENERICA
057500110103ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
057600110103ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
057700110103ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
057800110103ATTENDERE 9 GIORNI
057900110103CHIUSURA SPEDIZIONE CON IDD
058000110103COLLO/I MANCANTE/I
058100110103MESSA IN CONSEGNA
058200110103NON FATTA
058300110103COLLO SCONDIZIONATO
058400110103TOLTA DALLA CONSEGNA
058500110103CONSEGNA PARZIALE
058600110103FESTIVITA PATRONALE
058700110103RISERVA
058800110103RIPRISTINO
058900110103RIFIUTATO
059000110103RIFIUTATO
059100110103RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
059200110103RIPRISTINO
059300110103RISERVA
059400110103CHIUSO PER TURNO
059500110103AMMANCO SENZA EVENTO
059600110103AMMANCO CON EVENTO
059700110103MANCANZA SENZA EVENTO
059800110103MANCANZA CON EVENTO
059900110103AVARIA SENZA EVENTO
060000110103AVARIA CON EVENTO
060100110103ALLUVIONE
060200110103BLOCCO STRADALE
060300110103FESTIVITÀ REGIONALE
060400110103MARE MOLTO MOSSO
060500110103MANIFESTAZIONE PUBBLICA
060600110103NEVICATA ECCEZIONALE
060700110103SCIOPERO
060800110103RIFIUTA SENZA SPECIFICARE IL MOTIVO
060900110103MERCE NON ORDINATA O NON CONFORME
061000110103MERCE SPEDITA IN RITARDO
061100110103MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
061200110103MERCE SPEDITA CON TROPPO ANTICIPO
061300110103MERCE RESA SENZA AUTORIZZAZIONE
061400110103DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
061500110103DESTINATARIO NON PAGA IL C/ASSEGNO
061600110103CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
061700110103DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
061800110103IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
061900110103DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
062000110103DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
062100110103DESTINATARIO RIMANDA LO SVINCOLO AL
062200110103DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
062300110103DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
062400110103DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
062500110103DESTINATARIO RISULTA TRASFERITO
062600110103DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
062700110103INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
062800110103IL DESTINATARIO E' CHIUSO FINO AL
062900110103IL DESTINATARIO E' CHIUSO PER FERIE
063000110103IL DESTINATARIO E' CHIUSO
063100110103IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
063200110103DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
063300110103ESERCIZIO NON ANCORA IN ATTIVITA
063400110103IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
063500110103IL DESTINATARIO RIFIUTA LA MERCE
063600110103IL DESTINATARIO NON INTENDE
063700110103FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
063800110103IL DESTINATARIO RICHIEDE
063900110103SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
064000110103DOCUMENTI INCOMPLETI O MANCANTI
064100110103IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
064200110103RIFIUTA LA CONSEGNA TASSATIVA
064300110103-
064400110103-
064500110103-
064600110103CONSEGNA OK
064700110103RESO
064800110103
064900041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
065000110103A                                                                                            1
065100110103M                                                                                            2
065200110103V                                                                                            3
065300110103E                                                                                            4
065400080901** WDCTCLI - CAUSALI CLIENTE
065500110103D                                                                                            1
065600110103D                                                                                            2
065700110103D                                                                                            3
065800110103D                                                                                            4
065900110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
066000110103INCARICO1                                                                                    1
066100110103INCARICO2                                                                                    2
066200110103CONSOK1                                                                                      3
066300110103CONSOK2                                                                                      4
066400110103GIACNODIS1                                                                                   3
066500110103GIACNODIS2                                                                                   4
066600110103GIACSIDIS1                                                                                   4
066700110103GIACSIDIS2                                                                                   4
066800110103                                                                                             4
066900110103                                                                                             4
067000110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
067100110103P                                                                                            1
067200110103P                                                                                            2
067300110103NON INVIARE                                                                                  3
067400110103NON INVIARE                                                                                  4
067500110103G
067600110103G
067700110103G                                                                                            4
067800110103G                                                                                            4
067900110103NON INVIARE                                                                                  4
068000110103NON INVIARE                                                                                  4
