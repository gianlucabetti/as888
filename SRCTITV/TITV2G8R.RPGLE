000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400041015     FTNTBE01L  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600110103     FTISTB03L  UF   E           K DISK    commit
000700090323     FTIVAWWWT  UF A E             DISK    commit
000800031103
000900031103     D*-----------
001000031103     D* SCHIERE A PROGRAMMA
001100031103     D*-----------
001200080618     D CAUBAR          S             10    DIM(90) CTDATA PERRCD(1)
001300080618     D CAUCLI          S             11    DIM(90) CTDATA PERRCD(1)
001400080618     D DESCLI          S             80    DIM(90) CTDATA PERRCD(1)
001500091222     D WDCTBAR         S              1    DIM(4)  CTDATA PERRCD(1)
001600091222     D WDCTCLI         S             10    DIM(4)  CTDATA PERRCD(1)
001700110103     D WCOD9BAR        S             10    DIM(10) CTDATA PERRCD(1)
001800110103     D WCOD9CLI        S             11    DIM(10) CTDATA PERRCD(1)
001900041015
002000041015     D*-----------
002100041015     D* SCHIERE
002200041015     D*-----------
002300041015     D skTADCod        S              2    DIM(500)
002400041015     D skTADRag        S              1    DIM(500)
002500041015     D jTAD            S              3  0 INZ(*zeros)
002600030924
002700041015     D*-----------
002800041015     D* VARIABILI D WRK
002900041015     D*-----------
003000031103     D prmppi          S             50
003100030924     D prmesito        S              1
003200030924     D wrkesito        S                   like(prmesito)
003300031103     D j               S              2  0
003400041015     D wTADCod         S              2
003500041015
003600041015     D*-----------
003700041015     D* DEFINIZIONE DS ESTERNE
003800041015     D*-----------
003900041015     D DTAD          E DS
004000031103
004100031103     D*-----------
004200031103     D* VARIABILI RIFERITE AL DATA-BASE
004300031103     D*-----------
004400080618     D wKstbKSU        S                   like(stbKSU)
004500080618     D wKstbFTR        S                   like(stbFTR)
004600030924
004700110103
004800110103     D*----------
004900110103     D* DS CLIENTE SHIPMENT'S STATUS
005000110103     D*----------
005100110103     D DS_Cli_Status   DS
005200110705     D  DS_CS_StsCode                 2    inz
005300110705     D  DS_CS_SAPsys                  3    inz
005400110705     D  DS_CS_DlvNbr                 10    inz
005500110705     D  DS_CS_BokDate                 8    inz
005600110705     D  DS_CS_BokNote               250    inz
005700110705     D  DS_CS_DlvDate                 8    inz
005800110705     D  DS_CS_DlvNote               250    inz
005900110103
006000110705
006100110705     D*----------
006200110705     D* DS SCOMPOSIZIONE DATA
006300110705     D*----------
006400110705     D DS_DATE         DS                  inz
006500110705     D  DS_AAAA                       4
006600110705     D  DS_MM                         2
006700110705     D  DS_GG                         2
006800110705
006900031103
007000031103     D*-----------
007100031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
007200031103     D*-----------
007300031103     D DSPPI           DS
007400080618     D  w§PPIKSU                      7  0
007500031103
007600030924
007700030325
007800030924     C* MAIN/
007900110705     C                   SETOFF                                       40
008000041015     C*
008100041015     C* Carica i dati tabellati
008200041015     C                   EXSR      cartab
008300041015     C*
008400041015     C* Esegue traduzione
008500991027     C                   EXSR      traduci
008600110705     C*
008700110705     C* Alla fine se scritto qualcosa aggiunto il terminatore file
008800110705     C                   IF        *in40
008900110705     C                   EVAL      VAWDTA = 'EOF'
009000110705     C                   WRITE     TIVAW000
009100110705     C                   ENDIF
009200110705     C*
009300921023     C                   SETON                                        LR
009400041015
009500041015
009600041015
009700041015     C     cartab        BEGSR
009800041015     C*
009900041015     C* Caricamento tipi anomalia C/A
010000041015     C     'TAD'         SETLL     tntbe01l
010100041015     C                   IF        %equal(tntbe01l)
010200041015     C     'TAD'         READE     tntbe01l
010300041015     C                   DOW       not %eof(tntbe01l)
010400041015     C                   EVAL      DTAD = tbeUNI
010500041015     C                   ADD       1             jTAD
010600041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
010700041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
010800041015     C     'TAD'         READE     tntbe01l
010900041015     C                   ENDDO
011000041015     C                   ENDIF
011100041015     C*
011200041015     C                   ENDSR
011300991027
011400030924
011500030924
011600991027     C     traduci       BEGSR
011700031103     C*
011800031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
011900031103     C* di traduzione (trattasi d "post-traduzione")
012000031104     C                   EVAL      wKstbKSU = w§PPIKSU
012100031103     C                   EVAL      wKstbFTR = *blanks
012200110103     C     KEYstb03P     SETLL     tistb03l
012300110103     C                   IF        %equal(tistb03l)
012400110103     C     KEYstb03P     READE     tistb03l
012500991027     C*
012600031103     C* Ciclo x tuttii record del cliente da trasmettere
012700110103     C                   DOW       not %eof(tistb03l)
012800031014     C*
012900030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
013000110103     C                   RESET                   DS_Cli_Status
013100080618     C*
013200080618     C                   SETON                                        50
013300031103     C*
013400031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
013500031103     C* deve restituire al cliente nel file tradotto
013600031103     C                   IF        stbFTR = *blanks                             * ridondante
013700110103     C*
013800110103     C* Gestisco solamente gli stati standard
013900110103     C                   IF         stbTIS =  '1'                    OR
014000110103     C                             (stbTIS <> '1'  AND stbPRS = '1') OR
014100110103     C                             (stbTIS <> '1'  AND stbPRS = '3')
014200110103     C*
014300110103     C                   SETOFF                                       50
014400031103     C*
014500031103     C* Innanzitutto chaino la bolla direttamente su TITAS
014600031103     C     KEYtas30P     CHAIN     titas30c
014700031103     C                   IF        %found(titas30c)
014800080618     C*
014900110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
015000110103     C                   IF        stbTIS = '1'
015100031103     C                   Z-ADD     1             j
015200031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
015300110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
015400080618     C                   IF        CAUCLI(j) <> '*'
015500110705     C                   EVAL      DS_CS_StsCode = CAUCLI(j)
015600110705     C                   EVAL      DS_CS_DlvNote = DESCLI(j)
015700080618     C                   ENDIF
015800031103     C                   ELSE
015900031103     C                   SETON                                        50
016000031103     C                   ENDIF
016100110103     C                   ENDIF
016200110103     C*
016300110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
016400110103     C                   IF        stbTIS <> '1'
016500110103     C                   Z-ADD     1             j
016600110103     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
016700110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
016800110705     C                   EVAL      DS_CS_StsCode = WCOD9CLI(j)
016900110103     C                   ELSE
017000110103     C                   SETON                                        50
017100110103     C                   ENDIF
017200110103     C                   ENDIF
017300041015     C*
017400080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
017500110705     C***                IF        stbPRS =  '5'
017600080618     C*
017700110705     C***                SETOFF                                       50
017800080618     C*
017900080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
018000110705     C***                Z-ADD     1             jTAD
018100110705     C***                MOVEL(P)  stbCOS        wTADCod
018200110705     C***  wTADCod       LOOKUP    skTADCod(jTAD)                         70
018300110705     C***                IF        %found
018400110705     C***                Z-ADD     1             j
018500110705     C***  skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
018600110705     C***                IF        %found
018700110705     C***                EVAL      DS_CS_EveCod = WDCTCLI(j)
018800110705     C***                ELSE
018900110705     C***                SETON                                        50
019000110705     C***                ENDIF
019100110705     C***                ELSE
019200110705     C***                SETON                                        50
019300110705     C***                ENDIF
019400110705     C***                ENDIF
019500080618     C*
019600080618     C* Scarico il buffer d output
019700080618     C  N50              EXSR      WRIREC
019800110103     C*
019900110103     C                   ENDIF
020000110103     C                   ENDIF
020100110103     C                   ENDIF
020200031103     C*
020300031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
020400031103     C                   EVAL      stbFTR = 'T'
020500031103     C                   UPDATE    tistb000
020600991027     C*
020700110103     C     KEYstb03P     READE     tistb03l
020800031103     C                   SETOFF                                       50
020900030325     C                   ENDDO
021000031103     C                   ENDIF
021100991027     C*
021200030325     C                   EVAL      wrkesito = '0'
021300991027     C*
021400910830     C                   ENDSR
021500031103
021600031103
021700031103
021800110103     C     wrirec        BEGSR
021900110705     C*
022000110705     C                   SETON                                        40
022100110705     C*
022200110706     C                   EVAL      DS_CS_SAPsys  = 'P12'
022300110705     C                   EVAL      DS_CS_DlvNbr  = %trim(stbRMA)
022400110705     C*
022500110705     C                   IF        tasDCR > *zeros
022600110705     C                   MOVE(P)   tasDCR        DS_DATE
022700110705     C                   EVAL      DS_CS_BokDate = DS_GG + DS_MM + DS_AAAA
022800110705     C                   ELSE
022900110705     C                   MOVE(P)   stbDSP        DS_DATE
023000110705     C                   EVAL      DS_CS_BokDate = DS_GG + DS_MM + DS_AAAA
023100110705     C                   ENDIF
023200110705     C*
023300110705     C                   MOVE(P)   stbDAS        DS_DATE
023400110705     C                   EVAL      DS_CS_DlvDate = DS_GG + DS_MM + DS_AAAA
023500031103     C*
023600031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
023700110705     C                   EVAL      VAWDTA = %trim(DS_CS_StsCode) + ';' +
023800110705     C                                      %trim(DS_CS_SAPsys)  + ';' +
023900110705     C                                      %trim(DS_CS_DlvNbr)  + ';' +
024000110705     C                                      %trim(DS_CS_BokDate) + ';' +
024100110705     C                                      %trim(DS_CS_BokNote) + ';' +
024200110705     C                                      %trim(DS_CS_DlvDate) + ';' +
024300110706     C                                      %trim(DS_CS_DlvNote)
024400031103     C                   WRITE     TIVAW000
024500031103     C*
024600031103     C                   ENDSR
024700031103
024800031103
024900991027
025000991027      /TITLE Operazioni iniziali.
025100991027     C     *inzsr        BEGSR
025200991027     C*
025300991027     C     *ENTRY        PLIST
025400031103     C                   parm                    prmppi
025500991027     C     wrkesito      parm      wrkesito      prmesito
025600031103     C*
025700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
025800031103     C                   EVAL      DSPPI = prmppi
025900030709     C*
026000030709     C* Definizione chiavi
026100030924     C*
026200110103     C* Chiave su TISTB03L - Parziale
026300110103     C     KEYstb03P     KLIST
026400031104     C                   KFLD                    wKstbKSU
026500031103     C                   KFLD                    wKstbFTR
026600031103     C*
026700031103     C* Chiave su TITAS30C - Parziale
026800031103     C     KEYtas30P     KLIST
026900031103     C                   KFLD                    stbAAS
027000031103     C                   KFLD                    stbLNP
027100031103     C                   KFLD                    stbNRS
027200031103     C                   KFLD                    stbNSP
027300991027     C*
027400991027     C                   ENDSR
027500110103** CAUBAR - CAUSALI BARTOLINI
027600110705A                       L.AVVISO                                                     1
027700110705ARS                     AVVISATO-RIMANDA SVINCOLO 1GG                                2
027800110705AVV                     LASCIATO AVVISO 1GG                                          3
027900110705AV2                     LASCIATO AVVISO 2GG                                          4
028000110705AV3                     LASCIATO AVVISO 3GG                                          5
028100110705AV5                     LASCIATO AVVISO 5GG                                          6
028200110705AV7                     LASCIATO AVVISO 7GG                                          7
028300110705AV9                     LASCIATO AVVISO 9 GIORNI                                     8
028400110705A16                     L.A. RIMANDA LO SVINCOLO                                     9
028500110705A23                     L.A. CHIUSO FINO AL                                         10
028600110705CPO                     CAMBIO PORTO                                                11
028700110705DAN                     COLLO/I DANNEGGIATO/I                                       12
028800110705DIR                     DIROTTAMENTO                                                13
028900110705F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
029000110705G                       GIACENZA GENERICA TEMPORANEA                                15
029100110705GEN                     GIACENZA GENERICA                                           16
029200110705G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
029300110705G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
029400110705G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
029500110705G09                     ATTENDERE 9 GIORNI                                          20
029600110705IDD                     CHIUSURA SPEDIZIONE CON IDD                                 21
029700110705MAN                     COLLO/I MANCANTE/I                                          22
029800110705MIC                     MESSA IN CONSEGNA                                           23
029900110705N                       NON FATTA                                                   24
030000110705NAV                     COLLO SCONDIZIONATO                                         25
030100110705NIC                     TOLTA DALLA CONSEGNA                                        26
030200110705P                       CONSEGNA PARZIALE                                           27
030300110705PAT                     FESTIVITA PATRONALE                                         28
030400110705R                       RISERVA                                                     29
030500110705RDC                     RIPRISTINO                                                  30
030600110705RFD                     RIFIUTATO                                                   31
030700110705RFM                     RIFIUTATO                                                   32
030800110705RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
030900110705RIP                     RIPRISTINO                                                  34
031000110705RS                      RISERVA                                                     35
031100110705T                       CHIUSO PER TURNO                                            36
031200110705WA                      AMMANCO SENZA EVENTO                                        37
031300110705WAE                     AMMANCO CON EVENTO                                          38
031400110705WM                      MANCANZA SENZA EVENTO                                       39
031500110705WME                     MANCANZA CON EVENTO                                         40
031600110705WV                      AVARIA SENZA EVENTO                                         41
031700110705WVE                     AVARIA CON EVENTO                                           42
031800110705ZAL                     ALLUVIONE                                                   43
031900110705ZBS                     BLOCCO STRADALE                                             44
032000110705ZFR                     FESTIVITÀ REGIONALE                                         45
032100110705ZMM                     MARE MOLTO MOSSO                                            46
032200110705ZMP                     MANIFESTAZIONE PUBBLICA                                     47
032300110705ZNV                     NEVICATA ECCEZIONALE                                        48
032400110705ZSC                     SCIOPERO                                                    49
032500110705001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
032600110705002                     MERCE NON ORDINATA O NON CONFORME                           51
032700110705003                     MERCE SPEDITA IN RITARDO                                    52
032800110705004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
032900110705005                     MERCE SPEDITA CON TROPPO ANTICIPO                           54
033000110705006                     MERCE RESA SENZA AUTORIZZAZIONE                             55
033100110705007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
033200110705008                     DESTINATARIO NON PAGA IL C/ASSEGNO                          57
033300110705009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58
033400110705010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
033500110705012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
033600110705013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
033700110705014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
033800110705016                     DESTINATARIO RIMANDA LO SVINCOLO AL                         63
033900110705017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
034000110705018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65
034100110705019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
034200110705020                     DESTINATARIO RISULTA TRASFERITO                             67
034300110705021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68
034400110705022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
034500110705023                     IL DESTINATARIO E' CHIUSO FINO AL                           70
034600110705024                     IL DESTINATARIO E' CHIUSO PER FERIE                         71
034700110705025                     IL DESTINATARIO E' CHIUSO                                   72
034800110705026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
034900110705027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
035000110705028                     ESERCIZIO NON ANCORA IN ATTIVITA                            75
035100110705029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
035200110705030                     IL DESTINATARIO RIFIUTA LA MERCE                            77
035300110705031                     IL DESTINATARIO NON INTENDE                                 78
035400110705032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79
035500110705033                     IL DESTINATARIO RICHIEDE                                    80
035600110705034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
035700110705035                     DOCUMENTI INCOMPLETI O MANCANTI                             82
035800110705036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
035900110705037                     RIFIUTA LA CONSEGNA TASSATIVA                               84
036000110705100                     -                                                           85
036100110705101                     -                                                           86
036200110705999                     -                                                           87
036300110705CONSOK                  CONSEGNA OK                                                 88
036400110705RESO                    RESO                                                        89
036500110705                                                                                    90
036600110103** CAUCLI - CAUSALI CLIENTE
036700110705NON INVIARE             L.AVVISO                                                     1
036800110705NON INVIARE             AVVISATO-RIMANDA SVINCOLO 1GG                                2
036900110705NON INVIARE             LASCIATO AVVISO 1GG                                          3
037000110705NON INVIARE             LASCIATO AVVISO 2GG                                          4
037100110705NON INVIARE             LASCIATO AVVISO 3GG                                          5
037200110705NON INVIARE             LASCIATO AVVISO 5GG                                          6
037300110705NON INVIARE             LASCIATO AVVISO 7GG                                          7
037400110705NON INVIARE             LASCIATO AVVISO 9 GIORNI                                     8
037500110705NON INVIARE             L.A. RIMANDA LO SVINCOLO                                     9
037600110705NON INVIARE             L.A. CHIUSO FINO AL                                         10
037700110705NON INVIARE             CAMBIO PORTO                                                11
03780011070550                      COLLO/I DANNEGGIATO/I                                       12
037900110705NON INVIARE             DIROTTAMENTO                                                13
038000110705NON INVIARE             FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
038100110705NON INVIARE             GIACENZA GENERICA TEMPORANEA                                15
038200110705NON INVIARE             GIACENZA GENERICA                                           16
038300110705NON INVIARE             ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
038400110705NON INVIARE             ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
038500110705NON INVIARE             ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
038600110705NON INVIARE             ATTENDERE 9 GIORNI                                          20
03870011070550                      CHIUSURA SPEDIZIONE CON IDD                                 21
03880011070550                      COLLO/I MANCANTE/I                                          22
038900110705NON INVIARE             MESSA IN CONSEGNA                                           23
039000110705NON INVIARE             NON FATTA                                                   24
039100110705NON INVIARE             COLLO SCONDIZIONATO                                         25
039200110705NON INVIARE             TOLTA DALLA CONSEGNA                                        26
03930011070550                      CONSEGNA PARZIALE                                           27
039400110705NON INVIARE             FESTIVITA PATRONALE                                         28
03950011070550                      RISERVA                                                     29
03960011070501                      RIPRISTINO                                                  30
03970011070550                      RIFIUTATO                                                   31
03980011070550                      RIFIUTATO                                                   32
039900110705NON INVIARE             RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
04000011070501                      RIPRISTINO                                                  34
04010011070550                      RISERVA                                                     35
040200110705NON INVIARE             CHIUSO PER TURNO                                            36
040300110705NON INVIARE             AMMANCO SENZA EVENTO                                        37
040400110705NON INVIARE             AMMANCO CON EVENTO                                          38
040500110705NON INVIARE             MANCANZA SENZA EVENTO                                       39
040600110705NON INVIARE             MANCANZA CON EVENTO                                         40
040700110705NON INVIARE             AVARIA SENZA EVENTO                                         41
040800110705NON INVIARE             AVARIA CON EVENTO                                           42
040900110705NON INVIARE             ALLUVIONE                                                   43
041000110705NON INVIARE             BLOCCO STRADALE                                             44
041100110705NON INVIARE             FESTIVITÀ REGIONALE                                         45
041200110705NON INVIARE             MARE MOLTO MOSSO                                            46
041300110705NON INVIARE             MANIFESTAZIONE PUBBLICA                                     47
041400110705NON INVIARE             NEVICATA ECCEZIONALE                                        48
041500110705NON INVIARE             SCIOPERO                                                    49
041600110705NON INVIARE             RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
041700110705NON INVIARE             MERCE NON ORDINATA O NON CONFORME                           51
041800110705NON INVIARE             MERCE SPEDITA IN RITARDO                                    52
041900110705NON INVIARE             MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
042000110705NON INVIARE             MERCE SPEDITA CON TROPPO ANTICIPO                           54
042100110705NON INVIARE             MERCE RESA SENZA AUTORIZZAZIONE                             55
042200110705NON INVIARE             DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
042300110705NON INVIARE             DESTINATARIO NON PAGA IL C/ASSEGNO                          57
042400110705NON INVIARE             CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58
042500110705NON INVIARE             DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
042600110705NON INVIARE             IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
042700110705NON INVIARE             DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
042800110705NON INVIARE             DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
042900110705NON INVIARE             DESTINATARIO RIMANDA LO SVINCOLO AL                         63
043000110705NON INVIARE             DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
043100110705NON INVIARE             DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65
043200110705NON INVIARE             DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
043300110705NON INVIARE             DESTINATARIO RISULTA TRASFERITO                             67
043400110705NON INVIARE             DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68
043500110705NON INVIARE             INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
043600110705NON INVIARE             IL DESTINATARIO E' CHIUSO FINO AL                           70
043700110705NON INVIARE             IL DESTINATARIO E' CHIUSO PER FERIE                         71
043800110705NON INVIARE             IL DESTINATARIO E' CHIUSO                                   72
043900110705NON INVIARE             IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
044000110705NON INVIARE             DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
044100110705NON INVIARE             ESERCIZIO NON ANCORA IN ATTIVITA                            75
044200110705NON INVIARE             IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
044300110705NON INVIARE             IL DESTINATARIO RIFIUTA LA MERCE                            77
044400110705NON INVIARE             IL DESTINATARIO NON INTENDE                                 78
044500110705NON INVIARE             FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79
044600110705NON INVIARE             IL DESTINATARIO RICHIEDE                                    80
044700110705NON INVIARE             SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
044800110705NON INVIARE             DOCUMENTI INCOMPLETI O MANCANTI                             82
04490011070550                      IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
045000110705NON INVIARE             RIFIUTA LA CONSEGNA TASSATIVA                               84
045100110705NON INVIARE             -                                                           85
045200110705NON INVIARE             -                                                           86
045300110705NON INVIARE             -                                                           87
04540011070550                      CONSEGNA OK                                                 88
04550011070550                      RESO                                                        89
045600110705                                                                                    90
045700110103** DESCLI - DESCRIZIONI CAUSALI CLIENTE
045800110705L.AVVISO                                                                             1
045900110705AVVISATO-RIMANDA SVINCOLO 1GG                                                        2
046000110705LASCIATO AVVISO 1GG                                                                  3
046100110705LASCIATO AVVISO 2GG                                                                  4
046200110705LASCIATO AVVISO 3GG                                                                  5
046300110705LASCIATO AVVISO 5GG                                                                  6
046400110705LASCIATO AVVISO 7GG                                                                  7
046500110705LASCIATO AVVISO 9 GIORNI                                                             8
046600110705L.A. RIMANDA LO SVINCOLO                                                             9
046700110705L.A. CHIUSO FINO AL                                                                 10
046800110705CAMBIO PORTO                                                                        11
046900110705COLLO/I DANNEGGIATO/I                                                               12
047000110705DIROTTAMENTO                                                                        13
047100110705FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA                                    14
047200110705GIACENZA GENERICA TEMPORANEA                                                        15
047300110705GIACENZA GENERICA                                                                   16
047400110705ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                                         17
047500110705ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                                         18
047600110705ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                                            19
047700110705ATTENDERE 9 GIORNI                                                                  20
047800110705CHIUSURA SPEDIZIONE CON IDD                                                         21
047900110705COLLO/I MANCANTE/I                                                                  22
048000110705MESSA IN CONSEGNA                                                                   23
048100110705NON FATTA                                                                           24
048200110705COLLO SCONDIZIONATO                                                                 25
048300110705TOLTA DALLA CONSEGNA                                                                26
048400110705CONSEGNA PARZIALE                                                                   27
048500110705FESTIVITA PATRONALE                                                                 28
048600110705RISERVA                                                                             29
048700110705RIPRISTINO                                                                          30
048800110705RIFIUTATO                                                                           31
048900110705RIFIUTATO                                                                           32
049000110705RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                                            33
049100110705RIPRISTINO                                                                          34
049200110705RISERVA                                                                             35
049300110705CHIUSO PER TURNO                                                                    36
049400110705AMMANCO SENZA EVENTO                                                                37
049500110705AMMANCO CON EVENTO                                                                  38
049600110705MANCANZA SENZA EVENTO                                                               39
049700110705MANCANZA CON EVENTO                                                                 40
049800110705AVARIA SENZA EVENTO                                                                 41
049900110705AVARIA CON EVENTO                                                                   42
050000110705ALLUVIONE                                                                           43
050100110705BLOCCO STRADALE                                                                     44
050200110705FESTIVITÀ REGIONALE                                                                 45
050300110705MARE MOLTO MOSSO                                                                    46
050400110705MANIFESTAZIONE PUBBLICA                                                             47
050500110705NEVICATA ECCEZIONALE                                                                48
050600110705SCIOPERO                                                                            49
050700110705RIFIUTA SENZA SPECIFICARE IL MOTIVO                                                 50
050800110705MERCE NON ORDINATA O NON CONFORME                                                   51
050900110705MERCE SPEDITA IN RITARDO                                                            52
051000110705MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                                            53
051100110705MERCE SPEDITA CON TROPPO ANTICIPO                                                   54
051200110705MERCE RESA SENZA AUTORIZZAZIONE                                                     55
051300110705DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE                                       56
051400110705DESTINATARIO NON PAGA IL C/ASSEGNO                                                  57
051500110705CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO                                  58
051600110705DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO                                     59
051700110705IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO                                      60
051800110705DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO                                     61
051900110705DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                                         62
052000110705DESTINATARIO RIMANDA LO SVINCOLO AL                                                 63
052100110705DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                                           64
052200110705DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO                                    65
052300110705DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO                              66
052400110705DESTINATARIO RISULTA TRASFERITO                                                     67
052500110705DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO                               68
052600110705INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO                                   69
052700110705IL DESTINATARIO E' CHIUSO FINO AL                                                   70
052800110705IL DESTINATARIO E' CHIUSO PER FERIE                                                 71
052900110705IL DESTINATARIO E' CHIUSO                                                           72
053000110705IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO                                  73
053100110705DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE                                  74
053200110705ESERCIZIO NON ANCORA IN ATTIVITA                                                    75
053300110705IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO                                 76
053400110705IL DESTINATARIO RIFIUTA LA MERCE                                                    77
053500110705IL DESTINATARIO NON INTENDE                                                         78
053600110705FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO                                  79
053700110705IL DESTINATARIO RICHIEDE                                                            80
053800110705SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE                                    81
053900110705DOCUMENTI INCOMPLETI O MANCANTI                                                     82
054000110705IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA                                83
054100110705RIFIUTA LA CONSEGNA TASSATIVA                                                       84
054200110705-                                                                                   85
054300110705-                                                                                   86
054400110705-                                                                                   87
054500110705CONSEGNA OK                                                                         88
054600110705RESO                                                                                89
054700110705                                                                                    90
054800041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
054900110103A                                                                                            1
055000110103M                                                                                            2
055100110103V                                                                                            3
055200110103E                                                                                            4
055300080901** WDCTCLI - CAUSALI CLIENTE
055400110103D                                                                                            1
055500110103D                                                                                            2
055600110103D                                                                                            3
055700110103D                                                                                            4
055800110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
055900110705INCARICO1                                                                                    1
056000110705INCARICO2                                                                                    2
056100110705CONSOK1                                                                                      3
056200110705CONSOK2                                                                                      4
056300110705GIACNODIS1                                                                                   5
056400110705GIACNODIS2                                                                                   6
056500110705GIACSIDIS1                                                                                   7
056600110705GIACSIDIS2                                                                                   8
056700110705                                                                                             9
056800110705                                                                                             10
056900110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
05700011070501                                                                                           1
05710011070501                                                                                           2
05720011070550                                                                                           3
05730011070550                                                                                           4
05740011070562                                                                                           5
05750011070562                                                                                           6
057600110705NON INVIARE                                                                                  7
057700110705NON INVIARE                                                                                  8
057800110705NON INVIARE                                                                                  9
057900110705NON INVIARE                                                                                  10
