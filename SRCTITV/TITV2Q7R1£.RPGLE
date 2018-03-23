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
001600130808     D WDCTCLI         S             10    DIM(6)  CTDATA PERRCD(1)
001700130807     D WCOD9BAR        S             10    DIM(10) CTDATA PERRCD(1)
001800130807     D WCOD9CLI        S             11    DIM(10) CTDATA PERRCD(1)
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
003400130807     D wTADCod         S              2
003500130807     D wTotRec         s              5S 0 inz
003600041015
003700041015     D*-----------
003800041015     D* DEFINIZIONE DS ESTERNE
003900041015     D*-----------
004000041015     D DTAD          E DS
004100031103
004200031103     D*-----------
004300031103     D* VARIABILI RIFERITE AL DATA-BASE
004400031103     D*-----------
004500080618     D wKstbKSU        S                   like(stbKSU)
004600080618     D wKstbFTR        S                   like(stbFTR)
004700030924
004800110103
004900110103     D*----------
005000130807     D* DS CLIENTE RECORD TYPE 'S0'
005100110103     D*----------
005200130807     D DS_Cli_S0       DS
005300130807     D  DS_S0_RcdTyp                  2    inz('S0')
005400130807     D  DS_S0_Sender                 10    inz('BRT2013-10')
005500130807     D  DS_S0_Rceiver                10    inz('DTC')
005600130807     D  DS_S0_Fill001                 4    inz
005700130807     D  DS_S0_MsgDate                 6    inz(*zeros)
005800130807     D  DS_S0_Fill002                48    inz
005900130807     D*----------
006000130807     D* DS CLIENTE RECORD TYPE 'S5'
006100130807     D*----------
006200130807     D DS_Cli_S5       DS
006300130807     D  DS_S5_RcdTyp                  2    inz('S5')
006400130807     D  DS_S5_RMA                    16    inz
006500130807     D  DS_S5_RMN                     9    inz(*zeros)
006600130807     D  DS_S5_Fill001                 2    inz
006700130807     D  DS_S5_StsTime                 4    inz(*zeros)
006800130807     D  DS_S5_StsDate                 6    inz(*zeros)
006900130808     D  DS_S5_StsCode                 6    inz
007000130807     D  DS_S5_Fill002                 3    inz
007100130807     D  DS_S5_DTC_nbr                 3    inz(*zeros)
007200130807     D  DS_S5_NCL                     3    inz(*zeros)
007300130807     D  DS_S5_Fill003                26    inz
007400130807     D*----------
007500130807     D* DS CLIENTE RECORD TYPE 'S9'
007600130807     D*----------
007700130807     D DS_Cli_S9       DS
007800130807     D  DS_S9_RcdTyp                  2    inz('S9')
007900130807     D  DS_S9_RcdTot                  5    inz(*zeros)
008000130807     D  DS_S9_Fill001                73    inz
008100110705
008200031103
008300031103     D*-----------
008400031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
008500031103     D*-----------
008600031103     D DSPPI           DS
008700080618     D  w§PPIKSU                      7  0
008800031103
008900030924
009000030325
009100030924     C* MAIN/
009200130807     C*
009300130807     C* Inizializzo i contatori generali
009400130807     C                   clear                   wTotRec
009500041015     C*
009600041015     C* Carica i dati tabellati
009700041015     C                   EXSR      cartab
009800041015     C*
009900041015     C* Esegue traduzione
010000130807     C                   EXSR      chkRECFILMBR
010100130807     C                   EXSR      wriRec0
010200130807     C                   EXSR      traduci
010300130807     C                   EXSR      wriRec9
010400110705     C*
010500921023     C                   SETON                                        LR
010600041015
010700130807
010800041015
010900041015     C     cartab        BEGSR
011000041015     C*
011100041015     C* Caricamento tipi anomalia C/A
011200041015     C     'TAD'         SETLL     tntbe01l
011300041015     C                   IF        %equal(tntbe01l)
011400041015     C     'TAD'         READE     tntbe01l
011500041015     C                   DOW       not %eof(tntbe01l)
011600041015     C                   EVAL      DTAD = tbeUNI
011700041015     C                   ADD       1             jTAD
011800041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
011900041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
012000041015     C     'TAD'         READE     tntbe01l
012100041015     C                   ENDDO
012200041015     C                   ENDIF
012300041015     C*
012400041015     C                   ENDSR
012500130807
012600130807
012700130807
012800130807     C     chkRECFILMBR  BEGSR
012900130807     C*
013000130807     C     *start        SETLL     TIVAWWWT
013100130807     C                   DOW       not %eof(TIVAWWWT)
013200130807     C                   READ      TIVAW000                               55
013300130807     C  N55              ADD       1             wTotRec
013400130807     C                   ENDDO
013500130807     C*
013600130807     C                   ENDSR
013700130807
013800130807
013900130807
014000130807     C     wriRec0       BEGSR
014100130807     C*
014200130807     C                   reset                   DS_Cli_S0
014300130807     C                   eval      DS_S0_MsgDate =
014400130807     C                             %subst(%editc(%dec(%date():*ISO):'X'):3)
014500130807     C*
014600130807     C                   eval      vawDTA = DS_Cli_S0
014700130807     C                   write     TIVAW000
014800130807     C                   add       1             wTotRec
014900130807     C*
015000130807     C                   ENDSR
015100130807
015200130807
015300130807
015400130807     C     wriRec9       BEGSR
015500130807     C*
015600130808     C                   reset                   DS_Cli_S9
015700130807     C                   add       1             wTotRec
015800130807     C                   eval      DS_S9_RcdTot = %char(wTotRec)
015900130807     C*
016000130807     C                   eval      vawDTA = DS_Cli_S9
016100130807     C                   write     TIVAW000
016200130807     C*
016300130807     C                   ENDSR
016400991027
016500030924
016600030924
016700991027     C     traduci       BEGSR
016800031103     C*
016900031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
017000031103     C* di traduzione (trattasi d "post-traduzione")
017100031104     C                   EVAL      wKstbKSU = w§PPIKSU
017200031103     C                   EVAL      wKstbFTR = *blanks
017300110103     C     KEYstb03P     SETLL     tistb03l
017400110103     C                   IF        %equal(tistb03l)
017500110103     C     KEYstb03P     READE     tistb03l
017600991027     C*
017700031103     C* Ciclo x tuttii record del cliente da trasmettere
017800110103     C                   DOW       not %eof(tistb03l)
017900031014     C*
018000030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
018100130807     C                   reset                   DS_Cli_S5
018200080618     C*
018300130807     C                   SETON                                        50
018400031103     C*
018500031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
018600031103     C* deve restituire al cliente nel file tradotto
018700031103     C                   IF        stbFTR = *blanks                             * ridondante
018800110103     C*
018900110103     C* Gestisco solamente gli stati standard
019000130808     C***                IF         stbTIS =  '1'                    OR
019100130808     C***                          (stbTIS <> '1'  AND stbPRS = '1') OR
019200130808     C***                          (stbTIS <> '1'  AND stbPRS = '3')
019300130808     C                   IF        (stbTIS= '1' OR
019400130906     C                             (stbTIS= '9' AND
019500130906     C                              (stbPRS= '1' OR stbPRS = '4'))) AND
019600130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
019700130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
019800110103     C*
019900110103     C                   SETOFF                                       50
020000031103     C*
020100031103     C* Innanzitutto chaino la bolla direttamente su TITAS
020200031103     C     KEYtas30P     CHAIN     titas30c
020300031103     C                   IF        %found(titas30c)
020400080618     C*
020500110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
020600110103     C                   IF        stbTIS = '1'
020700031103     C                   Z-ADD     1             j
020800031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
020900110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
021000080618     C                   IF        CAUCLI(j) <> '*'
021100130807     C                   EVAL      DS_S5_StsCode = CAUCLI(j)
021200080618     C                   ENDIF
021300031103     C                   ELSE
021400031103     C                   SETON                                        50
021500031103     C                   ENDIF
021600110103     C                   ENDIF
021700110103     C*
021800110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
021900110103     C                   IF        stbTIS <> '1'
022000110103     C                   Z-ADD     1             j
022100110103     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
022200110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
022300130807     C                   EVAL      DS_S5_StsCode = WCOD9CLI(j)
022400110103     C                   ELSE
022500110103     C                   SETON                                        50
022600110103     C                   ENDIF
022700110103     C                   ENDIF
022800041015     C*
022900080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
023000130807     C                   IF        stbPRS =  '5'
023100080618     C*
023200130807     C                   SETOFF                                       50
023300080618     C*
023400080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
023500130807     C                   Z-ADD     1             jTAD
023600130807     C                   MOVEL(P)  stbCOS        wTADCod
023700130807     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
023800130807     C                   IF        %found
023900130807     C                   Z-ADD     1             j
024000130807     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
024100130807     C                   IF        %found
024200130807     C                   EVAL      DS_S5_StsCode = WDCTCLI(j)
024300130807     C                   ELSE
024400130807     C                   SETON                                        50
024500130807     C                   ENDIF
024600130807     C                   ELSE
024700130807     C                   SETON                                        50
024800130807     C                   ENDIF
024900130807     C                   ENDIF
025000080618     C*
025100080618     C* Scarico il buffer d output
025200130807     C  N50              EXSR      wriRec5
025300110103     C*
025400110103     C                   ENDIF
025500110103     C                   ENDIF
025600110103     C                   ENDIF
025700031103     C*
025800031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
025900031103     C                   EVAL      stbFTR = 'T'
026000031103     C                   UPDATE    tistb000
026100991027     C*
026200110103     C     KEYstb03P     READE     tistb03l
026300031103     C                   SETOFF                                       50
026400030325     C                   ENDDO
026500031103     C                   ENDIF
026600991027     C*
026700030325     C                   EVAL      wrkesito = '0'
026800991027     C*
026900910830     C                   ENDSR
027000031103
027100031103
027200031103
027300130807     C     wriRec5       BEGSR
027400110705     C*
027500130807     C                   eval      DS_S5_RMA     = stbRMA
027600130808     C                   eval      DS_S5_RMN     = %char(stbRMN)
027700130906     C                   eval      DS_S5_StsTime = %editc(stbORS:'X')
027800130807     C                   eval      DS_S5_StsDate = %subst(%editc(stbDAS:'X'):3)
027900031103     C*
028000130807     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
028100130807     C                   eval      vawDTA = DS_Cli_S5
028200130807     C                   write     TIVAW000
028300130807     C                   add       1             wTotRec
028400031103     C*
028500031103     C                   ENDSR
028600031103
028700031103
028800991027
028900991027      /TITLE Operazioni iniziali.
029000991027     C     *inzsr        BEGSR
029100991027     C*
029200991027     C     *ENTRY        PLIST
029300031103     C                   parm                    prmppi
029400991027     C     wrkesito      parm      wrkesito      prmesito
029500031103     C*
029600031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
029700031103     C                   EVAL      DSPPI = prmppi
029800030709     C*
029900030709     C* Definizione chiavi
030000030924     C*
030100110103     C* Chiave su TISTB03L - Parziale
030200110103     C     KEYstb03P     KLIST
030300031104     C                   KFLD                    wKstbKSU
030400031103     C                   KFLD                    wKstbFTR
030500031103     C*
030600031103     C* Chiave su TITAS30C - Parziale
030700031103     C     KEYtas30P     KLIST
030800031103     C                   KFLD                    stbAAS
030900031103     C                   KFLD                    stbLNP
031000031103     C                   KFLD                    stbNRS
031100031103     C                   KFLD                    stbNSP
031200991027     C*
031300991027     C                   ENDSR
031400110103** CAUBAR - CAUSALI BARTOLINI
031500110705A                       L.AVVISO                                                     1
031600110705ARS                     AVVISATO-RIMANDA SVINCOLO 1GG                                2
031700110705AVV                     LASCIATO AVVISO 1GG                                          3
031800110705AV2                     LASCIATO AVVISO 2GG                                          4
031900110705AV3                     LASCIATO AVVISO 3GG                                          5
032000110705AV5                     LASCIATO AVVISO 5GG                                          6
032100110705AV7                     LASCIATO AVVISO 7GG                                          7
032200110705AV9                     LASCIATO AVVISO 9 GIORNI                                     8
032300110705A16                     L.A. RIMANDA LO SVINCOLO                                     9
032400110705A23                     L.A. CHIUSO FINO AL                                         10
032500110705CPO                     CAMBIO PORTO                                                11
032600110705DAN                     COLLO/I DANNEGGIATO/I                                       12
032700110705DIR                     DIROTTAMENTO                                                13
032800110705F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
032900110705G                       GIACENZA GENERICA TEMPORANEA                                15
033000110705GEN                     GIACENZA GENERICA                                           16
033100110705G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
033200110705G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
033300110705G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
033400110705G09                     ATTENDERE 9 GIORNI                                          20
033500110705IDD                     CHIUSURA SPEDIZIONE CON IDD                                 21
033600110705MAN                     COLLO/I MANCANTE/I                                          22
033700110705MIC                     MESSA IN CONSEGNA                                           23
033800110705N                       NON FATTA                                                   24
033900110705NAV                     COLLO SCONDIZIONATO                                         25
034000110705NIC                     TOLTA DALLA CONSEGNA                                        26
034100110705P                       CONSEGNA PARZIALE                                           27
034200110705PAT                     FESTIVITA PATRONALE                                         28
034300110705R                       RISERVA                                                     29
034400110705RDC                     RIPRISTINO                                                  30
034500110705RFD                     RIFIUTATO                                                   31
034600110705RFM                     RIFIUTATO                                                   32
034700110705RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
034800110705RIP                     RIPRISTINO                                                  34
034900110705RS                      RISERVA                                                     35
035000110705T                       CHIUSO PER TURNO                                            36
035100110705WA                      AMMANCO SENZA EVENTO                                        37
035200110705WAE                     AMMANCO CON EVENTO                                          38
035300110705WM                      MANCANZA SENZA EVENTO                                       39
035400110705WME                     MANCANZA CON EVENTO                                         40
035500110705WV                      AVARIA SENZA EVENTO                                         41
035600110705WVE                     AVARIA CON EVENTO                                           42
035700110705ZAL                     ALLUVIONE                                                   43
035800110705ZBS                     BLOCCO STRADALE                                             44
035900110705ZFR                     FESTIVITÀ REGIONALE                                         45
036000110705ZMM                     MARE MOLTO MOSSO                                            46
036100110705ZMP                     MANIFESTAZIONE PUBBLICA                                     47
036200110705ZNV                     NEVICATA ECCEZIONALE                                        48
036300110705ZSC                     SCIOPERO                                                    49
036400110705001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
036500110705002                     MERCE NON ORDINATA O NON CONFORME                           51
036600110705003                     MERCE SPEDITA IN RITARDO                                    52
036700110705004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
036800110705005                     MERCE SPEDITA CON TROPPO ANTICIPO                           54
036900110705006                     MERCE RESA SENZA AUTORIZZAZIONE                             55
037000110705007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
037100110705008                     DESTINATARIO NON PAGA IL C/ASSEGNO                          57
037200110705009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58
037300110705010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
037400110705012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
037500110705013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
037600110705014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
037700110705016                     DESTINATARIO RIMANDA LO SVINCOLO AL                         63
037800110705017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
037900110705018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65
038000110705019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
038100110705020                     DESTINATARIO RISULTA TRASFERITO                             67
038200110705021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68
038300110705022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
038400110705023                     IL DESTINATARIO E' CHIUSO FINO AL                           70
038500110705024                     IL DESTINATARIO E' CHIUSO PER FERIE                         71
038600110705025                     IL DESTINATARIO E' CHIUSO                                   72
038700110705026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
038800110705027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
038900110705028                     ESERCIZIO NON ANCORA IN ATTIVITA                            75
039000110705029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
039100110705030                     IL DESTINATARIO RIFIUTA LA MERCE                            77
039200110705031                     IL DESTINATARIO NON INTENDE                                 78
039300110705032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79
039400110705033                     IL DESTINATARIO RICHIEDE                                    80
039500110705034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
039600110705035                     DOCUMENTI INCOMPLETI O MANCANTI                             82
039700110705036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
039800110705037                     RIFIUTA LA CONSEGNA TASSATIVA                               84
039900110705100                     -                                                           85
040000110705101                     -                                                           86
040100110705999                     -                                                           87
040200110705CONSOK                  CONSEGNA OK                                                 88
040300110705RESO                    RESO                                                        89
040400110705                                                                                    90
040500110103** CAUCLI - CAUSALI CLIENTE
04060013080820 5                    L.AVVISO                                                     1
04070013080820 5                    AVVISATO-RIMANDA SVINCOLO 1GG                                2
04080013080820 5                    LASCIATO AVVISO 1GG                                          3
04090013080820 5                    LASCIATO AVVISO 2GG                                          4
04100013080820 5                    LASCIATO AVVISO 3GG                                          5
04110013080820 5                    LASCIATO AVVISO 5GG                                          6
04120013080820 5                    LASCIATO AVVISO 7GG                                          7
04130013080820 5                    LASCIATO AVVISO 9 GIORNI                                     8
04140013080820 5                    L.A. RIMANDA LO SVINCOLO                                     9
04150013080820 5                    L.A. CHIUSO FINO AL                                         10
041600130807NON INVIARE             CAMBIO PORTO                                                11
04170013080856 14                   COLLO/I DANNEGGIATO/I                                       12
04180013080820 53                   DIROTTAMENTO                                                13
04190013080820 5                    FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
04200013080820 12                   GIACENZA GENERICA TEMPORANEA                                15
04210013080820 12                   GIACENZA GENERICA                                           16
04220013080820 5                    ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
04230013080820 5                    ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
04240013080820 5                    ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
04250013080820 5                    ATTENDERE 9 GIORNI                                          20
04260013080856 14                   CHIUSURA SPEDIZIONE CON IDD                                 21
04270013080856 45                   COLLO/I MANCANTE/I                                          22
042800130808NON INVIARE             MESSA IN CONSEGNA                                           23
042900130807NON INVIARE             NON FATTA                                                   24
043000130808NON INVIARE             COLLO SCONDIZIONATO                                         25
043100130807NON INVIARE             TOLTA DALLA CONSEGNA                                        26
04320014030420 41                   CONSEGNA PARZIALE                                           27
04330013080720 58                   FESTIVITA PATRONALE                                         28
043400130807NON INVIARE             RISERVA                                                     29
043500130807NON INVIARE             RIPRISTINO                                                  30
04360013080856 3                    RIFIUTATO                                                   31
04370013080856 3                    RIFIUTATO                                                   32
04380013080820 5                    RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
043900130807NON INVIARE             RIPRISTINO                                                  34
044000130807NON INVIARE             RISERVA                                                     35
04410013080820 7                    CHIUSO PER TURNO                                            36
044200130808NON INVIARE             AMMANCO SENZA EVENTO                                        37
044300130808NON INVIARE             AMMANCO CON EVENTO                                          38
044400130808NON INVIARE             MANCANZA SENZA EVENTO                                       39
044500110705NON INVIARE             MANCANZA CON EVENTO                                         40
044600110705NON INVIARE             AVARIA SENZA EVENTO                                         41
044700110705NON INVIARE             AVARIA CON EVENTO                                           42
04480013080720 63                   ALLUVIONE                                                   43
04490013080720 63                   BLOCCO STRADALE                                             44
04500013080720 58                   FESTIVITÀ REGIONALE                                         45
04510013080720 63                   MARE MOLTO MOSSO                                            46
04520013080720 17                   MANIFESTAZIONE PUBBLICA                                     47
04530013080720 63                   NEVICATA ECCEZIONALE                                        48
04540013080720 20                   SCIOPERO                                                    49
04550013080856 3                    RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
04560013080856 3                    MERCE NON ORDINATA O NON CONFORME                           51
04570013080856 3                    MERCE SPEDITA IN RITARDO                                    52
04580013080856 3                    MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
04590013080856 3                    MERCE SPEDITA CON TROPPO ANTICIPO                           54
04600013080856 3                    MERCE RESA SENZA AUTORIZZAZIONE                             55
04610013080856 3                    DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
04620013080856 3                    DESTINATARIO NON PAGA IL C/ASSEGNO                          57
046300110705NON INVIARE             CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58
04640013080856 3                    DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
04650013080856 3                    IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
04660013080856 3                    DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
04670013080856 3                    DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
04680013080820 5                    DESTINATARIO RIMANDA LO SVINCOLO AL                         63
04690013080820 7                    DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
04700013080856 3                    DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65
04710013080820 7                    DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
04720013080820 7                    DESTINATARIO RISULTA TRASFERITO                             67
04730013080820 30                   DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68
04740013080820 30                   INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
04750013080820 7                    IL DESTINATARIO E' CHIUSO FINO AL                           70
04760013080820 7                    IL DESTINATARIO E' CHIUSO PER FERIE                         71
04770013080820 7                    IL DESTINATARIO E' CHIUSO                                   72
04780013080820 12                   IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
04790013080856 3                    DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
04800013080820 7                    ESERCIZIO NON ANCORA IN ATTIVITA                            75
04810013080856 3                    IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
04820013080856 3                    IL DESTINATARIO RIFIUTA LA MERCE                            77
04830013080856 3                    IL DESTINATARIO NON INTENDE                                 78
04840013080856 3                    FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79
04850013080820 12                   IL DESTINATARIO RICHIEDE                                    80
04860013080820 63                   SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
04870013080856 37                   DOCUMENTI INCOMPLETI O MANCANTI                             82
048800130807NON INVIARE             IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
04890013080856 3                    RIFIUTA LA CONSEGNA TASSATIVA                               84
049000130807NON INVIARE             -                                                           85
049100130807NON INVIARE             -                                                           86
049200130807NON INVIARE             -                                                           87
04930013080721                      CONSEGNA OK                                                 88
04940013080876 0                    RESO                                                        89
049500110705                                                                                    90
049600110103** DESCLI - DESCRIZIONI CAUSALI CLIENTE
049700110705L.AVVISO                                                                             1
049800110705AVVISATO-RIMANDA SVINCOLO 1GG                                                        2
049900110705LASCIATO AVVISO 1GG                                                                  3
050000110705LASCIATO AVVISO 2GG                                                                  4
050100110705LASCIATO AVVISO 3GG                                                                  5
050200110705LASCIATO AVVISO 5GG                                                                  6
050300110705LASCIATO AVVISO 7GG                                                                  7
050400110705LASCIATO AVVISO 9 GIORNI                                                             8
050500110705L.A. RIMANDA LO SVINCOLO                                                             9
050600110705L.A. CHIUSO FINO AL                                                                 10
050700110705CAMBIO PORTO                                                                        11
050800110705COLLO/I DANNEGGIATO/I                                                               12
050900110705DIROTTAMENTO                                                                        13
051000110705FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA                                    14
051100110705GIACENZA GENERICA TEMPORANEA                                                        15
051200110705GIACENZA GENERICA                                                                   16
051300110705ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                                         17
051400110705ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                                         18
051500110705ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                                            19
051600110705ATTENDERE 9 GIORNI                                                                  20
051700110705CHIUSURA SPEDIZIONE CON IDD                                                         21
051800110705COLLO/I MANCANTE/I                                                                  22
051900110705MESSA IN CONSEGNA                                                                   23
052000110705NON FATTA                                                                           24
052100110705COLLO SCONDIZIONATO                                                                 25
052200110705TOLTA DALLA CONSEGNA                                                                26
052300110705CONSEGNA PARZIALE                                                                   27
052400110705FESTIVITA PATRONALE                                                                 28
052500110705RISERVA                                                                             29
052600110705RIPRISTINO                                                                          30
052700110705RIFIUTATO                                                                           31
052800110705RIFIUTATO                                                                           32
052900110705RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                                            33
053000110705RIPRISTINO                                                                          34
053100110705RISERVA                                                                             35
053200110705CHIUSO PER TURNO                                                                    36
053300110705AMMANCO SENZA EVENTO                                                                37
053400110705AMMANCO CON EVENTO                                                                  38
053500110705MANCANZA SENZA EVENTO                                                               39
053600110705MANCANZA CON EVENTO                                                                 40
053700110705AVARIA SENZA EVENTO                                                                 41
053800110705AVARIA CON EVENTO                                                                   42
053900110705ALLUVIONE                                                                           43
054000110705BLOCCO STRADALE                                                                     44
054100110705FESTIVITÀ REGIONALE                                                                 45
054200110705MARE MOLTO MOSSO                                                                    46
054300110705MANIFESTAZIONE PUBBLICA                                                             47
054400110705NEVICATA ECCEZIONALE                                                                48
054500110705SCIOPERO                                                                            49
054600110705RIFIUTA SENZA SPECIFICARE IL MOTIVO                                                 50
054700110705MERCE NON ORDINATA O NON CONFORME                                                   51
054800110705MERCE SPEDITA IN RITARDO                                                            52
054900110705MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                                            53
055000110705MERCE SPEDITA CON TROPPO ANTICIPO                                                   54
055100110705MERCE RESA SENZA AUTORIZZAZIONE                                                     55
055200110705DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE                                       56
055300110705DESTINATARIO NON PAGA IL C/ASSEGNO                                                  57
055400110705CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO                                  58
055500110705DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO                                     59
055600110705IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO                                      60
055700110705DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO                                     61
055800110705DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                                         62
055900110705DESTINATARIO RIMANDA LO SVINCOLO AL                                                 63
056000110705DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                                           64
056100110705DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO                                    65
056200110705DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO                              66
056300110705DESTINATARIO RISULTA TRASFERITO                                                     67
056400110705DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO                               68
056500110705INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO                                   69
056600110705IL DESTINATARIO E' CHIUSO FINO AL                                                   70
056700110705IL DESTINATARIO E' CHIUSO PER FERIE                                                 71
056800110705IL DESTINATARIO E' CHIUSO                                                           72
056900110705IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO                                  73
057000110705DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE                                  74
057100110705ESERCIZIO NON ANCORA IN ATTIVITA                                                    75
057200110705IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO                                 76
057300110705IL DESTINATARIO RIFIUTA LA MERCE                                                    77
057400110705IL DESTINATARIO NON INTENDE                                                         78
057500110705FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO                                  79
057600110705IL DESTINATARIO RICHIEDE                                                            80
057700110705SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE                                    81
057800110705DOCUMENTI INCOMPLETI O MANCANTI                                                     82
057900110705IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA                                83
058000110705RIFIUTA LA CONSEGNA TASSATIVA                                                       84
058100110705-                                                                                   85
058200110705-                                                                                   86
058300110705-                                                                                   87
058400110705CONSEGNA OK                                                                         88
058500110705RESO                                                                                89
058600110705                                                                                    90
058700041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
058800110103A                                                                                            1
058900110103M                                                                                            2
059000110103V                                                                                            3
059100110103E                                                                                            4
059200080901** WDCTCLI - CAUSALI CLIENTE
05930013080756 56                                                                                        1
05940013080756 45                                                                                        2
05950013080756 14                                                                                        3
05960013080756 14                                                                                        4
059700110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
059800110705INCARICO1                                                                                    1
059900110705INCARICO2                                                                                    2
060000110705CONSOK1                                                                                      3
060100130906CONSOK2                                                                                      4
060200110705GIACNODIS1                                                                                   5
060300110705GIACNODIS2                                                                                   6
060400110705GIACSIDIS1                                                                                   7
060500110705GIACSIDIS2                                                                                   8
060600110705                                                                                             9
060700110705                                                                                             10
060800110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
06090013080712                                                                                           1
06100013080712                                                                                           2
06110013080721                                                                                           3
061200130906NON INVIARE                                                                                  4
06130013080820 32                                                                                        5
06140013080820 32                                                                                        6
061500110705NON INVIARE                                                                                  7
061600110705NON INVIARE                                                                                  8
061700110705NON INVIARE                                                                                  9
061800110705NON INVIARE                                                                                  10
