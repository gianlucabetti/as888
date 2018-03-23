000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500041015     FTNTBE01L  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700110103     FTISTB03L  UF   E           K DISK    commit
000800090323     FTIVAWWWT  UF A E             DISK    commit
000900140131     Ffirab01l  IF   E           K DISK
001000031103
001100031103     D*-----------
001200031103     D* SCHIERE A PROGRAMMA
001300031103     D*-----------
001400131104     D WCAU            S             90    DIM(90) CTDATA PERRCD(1)
001500131104     D WDCT            S             30    DIM(4)  CTDATA PERRCD(1)
001600131104     D WTIS9           S             30    DIM(10) CTDATA PERRCD(1)
001700131104
001800131104
001900131104     D*-----------
002000131104     D* DS RIDEFINIZIONE SCHIERE A PROGRAMMA
002100131104     D*-----------
002200131104     D DS_WCAU         DS
002300131104     D  dsCAU_BRT                    15
002400131104     D  dsCAU_CLI                    15
002500131104     D  dsCAU_DES                    60
002600131104     D*
002700131104     D DS_WDCT         DS
002800131104     D  dsDCT_BRT                    15
002900131104     D  dsDCT_CLI                    15
003000131104     D*
003100131104     D DS_WTIS9        DS
003200131104     D  dsTIS9_BRT                   15
003300131104     D  dsTIS9_CLI                   15
003400131104
003500131104
003600041015     D*-----------
003700041015     D* SCHIERE
003800041015     D*-----------
003900131104     D skCAU_BRT       S             10    DIM(%elem(WCAU))
004000131104     D skCAU_CLI       S             10    DIM(%elem(WCAU))
004100131104     D skCAU_DES       S             60    DIM(%elem(WCAU))
004200131104     D jCAU            S              3  0 INZ(*zeros)
004300131104     D skDCT_BRT       S              1    DIM(%elem(WDCT))
004400131104     D skDCT_CLI       S             10    DIM(%elem(WDCT))
004500131104     D jDCT            S              3  0 INZ(*zeros)
004600131104     D skTIS9_BRT      S             10    DIM(%elem(WTIS9))
004700131104     D skTIS9_CLI      S             10    DIM(%elem(WTIS9))
004800131104     D jTIS9           S              3  0 INZ(*zeros)
004900131104     D skTADCod        S              2    DIM(100)
005000131104     D skTADRag        S              1    DIM(100)
005100041015     D jTAD            S              3  0 INZ(*zeros)
005200140131
005300140131
005400140131     D ds_out_cli      ds
005500140131     D  dout_NumDDT                  10
005600140131     D  dout_RifSAP                  10
005700140131     D  dout_DataCons                 8
005800140131     D  dout_Causale                  2
005900140131     D  dout_SubCaus                  3
006000140131     D  dout_IdBRT                   15
006100140131
006200030924
006300041015     D*-----------
006400041015     D* VARIABILI D WRK
006500041015     D*-----------
006600031103     D prmppi          S             50
006700030924     D prmesito        S              1
006800030924     D wrkesito        S                   like(prmesito)
006900031103     D j               S              2  0
007000130807     D wTADCod         S              2
007100140131     D wWrkSts         S             25
007200131004
007300131004
007400131004     D*-----------
007500131004     D* DEFINIZIONE DS ESTERNE
007600131004     D*-----------
007700131004     D DTAD          e ds                  inz
007800131004
007900131004
008000031103     D*-----------
008100031103     D* VARIABILI RIFERITE AL DATA-BASE
008200031103     D*-----------
008300080618     D wKstbKSU        S                   like(stbKSU)
008400080618     D wKstbFTR        S                   like(stbFTR)
008500030924
008600110705
008700031103     D*-----------
008800031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
008900031103     D*-----------
009000031103     D DSPPI           DS
009100080618     D  w§PPIKSU                      7  0
009200131004
009300030924
009400030325
009500030924     C* MAIN/
009600041015     C*
009700041015     C* Carica i dati tabellati
009800041015     C                   EXSR      cartab
009900041015     C*
010000041015     C* Esegue traduzione
010100131004     C                   setoff                                       31
010200131004     C  N31              EXSR      chkRECFILMBR
010300131004     C  N31              EXSR      traduci
010400110705     C*
010500921023     C                   SETON                                        LR
010600131004
010700131004
010800131004
010900131004     C     chkRECFILMBR  BEGSR
011000131004     C*
011100131004     C     *start        setll     TIVAWWWT
011200131004     C                   if        %found(TIVAWWWT)
011300131004     C                   seton                                        31
011400131004     C                   endif
011500131004     C*
011600131004     C                   ENDSR
011700131004
011800131004
011900131004
012000131004     C     wriOUTPUT     BEGSR
012100131004     C*
012200131004     C                   write     TIVAW000
012300131004     C*
012400131004     C                   ENDSR
012500131004
012600130807
012700041015
012800041015     C     cartab        BEGSR
012900041015     C*
013000041015     C* Caricamento tipi anomalia C/A
013100041015     C     'TAD'         SETLL     tntbe01l
013200041015     C                   IF        %equal(tntbe01l)
013300041015     C     'TAD'         READE     tntbe01l
013400041015     C                   DOW       not %eof(tntbe01l)
013500041015     C                   EVAL      DTAD = tbeUNI
013600041015     C                   ADD       1             jTAD
013700041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
013800041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
013900041015     C     'TAD'         READE     tntbe01l
014000041015     C                   ENDDO
014100041015     C                   ENDIF
014200131104     C*
014300131104     C* Carico la mappatura dei codici status personalizzati cliente
014400131104     C                   z-add     1             jCAU
014500131104     C                   dow       jCAU <= %elem(WCAU) and
014600131104     C                                     WCAU(jCAU) <> *blanks
014700131104     C                   eval      DS_WCAU = WCAU(jCAU)
014800131104     C                   eval      skCAU_BRT(jCAU) = dsCAU_BRT
014900131104     C                   eval      skCAU_CLI(jCAU) = dsCAU_CLI
015000131104     C                   eval      skCAU_DES(jCAU) = dsCAU_DES
015100131104     C                   add       1             jCAU
015200131104     C                   enddo
015300131104     C*
015400131104     C                   z-add     1             jDCT
015500131104     C                   dow       jDCT <= %elem(WDCT) and
015600131104     C                                     WDCT(jDCT) <> *blanks
015700131104     C                   eval      DS_WDCT = WDCT(jDCT)
015800131104     C                   eval      skDCT_BRT(jDCT) = dsDCT_BRT
015900131104     C                   eval      skDCT_CLI(jDCT) = dsDCT_CLI
016000131111     C                   add       1             jDCT
016100131104     C                   enddo
016200131104     C*
016300131104     C                   z-add     1             jTIS9
016400131104     C                   dow       jTIS9 <= %elem(WTIS9) and
016500131104     C                                      WTIS9(jTIS9) <> *blanks
016600131104     C                   eval      DS_WTIS9 = WTIS9(jTIS9)
016700131104     C                   eval      skTIS9_BRT(jTIS9) = dsTIS9_BRT
016800131104     C                   eval      skTIS9_CLI(jTIS9) = dsTIS9_CLI
016900131104     C                   add       1             jTIS9
017000131104     C                   enddo
017100131104     C*
017200131104     C                   ENDSR
017300131004
017400030924
017500030924
017600991027     C     traduci       BEGSR
017700031103     C*
017800031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
017900031103     C* di traduzione (trattasi d "post-traduzione")
018000031104     C                   EVAL      wKstbKSU = w§PPIKSU
018100031103     C                   EVAL      wKstbFTR = *blanks
018200110103     C     KEYstb03P     SETLL     tistb03l
018300110103     C                   IF        %equal(tistb03l)
018400110103     C     KEYstb03P     READE     tistb03l
018500991027     C*
018600031103     C* Ciclo x tuttii record del cliente da trasmettere
018700110103     C                   DOW       not %eof(tistb03l)
018800080618     C*
018900130807     C                   SETON                                        50
019000031103     C*
019100031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
019200031103     C* deve restituire al cliente nel file tradotto
019300031103     C                   IF        stbFTR = *blanks                             * ridondante
019400110103     C*
019500110103     C* Gestisco solamente gli stati standard
019600130808     C                   IF        (stbTIS= '1' OR
019700130906     C                             (stbTIS= '9' AND
019800130906     C                              (stbPRS= '1' OR stbPRS = '4'))) AND
019900130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
020000130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
020100131004     C*
020200131004     C* Per "sicurezza" non invio stati senza data
020300131004     C                   IF        stbDAS > *zeros
020400110103     C*
020500110103     C                   SETOFF                                       50
020600031103     C*
020700031103     C* Innanzitutto chaino la bolla direttamente su TITAS
020800031103     C     KEYtas30P     CHAIN     titas30c
020900031103     C                   IF        %found(titas30c)
021000131010     C*
021100131010     C* Innanzitutto chaino la bolla direttamente su TITAS
021200131216     C                   MOVEL     *zeros        wPO               3 0
021300131213     C*
021400131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
021500131213     C                   if        tasDTI = *zeros
021600131213     C                   eval      wPO = tasLNP
021700131213     C                   else
021800131213     C                   eval      wPO = tasLNA
021900131213     C                   endif
022000131213     C*
022100131213     C     wPO           CHAIN     azorg01l
022200131010     C                   IF        %found(azorg01l)
022300080618     C*
022400110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
022500110103     C                   IF        stbTIS = '1'
022600031103     C                   Z-ADD     1             j
022700131104     C     stbCOS        LOOKUP    skCAU_BRT(j)                           55
022800131113     C                   IF        %found AND skCAU_CLI(j) <> 'NONINVIARE'
022900131104     C                   IF        skCAU_CLI(j) <> '*'
023000131104     C                   EVAL      wWrkSts = skCAU_CLI(j)
023100080618     C                   ENDIF
023200031103     C                   ELSE
023300031103     C                   SETON                                        50
023400031103     C                   ENDIF
023500110103     C                   ENDIF
023600110103     C*
023700110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
023800110103     C                   IF        stbTIS <> '1'
023900110103     C                   Z-ADD     1             j
024000131104     C     stbCOS        LOOKUP    skTIS9_BRT(j)                          55
024100131113     C                   IF        %found AND skTIS9_CLI(j) <> 'NONINVIARE'
024200131104     C                   EVAL      wWrkSts = skTIS9_CLI(j)
024300110103     C                   ELSE
024400110103     C                   SETON                                        50
024500110103     C                   ENDIF
024600110103     C                   ENDIF
024700041015     C*
024800080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
024900130807     C                   IF        stbPRS =  '5'
025000080618     C*
025100130807     C                   SETOFF                                       50
025200080618     C*
025300080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
025400130807     C                   Z-ADD     1             jTAD
025500130807     C                   MOVEL(P)  stbCOS        wTADCod
025600130807     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
025700130807     C                   IF        %found
025800130807     C                   Z-ADD     1             j
025900131104     C     skTADRag(jTAD)LOOKUP    skDCT_BRT(j)                           70
026000130807     C                   IF        %found
026100131113     C                   IF        %trim(skDCT_CLI(j)) <> 'NONINVIARE'
026200140131     C                   EVAL      wWrkSts = skDCT_CLI(j)
026300131004     C                   ELSE
026400131004     C                   SETON                                        50
026500131004     C                   ENDIF
026600130807     C                   ELSE
026700130807     C                   SETON                                        50
026800130807     C                   ENDIF
026900130807     C                   ELSE
027000130807     C                   SETON                                        50
027100130807     C                   ENDIF
027200130807     C                   ENDIF
027300080618     C*
027400080618     C* Scarico il buffer d output
027500140131     C  N50              EXSR      WRIFILE
027600110103     C*
027700131010     C                   ENDIF
027800131004     C                   ENDIF
027900110103     C                   ENDIF
028000110103     C                   ENDIF
028100110103     C                   ENDIF
028200031103     C*
028300031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
028400031103     C                   EVAL      stbFTR = 'T'
028500031103     C                   UPDATE    tistb000
028600991027     C*
028700110103     C     KEYstb03P     READE     tistb03l
028800031103     C                   SETOFF                                       50
028900030325     C                   ENDDO
029000031103     C                   ENDIF
029100991027     C*
029200030325     C                   EVAL      wrkesito = '0'
029300991027     C*
029400910830     C                   ENDSR
029500140131
029600140131
029700140131
029800140131     C     WRIFILE       BEGSR
029900140131     C*
030000140131     C                   clear                   ds_out_cli
030100140131     C                   movel     '03'          dout_Causale
030200140131     C                   move      stbDAS        dout_DataCons
030300140131     C                   eval      dout_SubCaus = %trim(wWrkSts)
030400140131     C                   eval      dout_IdBRT   = %editc(stbLNP:'X') +
030500140131     C                                            %editc(stbNRS:'X') +
030600140131     C                                            %editc(stbNSP:'X') + '/' +
030700140131     C                                            %subst(%editc(stbAAS:'X'):3:2)
030800140131     C*
030900140131     C* Scorro il file raccordo accorpamento bolle x verticalizzare gli esiti
031000140131     C* anche delle bolle accorpate
031100140131     C*
031200140131     C     KEYrab01_C    SETLL     firab01l
031300140131     C                   IF        %equal(firab01l)
031400140131     C     KEYrab01_C    READE     firab01l
031500140131     C                   DOW       not %eof(firab01l)
031600140131     C                   move      rabRMNR       dout_NumDDT
031700140131     C                   EXSR      WRIOUT
031800140131     C     KEYrab01_C    READE     firab01l
031900140131     C                   ENDDO
032000140131     C*
032100140131     C                   ELSE
032200140131     C                   move      stbRMN        dout_NumDDT
032300140131     C                   EXSR      WRIOUT
032400140131     C                   ENDIF
032500140131     C*
032600140131     C                   ENDSR
032700140131
032800140131
032900140131
033000140131     C     WRIOUT        BEGSR
033100140131     C*
033200140131     C                   eval      vawDTA = ds_out_cli
033300140131     C*
033400140131     C                   WRITE     TIVAW000
033500140131     C*
033600140131     C                   ENDSR
033700031103
033800031103
033900991027
034000991027      /TITLE Operazioni iniziali.
034100991027     C     *inzsr        BEGSR
034200991027     C*
034300991027     C     *ENTRY        PLIST
034400031103     C                   parm                    prmppi
034500991027     C     wrkesito      parm      wrkesito      prmesito
034600031103     C*
034700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
034800031103     C                   EVAL      DSPPI = prmppi
034900030709     C*
035000030709     C* Definizione chiavi
035100030924     C*
035200110103     C* Chiave su TISTB03L - Parziale
035300110103     C     KEYstb03P     KLIST
035400031104     C                   KFLD                    wKstbKSU
035500031103     C                   KFLD                    wKstbFTR
035600031103     C*
035700031103     C* Chiave su TITAS30C - Parziale
035800031103     C     KEYtas30P     KLIST
035900031103     C                   KFLD                    stbAAS
036000031103     C                   KFLD                    stbLNP
036100031103     C                   KFLD                    stbNRS
036200031103     C                   KFLD                    stbNSP
036300140131     C*
036400140131     C* Chiave su FIRAB01L - Completa
036500140131     C     KEYrab01_C    KLIST
036600140131     C                   KFLD                    stbAAS
036700140131     C                   KFLD                    stbLNP
036800140131     C                   KFLD                    stbNRS
036900140131     C                   KFLD                    stbNSP
037000991027     C*
037100991027     C                   ENDSR
037200131104** WCAU - CAUSALI BRT / CLIENTE: 15 byte BRT / 15 byte CLI / 60 byte DESCRIZIONE ITA
037300140131A              670            L.AVVISO                                                     1
037400140131ARS            670            AVVISATO-RIMANDA SVINCOLO 1GG                                2 ex 32
037500140131AVV            670            LASCIATO AVVISO 1GG                                          3
037600140131AV2            670            LASCIATO AVVISO 2GG                                          4
037700140131AV3            670            LASCIATO AVVISO 3GG                                          5
037800140131AV5            670            LASCIATO AVVISO 5GG                                          6
037900140131AV7            670            LASCIATO AVVISO 7GG                                          7
038000140131AV9            670            LASCIATO AVVISO 9 GIORNI                                     8
038100140131A16            670            L.A. RIMANDA LO SVINCOLO                                     9 ex 32
038200140131A23            670            L.A. CHIUSO FINO AL                                         10
038300131113CPO            NONINVIARE     CAMBIO PORTO                                                11
038400140131DAN            652            COLLO/I DANNEGGIATO/I                                       12
038500140131DIR            NONINVIARE     DIROTTAMENTO                                                13
038600131113F              NONINVIARE     FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
038700140131G              670            GIACENZA GENERICA TEMPORANEA                                15 ex 32
038800140131GEN            670            GIACENZA GENERICA                                           16 ex 32
038900131113G02            NONINVIARE     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
039000131113G03            NONINVIARE     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
039100131113G05            NONINVIARE     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
039200131113G09            NONINVIARE     ATTENDERE 9 GIORNI                                          20
039300140131IDD            NONINVIARE     CHIUSURA SPEDIZIONE CON IDD                                 21
039400140131MAN            653            COLLO/I MANCANTE/I                                          22
039500140131MIC            NONINVIARE     MESSA IN CONSEGNA                                           23
039600140131N              NONINVIARE     NON FATTA                                                   24
039700140131NAV            652            COLLO SCONDIZIONATO                                         25
039800140131NIC            NONINVIARE     TOLTA DALLA CONSEGNA                                        26
039900140131P              NONINVIARE     CONSEGNA PARZIALE                                           27
040000140131PAT            670            FESTIVITA PATRONALE                                         28
040100140131R              671            RISERVA                                                     29
040200131113RDC            NONINVIARE     RIPRISTINO                                                  30
040300140131RFD            605            RIFIUTATO                                                   31
040400140131RFM            605            RIFIUTATO                                                   32
040500140131RIC            NONINVIARE     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
040600131113RIP            NONINVIARE     RIPRISTINO                                                  34
040700140131RS             NONINVIARE     RISERVA                                                     35
040800140131T              670            CHIUSO PER TURNO                                            36
040900140131WA             653            AMMANCO SENZA EVENTO                                        37
041000140131WAE            653            AMMANCO CON EVENTO                                          38
041100140131WM             653            MANCANZA SENZA EVENTO                                       39
041200140131WME            653            MANCANZA CON EVENTO                                         40
041300140131WV             652            AVARIA SENZA EVENTO                                         41
041400140131WVE            652            AVARIA CON EVENTO                                           42
041500140131ZAL            NONINVIARE     ALLUVIONE                                                   43
041600140131ZBS            NONINVIARE     BLOCCO STRADALE                                             44
041700140131ZFR            NONINVIARE     FESTIVITÀ REGIONALE                                         45
041800140131ZMM            NONINVIARE     MARE MOLTO MOSSO                                            46
041900140131ZMP            NONINVIARE     MANIFESTAZIONE PUBBLICA                                     47
042000140131ZNV            NONINVIARE     NEVICATA ECCEZIONALE                                        48
042100140131ZSC            NONINVIARE     SCIOPERO                                                    49
042200140131001            602            RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
042300140131002            601            MERCE NON ORDINATA O NON CONFORME                           51
042400140131003            655            MERCE SPEDITA IN RITARDO                                    52
042500140131004            605            MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
042600140131005            603            MERCE SPEDITA CON TROPPO ANTICIPO                           54
042700140131006            NONINVIARE     MERCE RESA SENZA AUTORIZZAZIONE                             55
042800140131007            605            DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
042900140131008            609            DESTINATARIO NON PAGA IL C/ASSEGNO                          57
043000140131009            610            CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58 ex 32
043100140131010            609            DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
043200140131012            NONINVIARE     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
043300140131013            609            DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
043400140131014            NONINVIARE     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
043500140131016            603            DESTINATARIO RIMANDA LO SVINCOLO AL                         63 ex 32
043600140131017            670            DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
043700140131018            NONINVIARE     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65 ex 32
043800140131019            669            DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
043900140131020            669            DESTINATARIO RISULTA TRASFERITO                             67
044000140131021            669            DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68 ex 16
044100140131022            611            INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
044200140131023            670            IL DESTINATARIO E' CHIUSO FINO AL                           70
044300140131024            670            IL DESTINATARIO E' CHIUSO PER FERIE                         71
044400140131025            670            IL DESTINATARIO E' CHIUSO                                   72
044500140131026            668            IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
044600140131027            601            DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
044700140131028            664            ESERCIZIO NON ANCORA IN ATTIVITA                            75
044800140131029            609            IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
044900140131030            607            IL DESTINATARIO RIFIUTA LA MERCE                            77
045000140131031            663            IL DESTINATARIO NON INTENDE                                 78
045100140131032            NONINVIARE     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79 ex 32
045200140131033            663            IL DESTINATARIO RICHIEDE                                    80 ex 32
045300140131034            NONINVIARE     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
045400140131035            NONINVIARE     DOCUMENTI INCOMPLETI O MANCANTI                             82
045500140131036            663            IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
045600140131037            603            RIFIUTA LA CONSEGNA TASSATIVA                               84
045700140131100            NONINVIARE     -                                                           85
045800140131101            NONINVIARE     -                                                           86
045900131113999            NONINVIARE     -                                                           87
046000140131CONSOK                        CONSEGNA OK                                                 88
046100140131RESO                          RESO                                                        89
046200131104                                                                                          90
046300131104** WDCT - TIPI ANOMALIE BRT/CLIENTE: 15 byte BRT / 15 byte CLI
046400140131A              653            AMMANCO                                                        1
046500140131M              653            MANCANZA                                                       2
046600140131V              652            AVARIA                                                         3
046700140131E              NONINVIARE     EVENTO AVVERSO                                                 4
046800131104** WTIS9 - CODICI EVENTO "PERSONALIZZATI" - BRT/CLIENTE: 15 byte BRT / 15 byte CLI
046900140131INCARICO1      NONINVIARE                                                                    1
047000140131INCARICO2      NONINVIARE                                                                    2
047100140131CONSOK1
047200140131CONSOK2
047300131113GIACNODIS1     NONINVIARE                                                                    5
047400131113GIACNODIS2     NONINVIARE                                                                    6
047500131113GIACSIDIS1     NONINVIARE                                                                    7
047600131113GIACSIDIS2     NONINVIARE                                                                    8
047700131113               NONINVIARE                                                                    9
047800131113               NONINVIARE                                                                    10
