000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400041015     FTNTBE01L  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600110103     FTISTB03L  UF   E           K DISK    commit
000700090323     FTIVAWWWT  UF A E             DISK    commit
000800031103
000900031103     D*-----------
001000031103     D* SCHIERE A PROGRAMMA
001100031103     D*-----------
001200131104     D WCAU            S             90    DIM(90) CTDATA PERRCD(1)
001300131104     D WDCT            S             30    DIM(4)  CTDATA PERRCD(1)
001400131104     D WTIS9           S             30    DIM(10) CTDATA PERRCD(1)
001500131104
001600131104
001700131104     D*-----------
001800131104     D* DS RIDEFINIZIONE SCHIERE A PROGRAMMA
001900131104     D*-----------
002000131104     D DS_WCAU         DS
002100131104     D  dsCAU_BRT                    15
002200131104     D  dsCAU_CLI                    15
002300131104     D  dsCAU_DES                    60
002400131104     D*
002500131104     D DS_WDCT         DS
002600131104     D  dsDCT_BRT                    15
002700131104     D  dsDCT_CLI                    15
002800131104     D*
002900131104     D DS_WTIS9        DS
003000131104     D  dsTIS9_BRT                   15
003100131104     D  dsTIS9_CLI                   15
003200131104
003300131104
003400041015     D*-----------
003500041015     D* SCHIERE
003600041015     D*-----------
003700131104     D skCAU_BRT       S             10    DIM(%elem(WCAU))
003800131104     D skCAU_CLI       S             10    DIM(%elem(WCAU))
003900131104     D skCAU_DES       S             60    DIM(%elem(WCAU))
004000131104     D jCAU            S              3  0 INZ(*zeros)
004100131104     D skDCT_BRT       S              1    DIM(%elem(WDCT))
004200131104     D skDCT_CLI       S             10    DIM(%elem(WDCT))
004300131104     D jDCT            S              3  0 INZ(*zeros)
004400131104     D skTIS9_BRT      S             10    DIM(%elem(WTIS9))
004500131104     D skTIS9_CLI      S             10    DIM(%elem(WTIS9))
004600131104     D jTIS9           S              3  0 INZ(*zeros)
004700131104     D skTADCod        S              2    DIM(100)
004800131104     D skTADRag        S              1    DIM(100)
004900041015     D jTAD            S              3  0 INZ(*zeros)
005000030924
005100041015     D*-----------
005200041015     D* VARIABILI D WRK
005300041015     D*-----------
005400031103     D prmppi          S             50
005500030924     D prmesito        S              1
005600030924     D wrkesito        S                   like(prmesito)
005700031103     D j               S              2  0
005800130807     D wTADCod         S              2
005900131004     D wWrkSts         S             25
006000131004
006100131004
006200131004     D*-----------
006300131004     D* DEFINIZIONE DS ESTERNE
006400131004     D*-----------
006500131004     D DTAD          e ds                  inz
006600131004
006700131004
006800031103     D*-----------
006900031103     D* VARIABILI RIFERITE AL DATA-BASE
007000031103     D*-----------
007100080618     D wKstbKSU        S                   like(stbKSU)
007200080618     D wKstbFTR        S                   like(stbFTR)
007300030924
007400110705
007500031103     D*-----------
007600031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
007700031103     D*-----------
007800031103     D DSPPI           DS
007900080618     D  w§PPIKSU                      7  0
008000131004
008100131004
008200150414     D*-----------
008300150414     D* DS FORMATI RECORD CLIENTE
008400150414     D*-----------
008500150414     D DS_CLI_HR1      DS
008600150414     D  dcHR1_REC_ID                  3    inz('HR1')
008700150414     D  dcHR1_APPLIC                  3    inz('784')
008800150414     D  dcHR1_SND_ID                 35    inz('BRTXX-IT')
008900150414     D  dcHR1_SND_NAM                35    inz('BRT S.P.A.')
009000150414     D  dcHR1_SND_CIT                35    inz(*blanks)
009100150414     D  dcHR1_RCV_ID                 35    inz('54205031500184')
009200150414     D  dcHR1_RCV_NAM                35    inz(*blanks)
009300150414     D  dcHR1_RCV_CIT                35    inz(*blanks)
009400150414     D  dcHR1_TOT_MSG                35    inz(*blanks)
009500150414     D  dcHR1_DOC_DAT                12    inz(*blanks)
009600150414     D  dcHR1_FILL_01                37    inz(*blanks)
009700150414     D  dcHR1_TOT                   300    inz(*blanks)
009800150414     D*
009900150414     D DS_CLI_ST1      DS
010000150414     D  dcST1_REC_ID                  3    inz('ST1')
010100150414     D  dcST1_RMA                    35    inz(*blanks)
010200150414     D  dcST1_RMN                    10    inz(*blanks)
010300150414     D  dcST1_EVTCOD                  3    inz(*blanks)
010400150414     D  dcST1_REASON                  3    inz(*blanks)
010500150414     D  dcST1_EVTDAT                  8    inz(*blanks)
010600150414     D  dcST1_EVTORA                  4    inz(*blanks)
010700150414     D  dcST1_NEXTDAT                 8    inz(*blanks)
010800150414     D  dcST1_NEXTORD                 4    inz(*blanks)
010900150414     D  dcST1_NEXTORA                 4    inz(*blanks)
011000150414     D  dcST1_TOT_HU                  4    inz(*blanks)
011100150414     D  dcST1_FIRMA                  35    inz(*blanks)
011200150414     D  dcST1_FREETXT                70    inz(*blanks)
011300150414     D  dcST1_WAYBCMR                17    inz(*blanks)
011400150414     D  dcST1_WAYBRTN                17    inz(*blanks)
011500150414     D  dcST1_EVTLOC                 35    inz(*blanks)
011600150414     D  dcST1_GPS_LAT                25    inz(*blanks)
011700150414     D  dcST1_GPS_LON                25    inz(*blanks)
011800150414     D  dcST1_PKB                     7    inz(*blanks)
011900150414     D  dcST1_VLB                     6    inz(*blanks)
012000150414     D  dcST1_METERS                  6    inz(*blanks)
012100150414     D  dcST1_COD_AMO                15    inz(*blanks)
012200150414     D  dcST1_COD_CUR                 3    inz(*blanks)
012300150414     D  dcST1_POD_URL               512    inz(*blanks)
012400150414     D  dcST1_FILL_01                41    inz(*blanks)
012500150414     D  dcST1_TOT                   900    inz(*blanks)
012600030924
012700030325
012800030924     C* MAIN/
012900041015     C*
013000041015     C* Carica i dati tabellati
013100041015     C                   EXSR      cartab
013200140308     C*
013300140308     C* Spia dati da inviare
013400140308     C                   SETOFF                                       45
013500041015     C*
013600041015     C* Esegue traduzione
013700140308     C                   SETOFF                                       31
013800131004     C  N31              EXSR      chkRECFILMBR
013900131004     C  N31              EXSR      traduci
014000110705     C*
014100921023     C                   SETON                                        LR
014200131004
014300131004
014400131004
014500131004     C     chkRECFILMBR  BEGSR
014600131004     C*
014700131004     C     *start        setll     TIVAWWWT
014800131004     C                   if        %found(TIVAWWWT)
014900131004     C                   seton                                        31
015000131004     C                   endif
015100131004     C*
015200131004     C                   ENDSR
015300131004
015400131004
015500131004
015600131004     C     wriOUTPUT     BEGSR
015700131004     C*
015800131004     C                   write     TIVAW000
015900131004     C*
016000131004     C                   ENDSR
016100131004
016200130807
016300041015
016400041015     C     cartab        BEGSR
016500041015     C*
016600041015     C* Caricamento tipi anomalia C/A
016700041015     C     'TAD'         SETLL     tntbe01l
016800041015     C                   IF        %equal(tntbe01l)
016900041015     C     'TAD'         READE     tntbe01l
017000041015     C                   DOW       not %eof(tntbe01l)
017100041015     C                   EVAL      DTAD = tbeUNI
017200041015     C                   ADD       1             jTAD
017300041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
017400041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
017500041015     C     'TAD'         READE     tntbe01l
017600041015     C                   ENDDO
017700041015     C                   ENDIF
017800131104     C*
017900131104     C* Carico la mappatura dei codici status personalizzati cliente
018000131104     C                   z-add     1             jCAU
018100131104     C                   dow       jCAU <= %elem(WCAU) and
018200131104     C                                     WCAU(jCAU) <> *blanks
018300131104     C                   eval      DS_WCAU = WCAU(jCAU)
018400131104     C                   eval      skCAU_BRT(jCAU) = dsCAU_BRT
018500131104     C                   eval      skCAU_CLI(jCAU) = dsCAU_CLI
018600131104     C                   eval      skCAU_DES(jCAU) = dsCAU_DES
018700131104     C                   add       1             jCAU
018800131104     C                   enddo
018900131104     C*
019000131104     C                   z-add     1             jDCT
019100131104     C                   dow       jDCT <= %elem(WDCT) and
019200131104     C                                     WDCT(jDCT) <> *blanks
019300131104     C                   eval      DS_WDCT = WDCT(jDCT)
019400131104     C                   eval      skDCT_BRT(jDCT) = dsDCT_BRT
019500131104     C                   eval      skDCT_CLI(jDCT) = dsDCT_CLI
019600131111     C                   add       1             jDCT
019700131104     C                   enddo
019800131104     C*
019900131104     C                   z-add     1             jTIS9
020000131104     C                   dow       jTIS9 <= %elem(WTIS9) and
020100131104     C                                      WTIS9(jTIS9) <> *blanks
020200131104     C                   eval      DS_WTIS9 = WTIS9(jTIS9)
020300131104     C                   eval      skTIS9_BRT(jTIS9) = dsTIS9_BRT
020400131104     C                   eval      skTIS9_CLI(jTIS9) = dsTIS9_CLI
020500131104     C                   add       1             jTIS9
020600131104     C                   enddo
020700131104     C*
020800131104     C                   ENDSR
020900131004
021000131004
021100131004
021200150414     C     wriHeader     BEGSR
021300131004     C*
021400150414     C                   reset                   DS_CLI_HR1
021500150414     C                   eval      dcHR1_DOC_DAT =
021600150414     C                             %trim(%char(%dec(%date():*ISO))) +
021700150414     C                             %subst(%editc(%dec(%time():*ISO):'X'):1:4)
021800150414     C*
021900131004     C                   clear                   vawDTA
022000150414     C                   eval      vawDTA = DS_CLI_HR1
022100131004     C                   exsr      wriOUTPUT
022200131004     C*
022300131004     C                   ENDSR
022400131004
022500030924
022600030924
022700991027     C     traduci       BEGSR
022800031103     C*
022900031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
023000031103     C* di traduzione (trattasi d "post-traduzione")
023100031104     C                   EVAL      wKstbKSU = w§PPIKSU
023200031103     C                   EVAL      wKstbFTR = *blanks
023300110103     C     KEYstb03P     SETLL     tistb03l
023400110103     C                   IF        %equal(tistb03l)
023500110103     C     KEYstb03P     READE     tistb03l
023600991027     C*
023700031103     C* Ciclo x tuttii record del cliente da trasmettere
023800110103     C                   DOW       not %eof(tistb03l)
023900080618     C*
024000150414     C                   SETON                                        50
024100031103     C*
024200031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
024300031103     C* deve restituire al cliente nel file tradotto
024400031103     C                   IF        stbFTR = *blanks                             * ridondante
024500110103     C*
024600110103     C* Gestisco solamente gli stati standard
024700130808     C                   IF        (stbTIS= '1' OR
024800130906     C                             (stbTIS= '9' AND
024900151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
025000151117     C                              (stbPRS= '1'                ))) AND
025100130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
025200130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
025300131004     C*
025400131004     C* Per "sicurezza" non invio stati senza data
025500131004     C                   IF        stbDAS > *zeros
025600110103     C*
025700110103     C                   SETOFF                                       50
025800031103     C*
025900031103     C* Innanzitutto chaino la bolla direttamente su TITAS
026000031103     C     KEYtas30P     CHAIN     titas30c
026100031103     C                   IF        %found(titas30c)
026200080618     C*
026300110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
026400140606     C                   SETOFF                                       80
026500110103     C                   IF        stbTIS = '1'
026600031103     C                   Z-ADD     1             j
026700131104     C     stbCOS        LOOKUP    skCAU_BRT(j)                           55
026800131113     C                   IF        %found AND skCAU_CLI(j) <> 'NONINVIARE'
026900131104     C                   EVAL      wWrkSts = skCAU_CLI(j)
027000140514     C                   SETOFF                                       50
027100140606     C                   SETON                                        80
027200031103     C                   ELSE
027300031103     C                   SETON                                        50
027400031103     C                   ENDIF
027500110103     C                   ENDIF
027600110103     C*
027700110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
027800140606     C                   IF        not *IN80
027900140514     C***                IF        stbTIS <> '1'
028000110103     C                   Z-ADD     1             j
028100131104     C     stbCOS        LOOKUP    skTIS9_BRT(j)                          55
028200131113     C                   IF        %found AND skTIS9_CLI(j) <> 'NONINVIARE'
028300131104     C                   EVAL      wWrkSts = skTIS9_CLI(j)
028400140514     C                   SETOFF                                       50
028500140606     C                   SETON                                        80
028600110103     C                   ELSE
028700110103     C                   SETON                                        50
028800110103     C                   ENDIF
028900140514     C***                ENDIF
029000140606     C                   ENDIF
029100041015     C*
029200080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
029300130807     C                   IF        stbPRS =  '5'
029400080618     C*
029500130807     C                   SETOFF                                       50
029600080618     C*
029700080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
029800130807     C                   Z-ADD     1             jTAD
029900130807     C                   MOVEL(P)  stbCOS        wTADCod
030000130807     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
030100130807     C                   IF        %found
030200130807     C                   Z-ADD     1             j
030300131104     C     skTADRag(jTAD)LOOKUP    skDCT_BRT(j)                           70
030400130807     C                   IF        %found
030500131113     C                   IF        %trim(skDCT_CLI(j)) <> 'NONINVIARE'
030600131104     C                   EVAL      wWrkSts = skDCT_CLI(j)
030700140514     C                   SETOFF                                       50
030800131004     C                   ELSE
030900131004     C                   SETON                                        50
031000131004     C                   ENDIF
031100130807     C                   ELSE
031200130807     C                   SETON                                        50
031300130807     C                   ENDIF
031400130807     C                   ELSE
031500130807     C                   SETON                                        50
031600130807     C                   ENDIF
031700150414     C                   ENDIF
031800140308     C*
031900140308     C* Al primo dettaglio da inviare => scrivo anche testata
032000140313     C                   IF        not *IN50
032100140313     C  N45              EXSR      wriHeader
032200140308     C                   SETON                                        45
032300140313     C                   ENDIF
032400080618     C*
032500080618     C* Scarico il buffer d output
032600131004     C  N50              EXSR      wriStato
032700110103     C*
032800131010     C                   ENDIF
032900131004     C                   ENDIF
033000110103     C                   ENDIF
033100110103     C                   ENDIF
033200031103     C*
033300031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
033400031103     C                   EVAL      stbFTR = 'T'
033500031103     C                   UPDATE    tistb000
033600991027     C*
033700110103     C     KEYstb03P     READE     tistb03l
033800031103     C                   SETOFF                                       50
033900030325     C                   ENDDO
034000031103     C                   ENDIF
034100991027     C*
034200030325     C                   EVAL      wrkesito = '0'
034300991027     C*
034400150414     C                   ENDSR
034500031103
034600031103
034700031103
034800150414     C     wriStato      BEGSR
034900150414     C*
035000150414     C                   reset                   DS_CLI_ST1
035100150414     C                   eval      dcST1_RMA    = %trim(stbRMA)
035200150414     C                   eval      dcST1_RMN    = %trim(dcST1_RMA)
035300150414     C                   eval      dcST1_EVTCOD = %subst(wWrkSts:1:3)
035400150414     C                   eval      dcST1_REASON = %subst(wWrkSts:4:3)
035500150414     C                   eval      dcST1_EVTDAT = %trim(%editc(stbDAS:'X'))
035600150414     C                   eval      dcST1_EVTORA = %trim(%editc(stbORS:'X'))
035700150414     C*
035800150414     C                   clear                   vawDTA
035900150414     C                   eval      vawDTA = DS_CLI_ST1
036000150414     C                   exsr      wriOUTPUT
036100031103     C*
036200031103     C                   ENDSR
036300031103
036400031103
036500991027
036600991027      /TITLE Operazioni iniziali.
036700991027     C     *inzsr        BEGSR
036800991027     C*
036900991027     C     *ENTRY        PLIST
037000031103     C                   parm                    prmppi
037100991027     C     wrkesito      parm      wrkesito      prmesito
037200031103     C*
037300031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
037400031103     C                   EVAL      DSPPI = prmppi
037500030709     C*
037600030709     C* Definizione chiavi
037700030924     C*
037800110103     C* Chiave su TISTB03L - Parziale
037900110103     C     KEYstb03P     KLIST
038000031104     C                   KFLD                    wKstbKSU
038100031103     C                   KFLD                    wKstbFTR
038200031103     C*
038300031103     C* Chiave su TITAS30C - Parziale
038400031103     C     KEYtas30P     KLIST
038500031103     C                   KFLD                    stbAAS
038600031103     C                   KFLD                    stbLNP
038700031103     C                   KFLD                    stbNRS
038800031103     C                   KFLD                    stbNSP
038900991027     C*
039000991027     C                   ENDSR
039100131104** WCAU - CAUSALI BRT / CLIENTE: 15 byte BRT / 15 byte CLI / 60 byte DESCRIZIONE ITA
039200150414A              348265         L.AVVISO                                                     1
039300150414ARS            348265         AVVISATO-RIMANDA SVINCOLO 1GG                                2 ex 32
039400150414AVV            348265         LASCIATO AVVISO 1GG                                          3
039500150414AV2            90 45          LASCIATO AVVISO 2GG                                          4
039600150414AV3            90 45          LASCIATO AVVISO 3GG                                          5
039700150414AV5            90 45          LASCIATO AVVISO 5GG                                          6
039800150414AV7            90 45          LASCIATO AVVISO 7GG                                          7
039900150414AV9            90 45          LASCIATO AVVISO 9 GIORNI                                     8
040000150414A16            348265         L.A. RIMANDA LO SVINCOLO                                     9 ex 32
040100150414A23            348269         L.A. CHIUSO FINO AL                                         10
040200131113CPO            NONINVIARE     CAMBIO PORTO                                                11
040300150414DAN            29 218         COLLO/I DANNEGGIATO/I                                       12
040400150414DIR            999999         DIROTTAMENTO                                                13
040500131113F              NONINVIARE     FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
040600150414G              348265         GIACENZA GENERICA TEMPORANEA                                15 ex 32
040700150414GEN            348265         GIACENZA GENERICA                                           16 ex 32
040800131113G02            NONINVIARE     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
040900131113G03            NONINVIARE     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
041000131113G05            NONINVIARE     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
041100131113G09            NONINVIARE     ATTENDERE 9 GIORNI                                          20
041200150414IDD            29 218         CHIUSURA SPEDIZIONE CON IDD                                 21
041300150414MAN            90 117         COLLO/I MANCANTE/I                                          22
041400150414MIC            113310         MESSA IN CONSEGNA                                           23
041500131113N              NONINVIARE     NON FATTA                                                   24
041600150414NAV            29 218         COLLO SCONDIZIONATO                                         25
041700131113NIC            NONINVIARE     TOLTA DALLA CONSEGNA                                        26
041800150414P              21 330         CONSEGNA PARZIALE                                           27
041900150414PAT            348269         FESTIVITA PATRONALE                                         28
042000150414R              21 231         RISERVA                                                     29
042100131113RDC            NONINVIARE     RIPRISTINO                                                  30
042200150414RFD            348265         RIFIUTATO                                                   31
042300150414RFM            348265         RIFIUTATO                                                   32
042400150414RIC            113310         RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
042500131113RIP            NONINVIARE     RIPRISTINO                                                  34
042600150414RS             21 231         RISERVA                                                     35
042700150414T              348269         CHIUSO PER TURNO                                            36
042800150414WA             90 117         AMMANCO SENZA EVENTO                                        37
042900150414WAE            90 117         AMMANCO CON EVENTO                                          38
043000150414WM             90 117         MANCANZA SENZA EVENTO                                       39
043100150414WME            90 117         MANCANZA CON EVENTO                                         40
043200150414WV             348253         AVARIA SENZA EVENTO                                         41
043300150414WVE            348253         AVARIA CON EVENTO                                           42
043400150414ZAL            348266         ALLUVIONE                                                   43
043500150414ZBS            348265         BLOCCO STRADALE                                             44
043600150414ZFR            348269         FESTIVITÀ REGIONALE                                         45
043700150414ZMM            348266         MARE MOLTO MOSSO                                            46
043800150414ZMP            348265         MANIFESTAZIONE PUBBLICA                                     47
043900150414ZNV            348266         NEVICATA ECCEZIONALE                                        48
044000150414ZSC            348265         SCIOPERO                                                    49
044100150414001            348265         RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
044200150414002            348284         MERCE NON ORDINATA O NON CONFORME                           51
044300150414003            348253         MERCE SPEDITA IN RITARDO                                    52
044400150414004            348284         MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
044500150414005            348253         MERCE SPEDITA CON TROPPO ANTICIPO                           54
044600150414006            348265         MERCE RESA SENZA AUTORIZZAZIONE                             55
044700150414007            348284         DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
044800150414008            348292         DESTINATARIO NON PAGA IL C/ASSEGNO                          57
044900150414009            348265         CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58 ex 32
045000150414010            348292         DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
045100150414012            348292         IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
045200150414013            348292         DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
045300150414014            348292         DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
045400150414016            348265         DESTINATARIO RIMANDA LO SVINCOLO AL                         63 ex 32
045500150414017            348265         DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
045600150414018            348265         DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65 ex 32
045700150414019            348269         DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
045800150414020            348213         DESTINATARIO RISULTA TRASFERITO                             67
045900150414021            348213         DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68 ex 16
046000150414022            348213         INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
046100150414023            348269         IL DESTINATARIO E' CHIUSO FINO AL                           70
046200150414024            348269         IL DESTINATARIO E' CHIUSO PER FERIE                         71
046300150414025            348269         IL DESTINATARIO E' CHIUSO                                   72
046400150414026            34875          IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
046500150414027            348284         DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
046600150414028            348269         ESERCIZIO NON ANCORA IN ATTIVITA                            75
046700150414029            348292         IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
046800150414030            348265         IL DESTINATARIO RIFIUTA LA MERCE                            77
046900150414031            348265         IL DESTINATARIO NON INTENDE                                 78
047000150414032            348265         FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79 ex 32
047100150414033            348265         IL DESTINATARIO RICHIEDE                                    80 ex 32
047200150414034            348265         SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
047300150414035            348241         DOCUMENTI INCOMPLETI O MANCANTI                             82
047400150414036            348265         IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
047500150414037            348265         RIFIUTA LA CONSEGNA TASSATIVA                               84
047600150414100            999999         -                                                           85
047700150414101            999999         -                                                           86
047800131113999            NONINVIARE     -                                                           87
047900150414CONSOK         21 310         CONSEGNA OK                                                 88
048000150414RESO           348253         RESO                                                        89
048100131104                                                                                          90
048200131104** WDCT - TIPI ANOMALIE BRT/CLIENTE: 15 byte BRT / 15 byte CLI
048300150414A              90 117         AMMANCO                                                        1
048400150414M              90 117         MANCANZA                                                       2
048500150414V              29 218         AVARIA                                                         3
048600150414E              348253         EVENTO AVVERSO                                                 4
048700131104** WTIS9 - CODICI EVENTO "PERSONALIZZATI" - BRT/CLIENTE: 15 byte BRT / 15 byte CLI
048800150414INCARICO1      90 57                                                                         1
048900150414INCARICO2      90 57                                                                         2
049000150414CONSOK1        21 310                                                                        3
049100150414CONSOK2        21 310                                                                        4
049200131113GIACNODIS1     NONINVIARE                                                                    5
049300131113GIACNODIS2     NONINVIARE                                                                    6
049400131113GIACSIDIS1     NONINVIARE                                                                    7
049500131113GIACSIDIS2     NONINVIARE                                                                    8
049600150414COLNOTAFF      999999                                                                        9
049700131113               NONINVIARE                                                                    10
