000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100215     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300991027
000400091222     FTNTBE01L  IF   E           K DISK
000500091223     FTABEL00F  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700090323     FTISTB02L  UF   E           K DISK    commit
000800090323     FTIVAWWWT  UF A E             DISK    commit
000900091222     FTIGCP51L  IF   E           K DISK
001000031103
001100031103     D*-----------
001200031103     D* SCHIERE A PROGRAMMA
001300031103     D*-----------
001400100510     D CAUBAR          S             10    DIM(92) CTDATA PERRCD(1)
001500100510     D CAUCLI          S             11    DIM(92) CTDATA PERRCD(1)
001600100510     D DESCLI          S             80    DIM(92) CTDATA PERRCD(1)
001700100317     D WDCTBAR         S              1    DIM(4)  CTDATA PERRCD(1)
001800100317     D WDCTCLI         S             10    DIM(4)  CTDATA PERRCD(1)
001900100423     D WDCTDES         S             50    DIM(4)  CTDATA PERRCD(1)
002000041015
002100041015     D*-----------
002200041015     D* SCHIERE
002300041015     D*-----------
002400100317     D skTADCod        S              2    DIM(100)
002500100317     D skTADRag        S              1    DIM(100)
002600091222     D jTAD            S              3  0 INZ(*zeros)
002700100317     D sk2Alavv        s             10    DIM(300)
002800100317     D sk2Agiac        s             10    DIM(300)
002900030924
003000041015     D*-----------
003100041015     D* VARIABILI D WRK
003200041015     D*-----------
003300031103     D prmppi          S             50
003400030924     D prmesito        S              1
003500030924     D wrkesito        S                   like(prmesito)
003600100317     D j               S              3  0 inz
003700100317     D i               s              3  0 inz
003800100317     D ii              s              3  0 inz
003900100317     D wTADCod         S              2    inz
004000100408     D sv_b_ldvBart    S                   like(ds_b_ldvBart)
004100100215
004200091222
004300091222     D*-----------
004400091222     D* DEFINIZIONE DS ESTERNE
004500091222     D*-----------
004600100317     D DTAD          E DS
004700100317     D DS2A          E DS
004800031103
004900031103     D*-----------
005000031103     D* VARIABILI RIFERITE AL DATA-BASE
005100031103     D*-----------
005200100317     D wKstbKSU        S                   like(stbKSU)
005300100317     D wKstbFTR        S                   like(stbFTR)
005400030924
005500030924     D*-----------
005600030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
005700030924     D*-----------
005800030925     D C_DS_Cli        DS
005900100318     D  ds_b_nazISO                   3a   inz('380')
006000100317     D  ds_b_typInfo                  1a   inz('E')
006100100317     D  ds_b_rifmitt                 15a   inz(*blanks)
006200100317     D  ds_b_ldvBart                 20a   inz(*blanks)
006300100317     D  ds_b_dataora                 12a   inz(*zeros)
006400100317     D  ds_b_codcau                   4a   inz(*zeros)
006500100317     D  ds_b_note                   250a   inz(*blanks)
006600091222
006700100318     D ds_Key_bolla    ds
006800091222     D  wstbLNP                       3a
006900091222     D  wstbNRS                       2a
007000091222     D  wstbNSP                       7a
007100100121
007200100318     D ds_data         ds
007300100121     D  wAAAA                         4a
007400100121     D  wMM                           2a
007500100121     D  wGG                           2a
007600100121
007700100318     D ds_ora          ds
007800100121     D  wHHMM                         4a
007900031103
008000031103     D*-----------
008100031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
008200031103     D*-----------
008300031103     D DSPPI           DS
008400080618     D  w§PPIKSU                      7  0
008500030924
008600100215
008700100215
008800030325
008900030924     C* MAIN/
009000100215     C*
009100091222     C*
009200091222     C* Carica i dati tabellati
009300091222     C                   EXSR      cartab
009400041015     C*
009500041015     C* Esegue traduzione
009600991027     C                   EXSR      traduci
009700921023     C                   SETON                                        LR
009800091222
009900091222
010000091222
010100091222     C     cartab        BEGSR
010200091223     C*
010300091223     C* Carico causali evento divise x categoria (A=Lavv G=Giac)
010400091223     C                   clear                   sk2Alavv
010500091223     C                   clear                   sk2Agiac
010600091223     C                   z-add     *zeros        i
010700091223     C                   z-add     *zeros        ii
010800091223     C                   eval      tblKUT = 1
010900091223     C                   eval      tblCOD = '2A'
011000091223     C     KEYtab00P     setll     tabel00f
011100091223     C     KEYtab00P     reade     tabel00f
011200091223     C                   dow       not %eof(tabel00f)
011300091223     C                   movel(p)  tblUNI        ds2a
011400091223     C                   if        §2aFTC = 'G'
011500091223     C                   add       1             i
011600091223     C                   eval      sk2Agiac(i) = tblKEY
011700091223     C                   endif
011800091223     C                   if        §2aFTC = 'A'
011900091223     C                   add       1             ii
012000091223     C                   eval      sk2Alavv(ii) = tblKEY
012100091223     C                   endif
012200091223     C     KEYtab00P     reade     tabel00f
012300091223     C                   enddo
012400091222     C*
012500091222     C* Caricamento tipi anomalia C/A
012600091222     C     'TAD'         SETLL     tntbe01l
012700091222     C                   IF        %equal(tntbe01l)
012800091222     C     'TAD'         READE     tntbe01l
012900091222     C                   DOW       not %eof(tntbe01l)
013000091222     C                   EVAL      DTAD = tbeUNI
013100091222     C                   ADD       1             jTAD
013200091222     C                   EVAL      skTADCod(jTAD) = tbeKE1
013300091222     C                   EVAL      skTADRag(jTAD) = §TADRAGR
013400091222     C     'TAD'         READE     tntbe01l
013500091222     C                   ENDDO
013600091222     C                   ENDIF
013700091222     C*
013800091222     C                   ENDSR
013900991027
014000030924
014100030924
014200991027     C     traduci       BEGSR
014300031103     C*
014400031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
014500031103     C* di traduzione (trattasi d "post-traduzione")
014600031104     C                   EVAL      wKstbKSU = w§PPIKSU
014700031103     C                   EVAL      wKstbFTR = *blanks
014800031103     C     KEYstb02P     SETLL     tistb02l
014900031103     C                   IF        %equal(tistb02l)
015000031103     C     KEYstb02P     READE     tistb02l
015100991027     C*
015200031103     C* Ciclo x tuttii record del cliente da trasmettere
015300031103     C                   DOW       not %eof(tistb02l)
015400031014     C*
015500030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
015600031014     C                   RESET                   C_DS_Cli
015700080618     C*
015800080618     C                   SETON                                        50
015900091223     C                   SETOFF                                       54
016000031103     C*
016100031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
016200031103     C* deve restituire al cliente nel file tradotto
016300031103     C                   IF        stbFTR = *blanks                             * ridondante
016400031103     C*
016500031103     C* Innanzitutto chaino la bolla direttamente su TITAS
016600031103     C     KEYtas30P     CHAIN     titas30c
016700031103     C                   IF        %found(titas30c)
016800091222     C*
016900091223     C* Gestisco gli stati standard
017000100511     C                   IF        (stbTIS = '1')                     OR
017100100511     C                             (stbTIS = '9' AND stbPRS = '1')
017200091222     C*
017300100215     C                   SETOFF                                       50
017400100215     C*
017500100215     C* Inizializzo colli consegnati = NCL e colli no consegnati = 0
017600100215     C                   z-add     *zeros        wColliCons        5 0
017700100215     C                   z-add     *zeros        wColliNoCons      5 0
017800080618     C*
017900080618     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
018000031103     C                   Z-ADD     1             j
018100031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
018200080618     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
018300100423     C                   EVAL      ds_b_codcau = CAUCLI(j)
018400100423     C                   EVAL      ds_b_note   = DESCLI(j)
018500091223     C                   z-add     1             i
018600091223     C     stbCOS        lookup    sk2Agiac(i)                            55
018700091223     C                   if        %found
018800091223     C                   SETON                                        54
018900091223     C                   endif
019000031103     C                   ELSE
019100031103     C                   SETON                                        50
019200031103     C                   ENDIF
019300080618     C                   ENDIF
019400041015     C*
019500091222     C* X stati provenienti dai danni, se trattasi d tipi anomalia A/M/V/E
019600080618     C                   IF        stbPRS =  '5'
019700080618     C*
019800080618     C                   SETOFF                                       50
019900080618     C*
020000080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
020100080618     C                   Z-ADD     1             jTAD
020200080618     C                   MOVEL(P)  stbCOS        wTADCod
020300080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
020400080618     C                   IF        %found
020500080618     C                   Z-ADD     1             j
020600080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
020700100215     C                   IF        %found AND WDCTBAR(j) <> 'NON INVIARE'
020800100423     C                   EVAL      ds_b_codcau = WDCTCLI(j)
020900100423     C                   EVAL      ds_b_note   = WDCTDES(j)
021000080618     C                   ELSE
021100080618     C                   SETON                                        50
021200080618     C                   ENDIF
021300080618     C                   ELSE
021400080618     C                   SETON                                        50
021500080618     C                   ENDIF
021600080618     C                   ENDIF
021700080618     C*
021800080618     C* Scarico il buffer d output
021900080618     C  N50              EXSR      WRIREC
022000100209     C*
022100100209     C                   ENDIF
022200100209     C                   ENDIF
022300031103     C*
022400031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
022500031103     C                   EVAL      stbFTR = 'T'
022600031103     C                   UPDATE    tistb000
022700991027     C*
022800031103     C     KEYstb02P     READE     tistb02l
022900031103     C                   SETOFF                                       50
023000030325     C                   ENDDO
023100031103     C                   ENDIF
023200991027     C*
023300030325     C                   EVAL      wrkesito = '0'
023400991027     C*
023500910830     C                   ENDSR
023600031103
023700100215
023800031103
023900100318     C     wrirec        BEGSR
024000100408     C*
024100100408     C                   setoff                                       40
024200091222     C*
024300091222     C                   movel     stbLNP        wstbLNP
024400091222     C                   movel     stbNRS        wstbNRS
024500091222     C                   movel     stbNSP        wstbNSP
024600091222     C*
024700100215     C                   eval      ds_data  = *zeros
024800100215     C                   eval      ds_ora   = *zeros
024900100121     C                   movel     stbDAS        ds_data
025000100121     C                   movel     stbORS        wHHMM
025100100318     C*
025200100318     C                   eval      ds_b_rifmitt = stbRMA
025300100318     C                   eval      ds_b_ldvBart = ds_key_bolla
025400100318     C                   eval      ds_b_dataora = wGG   +
025500100318     C                                            wMM   +
025600100318     C                                            wAAAA +
025700100318     C                                            ds_ora
025800091223     C*
025900091223     C* Se causale d tipo giacenza => reperisco
026000100318     C***                if        *in54 = *on
026100091222     C*
026200091222     C* Costruisco la chiave sul file TIGCP51L
026300100318     C***                eval      gcpFRG = *zeros
026400091222     C*
026500091222     C* Reperisco le giacenze relative alla bolla corrente
026600100318     C***  KEYgcp51C     chain     tigcp51l
026700100318     C***                if        %found(tigcp51l)
026800091222     C* ...giacenza in attesa d disposizioni
026900100318     C***                if        gcpDUR  > *zeros AND
027000100318     C***                          gcpDDM  = *zeros AND
027100100318     C***                          gcpFAS  < 030
027200100318     C***                eval      ds_b_codcau = '001'
027300100423     C***                eval      ds_b_note   =
027400100318     C***                endif
027500091222     C* ...giacenza con disposizioni
027600100318     C***                if        gcpDUR  > *zeros AND
027700100318     C***                          gcpDDM  > *zeros AND
027800100318     C***                          gcpFAS >= 030
027900100318     C***                eval      ds_b_codcau = '018'
028000100423     C***                eval      ds_b_note   =
028100100318     C***                endif
028200100318     C***                endif
028300100318     C***                endif
028400100408     C*
028500100408     C* A rottura d spedizione inizializzo gli indicatori d wrk
028600100408     C                   if        ds_b_ldvBart <> sv_b_ldvBart
028700100408     C                   setoff                                       41
028800100408     C                   eval      sv_b_ldvBart = ds_b_ldvBart
028900100408     C                   endif
029000100408     C*
029100100408     C* Forzatura no consegne OK dopo consegna con RISERVA
029200100408     C                   if        ds_b_codcau = 'ERE'
029300100408     C                   seton                                        41
029400100408     C                   endif
029500100408     C*
029600100408     C                   if        stbCOS = 'CONSOK' and *in41
029700100408     C                   seton                                        40
029800100408     C                   endif
029900091222     C*
030000091222     C                   clear                   tivaw000
030100031103     C*
030200031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
030300100318     C                   eval      VAWDTA = C_DS_Cli
030400100408     C  N40              write     TIVAW000
030500031103     C*
030600031103     C                   ENDSR
030700031103
030800031103
030900991027
031000991027      /TITLE Operazioni iniziali.
031100991027     C     *inzsr        BEGSR
031200991027     C*
031300991027     C     *ENTRY        PLIST
031400031103     C                   parm                    prmppi
031500991027     C     wrkesito      parm      wrkesito      prmesito
031600031103     C*
031700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
031800031103     C                   EVAL      DSPPI = prmppi
031900030709     C*
032000030709     C* Definizione chiavi
032100030924     C*
032200031103     C* Chiave su TISTB02L - Parziale
032300031103     C     KEYstb02P     KLIST
032400031104     C                   KFLD                    wKstbKSU
032500031103     C                   KFLD                    wKstbFTR
032600031103     C*
032700031103     C* Chiave su TITAS30C - Parziale
032800031103     C     KEYtas30P     KLIST
032900031103     C                   KFLD                    stbAAS
033000031103     C                   KFLD                    stbLNP
033100031103     C                   KFLD                    stbNRS
033200031103     C                   KFLD                    stbNSP
033300091222     C*
033400091222     C* Chiave su TIGCP51L - Completa
033500091222     C     KEYgcp51C     KLIST
033600091222     C                   KFLD                    stbAAS
033700091222     C                   KFLD                    stbLNP
033800091222     C                   KFLD                    stbNRS
033900091222     C                   KFLD                    stbNSP
034000091222     C                   KFLD                    gcpFRG
034100091223     C*
034200091223     C* Chiave su TABEL00F - Parziale
034300091223     C     KEYtab00P     KLIST
034400091223     C                   KFLD                    tblKUT
034500091223     C                   KFLD                    tblCOD
034600991027     C*
034700991027     C                   ENDSR
034800091222** CAUBAR - CAUSALI BARTOLINI
034900100317A                       L.AVVISO                                                       001
035000100317ARS                     AVVISATO-RIMANDA SVINCOLO 1GG                                  002
035100100317AVV                     LASCIATO AVVISO 1GG                                            003
035200100317AV2                     LASCIATO AVVISO 2GG                                            004
035300100317AV3                     LASCIATO AVVISO 3GG                                            005
035400100317AV5                     LASCIATO AVVISO 5GG                                            006
035500100317AV7                     LASCIATO AVVISO 7GG                                            007
035600100317AV9                     LASCIATO AVVISO 9 GIORNI                                       008
035700100317A16                     L.A. RIMANDA LO SVINCOLO                                       009
035800100317A23                     L.A. CHIUSO FINO AL                                            010
035900100317CPO                     CAMBIO PORTO                                                   011
036000100317DAN                     COLLO/I DANNEGGIATO/I                                          012
036100100317DIR                     DIROTTAMENTO                                                   013
036200100317F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA               014
036300100317G                       GIACENZA GENERICA TEMPORANEA                                   015
036400100317GEN                     GIACENZA GENERICA                                              016
036500100317G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                    017
036600100317G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                    018
036700100317G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                       019
036800100317G09                     ATTENDERE 9 GIORNI                                             020
036900100317IDD                     CHIUSURA SPEDIZIONE CON IDD                                    021
037000100317MAN                     COLLO/I MANCANTE/I                                             022
037100100317MIC                     MESSA IN CONSEGNA                                              023
037200100317N                       NON FATTA                                                      024
037300100317NAV                     COLLO SCONDIZIONATO                                            025
037400100317NIC                     TOLTA DALLA CONSEGNA                                           026
037500100317P                       CONSEGNA PARZIALE                                              027
037600100317PAT                     FESTIVITA PATRONALE                                            028
037700100317R                       RISERVA                                                        029
037800100317RDC                     RIPRISTINO                                                     030
037900100317RFD                     RIFIUTATO                                                      031
038000100317RFM                     RIFIUTATO                                                      032
038100100317RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                       033
038200100317RIP                     RIPRISTINO                                                     034
038300100317RS                      RISERVA                                                        035
038400100317T                       CHIUSO PER TURNO                                               036
038500100317WA                      AMMANCO SENZA EVENTO                                           037
038600100317WAE                     AMMANCO CON EVENTO                                             038
038700100317WM                      MANCANZA SENZA EVENTO                                          039
038800100317WME                     MANCANZA CON EVENTO                                            040
038900100317WV                      AVARIA SENZA EVENTO                                            041
039000100317WVE                     AVARIA CON EVENTO                                              042
039100100317ZAL                     ALLUVIONE                                                      043
039200100317ZBS                     BLOCCO STRADALE                                                044
039300100317ZFR                     FESTIVITÀ REGIONALE                                            045
039400100317ZMM                     MARE MOLTO MOSSO                                               046
039500100317ZMP                     MANIFESTAZIONE PUBBLICA                                        047
039600100317ZNV                     NEVICATA ECCEZIONALE                                           048
039700100317ZSC                     SCIOPERO                                                       049
039800100317001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO                            050
039900100317002                     MERCE NON ORDINATA O NON CONFORME                              051
040000100317003                     MERCE SPEDITA IN RITARDO                                       052
040100100317004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                       053
040200100317005                     MERCE SPEDITA CON TROPPO ANTICIPO                              054
040300100317006                     MERCE RESA SENZA AUTORIZZAZIONE                                055
040400100317007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE                  056
040500100317008                     DESTINATARIO NON PAGA IL C/ASSEGNO                             057
040600100317009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO             058
040700100317010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO                059
040800100317012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO                 060
040900100317013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO                061
041000100317014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                    062
041100100317016                     DESTINATARIO RIMANDA LO SVINCOLO AL                            063
041200100317017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                      064
041300100317018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO               065
041400100317019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO         066
041500100317020                     DESTINATARIO RISULTA TRASFERITO                                067
041600100317021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO          068
041700100317022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO              069
041800100317023                     IL DESTINATARIO E' CHIUSO FINO AL                              070
041900100317024                     IL DESTINATARIO E' CHIUSO PER FERIE                            071
042000100317025                     IL DESTINATARIO E' CHIUSO                                      072
042100100317026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO             073
042200100317027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE             074
042300100317028                     ESERCIZIO NON ANCORA IN ATTIVITA                               075
042400100317029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO            076
042500100317030                     IL DESTINATARIO RIFIUTA LA MERCE                               077
042600100317031                     IL DESTINATARIO NON INTENDE                                    078
042700100317032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO             079
042800100317033                     IL DESTINATARIO RICHIEDE                                       080
042900100317034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE               081
043000100317035                     DOCUMENTI INCOMPLETI O MANCANTI                                082
043100100317036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA           083
043200100317037                     RIFIUTA LA CONSEGNA TASSATIVA                                  084
043300100317100                     -                                                              085
043400100317101                     -                                                              086
043500100317999                     -                                                              087
043600100317CONSOK                  CONSEGNA OK                                                    088
043700100317RESO                    RESO                                                           089
043800100317                                                                                       090
043900100511INCARICO1                                                                              091
044000100511INCARICO2                                                                              092
044100091222** CAUCLI - CAUSALI CLIENTE
044200100317NCO                     L.AVVISO                                                       001
044300100317NCO                     AVVISATO-RIMANDA SVINCOLO 1GG                                  002
044400100317NCO                     LASCIATO AVVISO 1GG                                            003
044500100317NCO                     LASCIATO AVVISO 2GG                                            004
044600100317NCO                     LASCIATO AVVISO 3GG                                            005
044700100317NCO                     LASCIATO AVVISO 5GG                                            006
044800100317NCO                     LASCIATO AVVISO 7GG                                            007
044900100317NCO                     LASCIATO AVVISO 9 GIORNI                                       008
045000100317NOT                     L.A. RIMANDA LO SVINCOLO                                       009
045100100317NCO                     L.A. CHIUSO FINO AL                                            010
045200100317NON INVIARE             CAMBIO PORTO                                                   011
045300100317NMG                     COLLO/I DANNEGGIATO/I                                          012
045400100317PER                     DIROTTAMENTO                                                   013
045500100317NRC                     FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA               014
045600100317NOT                     GIACENZA GENERICA TEMPORANEA                                   015
045700100317NOT                     GIACENZA GENERICA                                              016
045800100317NOT                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                    017
045900100317NOT                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                    018
046000100317NOT                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                       019
046100100317NOT                     ATTENDERE 9 GIORNI                                             020
046200100317NMG                     CHIUSURA SPEDIZIONE CON IDD                                    021
046300100317NMI                     COLLO/I MANCANTE/I                                             022
046400100317RUT                     MESSA IN CONSEGNA                                              023
046500100317NFT                     NON FATTA                                                      024
046600100317XXX                     COLLO SCONDIZIONATO                                            025
046700100317DES                     TOLTA DALLA CONSEGNA                                           026
046800100317EPA                     CONSEGNA PARZIALE                                              027
046900100317NFE                     FESTIVITA PATRONALE                                            028
047000100317ERE                     RISERVA                                                        029
047100100317NON INVIARE             RIPRISTINO                                                     030
047200100317ROT                     RIFIUTATO                                                      031
047300100317ROT                     RIFIUTATO                                                      032
047400100317RUT                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                       033
047500100317NON INVIARE             RIPRISTINO                                                     034
047600100317ERE                     RISERVA                                                        035
047700100317NCO                     CHIUSO PER TURNO                                               036
047800100317NNL                     AMMANCO SENZA EVENTO                                           037
047900100317NNL                     AMMANCO CON EVENTO                                             038
048000100317NNL                     MANCANZA SENZA EVENTO                                          039
048100100317NNL                     MANCANZA CON EVENTO                                            040
048200100317NMG                     AVARIA SENZA EVENTO                                            041
048300100317NMG                     AVARIA CON EVENTO                                              042
048400100317NFM                     ALLUVIONE                                                      043
048500100317NFM                     BLOCCO STRADALE                                                044
048600100317NFE                     FESTIVITÀ REGIONALE                                            045
048700100317NFM                     MARE MOLTO MOSSO                                               046
048800100317NFM                     MANIFESTAZIONE PUBBLICA                                        047
048900100317NFM                     NEVICATA ECCEZIONALE                                           048
049000100317NFM                     SCIOPERO                                                       049
049100100317ROT                     RIFIUTA SENZA SPECIFICARE IL MOTIVO                            050
049200100317RNS                     MERCE NON ORDINATA O NON CONFORME                              051
049300100317RFH                     MERCE SPEDITA IN RITARDO                                       052
049400100317RNS                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                       053
049500100317RFH                     MERCE SPEDITA CON TROPPO ANTICIPO                              054
049600100317XXX                     MERCE RESA SENZA AUTORIZZAZIONE                                055
049700100317RNS                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE                  056
049800100317ROT                     DESTINATARIO NON PAGA IL C/ASSEGNO                             057
049900100317XXX                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO             058
050000100317ROT                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO                059
050100100317ROT                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO                 060
050200100317ROT                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO                061
050300100317ROT                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                    062
050400100317ROT                     DESTINATARIO RIMANDA LO SVINCOLO AL                            063
050500100317NCO                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                      064
050600100317XXX                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO               065
050700100317NDI                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO         066
050800100317NDI                     DESTINATARIO RISULTA TRASFERITO                                067
050900100317NDI                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO          068
051000100317NDD                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO              069
051100100317NCO                     IL DESTINATARIO E' CHIUSO FINO AL                              070
051200100317NCO                     IL DESTINATARIO E' CHIUSO PER FERIE                            071
051300100317NCO                     IL DESTINATARIO E' CHIUSO                                      072
051400100317NPD                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO             073
051500100317RNS                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE             074
051600100317NCO                     ESERCIZIO NON ANCORA IN ATTIVITA                               075
051700100317ROT                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO            076
051800100317ROT                     IL DESTINATARIO RIFIUTA LA MERCE                               077
051900100317ROT                     IL DESTINATARIO NON INTENDE                                    078
052000100317NRC                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO             079
052100100317XXX                     IL DESTINATARIO RICHIEDE                                       080
052200100317NFM                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE               081
052300100317NFD                     DOCUMENTI INCOMPLETI O MANCANTI                                082
052400100317ERE                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA           083
052500100317ROT                     RIFIUTA LA CONSEGNA TASSATIVA                                  084
052600100317NON INVIARE             -                                                              085
052700100317NON INVIARE             -                                                              086
052800100317NON INVIARE             -                                                              087
052900100317OK                      CONSEGNA OK                                                    088
053000100317DEV                     RESO                                                           089
053100100317NON INVIARE             -                                                              090
053200100510TRA                     ACCETTAZIONE BOLLA                                             091
053300100510TRA                     ACCETTAZIONE BOLLA                                             092
053400091222** DESCLI - DESCRIZIONI CAUSALI CLIENTE
053500091222L.AVVISO
053600091222AVVISATO-RIMANDA SVINCOLO 1GG
053700091222LASCIATO AVVISO 1GG
053800091222LASCIATO AVVISO 2GG
053900091222LASCIATO AVVISO 3GG
054000091222LASCIATO AVVISO 5GG
054100091222LASCIATO AVVISO 7GG
054200091222LASCIATO AVVISO 9 GIORNI
054300091222L.A. RIMANDA LO SVINCOLO
054400091222L.A. CHIUSO FINO AL
054500091222CAMBIO PORTO
054600091222COLLO/I DANNEGGIATO/I
054700091222DIROTTAMENTO
054800091222FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
054900091222GIACENZA GENERICA TEMPORANEA
055000091222GIACENZA GENERICA
055100091222ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
055200091222ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
055300091222ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
055400091222ATTENDERE 9 GIORNI
055500091222CHIUSURA SPEDIZIONE CON IDD
055600091222COLLO/I MANCANTE/I
055700091222MESSA IN CONSEGNA
055800091222NON FATTA
055900091222COLLO SCONDIZIONATO
056000091222TOLTA DALLA CONSEGNA
056100091222CONSEGNA PARZIALE
056200091222FESTIVITA PATRONALE
056300091222RISERVA
056400091222RIPRISTINO
056500091222RIFIUTATO
056600091222RIFIUTATO
056700091222RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
056800091222RIPRISTINO
056900091222RISERVA
057000091222CHIUSO PER TURNO
057100091222AMMANCO SENZA EVENTO
057200091222AMMANCO CON EVENTO
057300091222MANCANZA SENZA EVENTO
057400091222MANCANZA CON EVENTO
057500091222AVARIA SENZA EVENTO
057600091222AVARIA CON EVENTO
057700091222ALLUVIONE
057800091222BLOCCO STRADALE
057900091222FESTIVITÀ REGIONALE
058000091222MARE MOLTO MOSSO
058100091222MANIFESTAZIONE PUBBLICA
058200091222NEVICATA ECCEZIONALE
058300091222SCIOPERO
058400091222RIFIUTA SENZA SPECIFICARE IL MOTIVO
058500091222MERCE NON ORDINATA O NON CONFORME
058600091222MERCE SPEDITA IN RITARDO
058700091222MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
058800091222MERCE SPEDITA CON TROPPO ANTICIPO
058900091222MERCE RESA SENZA AUTORIZZAZIONE
059000091222DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
059100091222DESTINATARIO NON PAGA IL C/ASSEGNO
059200091222CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
059300091222DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
059400091222IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
059500091222DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
059600091222DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
059700091222DESTINATARIO RIMANDA LO SVINCOLO AL
059800091222DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
059900091222DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
060000091222DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
060100091222DESTINATARIO RISULTA TRASFERITO
060200091222DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
060300091222INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
060400091222IL DESTINATARIO E' CHIUSO FINO AL
060500091222IL DESTINATARIO E' CHIUSO PER FERIE
060600091222IL DESTINATARIO E' CHIUSO
060700091222IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
060800091222DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
060900091222ESERCIZIO NON ANCORA IN ATTIVITA
061000091222IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
061100091222IL DESTINATARIO RIFIUTA LA MERCE
061200091222IL DESTINATARIO NON INTENDE
061300091222FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
061400091222IL DESTINATARIO RICHIEDE
061500091222SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
061600091222DOCUMENTI INCOMPLETI O MANCANTI
061700091222IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
061800091222RIFIUTA LA CONSEGNA TASSATIVA
061900091222-
062000091222-
062100091222-
062200091222CONSEGNA OK
062300091222RESO
062400100510-
062500100510ACCETTAZIONE SPEDIZIONE (TRANSITO)
062600100510ACCETTAZIONE SPEDIZIONE (TRANSITO)
062700091222** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
062800100215A                        AMMANCO
062900100215M                        MANCA COLLO
063000100215V                        AVARIA
063100100215E                        EVENTO
063200091222** WDCTCLI - CAUSALI CLIENTE
063300100317NNL                      AMMANCO
063400100317NNL                      MANCA COLLO
063500100317NMG                      AVARIA
063600100215NON INVIARE              EVENTO
063700100423** WDCTDES - DESCRIZIONE TIPI ANOMALIE C/A BARTOLINI
063800100423AMMANCO
063900100423MANCA COLLO
064000100423AVARIA
064100100423EVENTO
