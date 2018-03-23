000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400091222     FTNTBE01L  IF   E           K DISK
000500091223     FTABEL00F  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700090323     FTISTB02L  UF   E           K DISK    commit
000800090323     FTIVAWWWT  UF A E             DISK    commit
000900091222     FTNCSB03L  IF   E           K DISK
001000091222     FTIGCP51L  IF   E           K DISK
001100031103
001200031103     D*-----------
001300031103     D* SCHIERE A PROGRAMMA
001400031103     D*-----------
001500091222     D CAUBAR          S             10    DIM(90) CTDATA PERRCD(1)
001600091222     D CAUCLI          S             11    DIM(90) CTDATA PERRCD(1)
001700091222     D DESCLI          S             80    DIM(90) CTDATA PERRCD(1)
001800091222     D WDCTBAR         S              1    DIM(4)  CTDATA PERRCD(1)
001900091222     D WDCTCLI         S             10    DIM(4)  CTDATA PERRCD(1)
002000041015
002100041015     D*-----------
002200041015     D* SCHIERE
002300041015     D*-----------
002400091223     D skTADCod        S              2    DIM(100)
002500091223     D skTADRag        S              1    DIM(100)
002600091222     D jTAD            S              3  0 INZ(*zeros)
002700091223     D sk2Alavv        s             10    DIM(300)
002800091223     D sk2Agiac        s             10    DIM(300)
002900030924
003000041015     D*-----------
003100041015     D* VARIABILI D WRK
003200041015     D*-----------
003300031103     D prmppi          S             50
003400030924     D prmesito        S              1
003500030924     D wrkesito        S                   like(prmesito)
003600091223     D j               S              3  0 inz
003700091223     D i               s              3  0 inz
003800091223     D ii              s              3  0 inz
003900091223     D wTADCod         S              2    inz
004000091222     D wCAS            S              9s 2 inz
004100091222
004200091222     D*-----------
004300091222     D* DEFINIZIONE DS ESTERNE
004400091222     D*-----------
004500091222     D DTAD          E DS
004600091223     D DS2A          E DS
004700031103
004800031103     D*-----------
004900031103     D* VARIABILI RIFERITE AL DATA-BASE
005000031103     D*-----------
005100080618     D wKstbKSU        S                   like(stbKSU)
005200080618     D wKstbFTR        S                   like(stbFTR)
005300030924
005400030924     D*-----------
005500030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
005600030924     D*-----------
005700030925     D C_DS_Cli        DS
005800091222     D  ds_b_tracer                  20a   inz
005900091222     D  ds_b_lotto                   12a   inz
006000091222     D  ds_b_datoraev                14a   inz
006100091222     D  ds_b_codmitt                 12a   inz('000000200003')
006200091222     D  ds_b_proven                  12a   inz('000000200003')
006300091222     D  ds_b_destinaz                12a   inz('000000200003')
006400091222     D  ds_b_articolo                20a   inz
006500091222     D  ds_b_destinat                40a   inz
006600091222     D  ds_b_indirizz                30a   inz
006700091222     D  ds_b_cap                      5a   inz
006800091222     D  ds_b_localita                25a   inz
006900091222     D  ds_b_prov                     2a   inz
007000091222     D  ds_b_telefono                12a   inz
007100091222     D  ds_b_divisa                   1a   inz('E')
007200091222     D  ds_b_contrass                 9a   inz
007300091222     D  ds_b_codcaus                  3a   inz
007400091222     D  ds_b_tiprec                   1a   inz
007500091222     D  ds_b_stscons                  3a   inz
007600091222     D  ds_b_codpenal                 3a   inz
007700091222     D  ds_b_codvers                 12a   inz
007800091222     D  ds_b_filler01                10a   inz
007900091222     D  ds_b_codcorr                  2a   inz('26')
008000091222     D  ds_b_datacons                 8a   inz
008100091222     D  ds_b_codgiac                  3a   inz
008200091222
008300091222     D ds_Key_bolla    ds
008400091222     D  wstbAAS                       4a
008500091222     D  wstbLNP                       3a
008600091222     D  wstbNRS                       2a
008700091222     D  wstbNSP                       7a
008800100121
008900100121     D ds_data         ds
009000100121     D  wAAAA                         4a
009100100121     D  wMM                           2a
009200100121     D  wGG                           2a
009300100121
009400100121     D ds_ora          ds
009500100121     D  wHHMM                         4a
009600100121     D  wSS                           2a
009700031103
009800031103     D*-----------
009900031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
010000031103     D*-----------
010100031103     D DSPPI           DS
010200080618     D  w§PPIKSU                      7  0
010300030924
010400030325
010500030924     C* MAIN/
010600091222     C*
010700091222     C* Carica i dati tabellati
010800091222     C                   EXSR      cartab
010900041015     C*
011000041015     C* Esegue traduzione
011100991027     C                   EXSR      traduci
011200921023     C                   SETON                                        LR
011300091222
011400091222
011500091222
011600091222     C     cartab        BEGSR
011700091223     C*
011800091223     C* Carico causali evento divise x categoria (A=Lavv G=Giac)
011900091223     C                   clear                   sk2Alavv
012000091223     C                   clear                   sk2Agiac
012100091223     C                   z-add     *zeros        i
012200091223     C                   z-add     *zeros        ii
012300091223     C                   eval      tblKUT = 1
012400091223     C                   eval      tblCOD = '2A'
012500091223     C     KEYtab00P     setll     tabel00f
012600091223     C     KEYtab00P     reade     tabel00f
012700091223     C                   dow       not %eof(tabel00f)
012800091223     C                   movel(p)  tblUNI        ds2a
012900091223     C                   if        §2aFTC = 'G'
013000091223     C                   add       1             i
013100091223     C                   eval      sk2Agiac(i) = tblKEY
013200091223     C                   endif
013300091223     C                   if        §2aFTC = 'A'
013400091223     C                   add       1             ii
013500091223     C                   eval      sk2Alavv(ii) = tblKEY
013600091223     C                   endif
013700091223     C     KEYtab00P     reade     tabel00f
013800091223     C                   enddo
013900091222     C*
014000091222     C* Caricamento tipi anomalia C/A
014100091222     C     'TAD'         SETLL     tntbe01l
014200091222     C                   IF        %equal(tntbe01l)
014300091222     C     'TAD'         READE     tntbe01l
014400091222     C                   DOW       not %eof(tntbe01l)
014500091222     C                   EVAL      DTAD = tbeUNI
014600091222     C                   ADD       1             jTAD
014700091222     C                   EVAL      skTADCod(jTAD) = tbeKE1
014800091222     C                   EVAL      skTADRag(jTAD) = §TADRAGR
014900091222     C     'TAD'         READE     tntbe01l
015000091222     C                   ENDDO
015100091222     C                   ENDIF
015200091222     C*
015300091222     C                   ENDSR
015400991027
015500030924
015600030924
015700991027     C     traduci       BEGSR
015800031103     C*
015900031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
016000031103     C* di traduzione (trattasi d "post-traduzione")
016100031104     C                   EVAL      wKstbKSU = w§PPIKSU
016200031103     C                   EVAL      wKstbFTR = *blanks
016300031103     C     KEYstb02P     SETLL     tistb02l
016400031103     C                   IF        %equal(tistb02l)
016500031103     C     KEYstb02P     READE     tistb02l
016600991027     C*
016700031103     C* Ciclo x tuttii record del cliente da trasmettere
016800031103     C                   DOW       not %eof(tistb02l)
016900031014     C*
017000030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
017100031014     C                   RESET                   C_DS_Cli
017200080618     C*
017300080618     C                   SETON                                        50
017400091223     C                   SETOFF                                       54
017500031103     C*
017600031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
017700031103     C* deve restituire al cliente nel file tradotto
017800031103     C                   IF        stbFTR = *blanks                             * ridondante
017900100209     C*
018000100209     C* Filtro x le spedizioni aventi RMA lungo almeno 10 byte e che inizi con '5' o '7'
018100171017     C***                IF        %len(%trim(stbRMA)) >= 10 AND
018200171017     C***                          (%subst(%trim(stbRMA):1:1) = '5' OR
018300171017     C***                           %subst(%trim(stbRMA):1:1) = '7')
018400031103     C*
018500031103     C* Innanzitutto chaino la bolla direttamente su TITAS
018600031103     C     KEYtas30P     CHAIN     titas30c
018700031103     C                   IF        %found(titas30c)
018800091223     C*
018900091223     C* Gestisco gli stati non-standard
019000091223     C                   IF        stbTIS = '9'
019100091223     C*
019200091223     C                   SETOFF                                       50
019300091223     C*
019400091223     C                   IF        %subst(stbCOS:1:8) = 'INCARICO'
019500091223     C                   EVAL      ds_b_codcaus = '011'
019600091223     C                   ELSE
019700091223     C                   SETON                                        50
019800091223     C                   ENDIF
019900091223     C                   ENDIF
020000091222     C*
020100091223     C* Gestisco gli stati standard
020200091223     C                   IF        stbTIS = '1'
020300091222     C*
020400091222     C                   SETOFF                                       50
020500091222     C*
020600091222     C* Aggancio quindi il contrassegno relativo alla bolla originale
020700091222     C     KEYcsb03C     chain     tncsb03l
020800091222     C                   if        %found(tncsb03l)
020900091222     C                   z-add     csbCAS        wCAS
021000091222     C                   else
021100091222     C                   eval      wCAS = *zeros
021200091222     C                   endif
021300080618     C*
021400080618     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
021500031103     C                   Z-ADD     1             j
021600031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
021700080618     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
021800091222     C                   EVAL      ds_b_codcaus = CAUCLI(j)
021900091223     C                   z-add     1             i
022000091223     C     stbCOS        lookup    sk2Agiac(i)                            55
022100091223     C                   if        %found
022200091223     C                   SETON                                        54
022300091223     C                   endif
022400031103     C                   ELSE
022500031103     C                   SETON                                        50
022600031103     C                   ENDIF
022700080618     C                   ENDIF
022800041015     C*
022900091222     C* X stati provenienti dai danni, se trattasi d tipi anomalia A/M/V/E
023000080618     C                   IF        stbPRS =  '5'
023100080618     C*
023200080618     C                   SETOFF                                       50
023300080618     C*
023400080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
023500080618     C                   Z-ADD     1             jTAD
023600080618     C                   MOVEL(P)  stbCOS        wTADCod
023700080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
023800080618     C                   IF        %found
023900080618     C                   Z-ADD     1             j
024000080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
024100080618     C                   IF        %found
024200091222     C                   EVAL      ds_b_codcaus = WDCTCLI(j)
024300080618     C                   ELSE
024400080618     C                   SETON                                        50
024500080618     C                   ENDIF
024600080618     C                   ELSE
024700080618     C                   SETON                                        50
024800080618     C                   ENDIF
024900080618     C                   ENDIF
025000080618     C*
025100080618     C* Scarico il buffer d output
025200080618     C  N50              EXSR      WRIREC
025300100209     C*
025400100209     C                   ENDIF
025500171017     C***                ENDIF
025600100211     C                   ENDIF
025700031103     C*
025800031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
025900031103     C                   EVAL      stbFTR = 'T'
026000031103     C                   UPDATE    tistb000
026100991027     C*
026200031103     C     KEYstb02P     READE     tistb02l
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
027300091222     C     wrirec        BEGSR
027400091222     C*
027500091222     C                   movel     stbAAS        wstbAAS
027600091222     C                   movel     stbLNP        wstbLNP
027700091222     C                   movel     stbNRS        wstbNRS
027800091222     C                   movel     stbNSP        wstbNSP
027900091222     C*
028000100208     C***                eval      ds_b_tracer   = %char(stbRMN)
028100100208     C                   eval      ds_b_tracer   = stbRMA
028200100121     C                   eval      ds_data = *zeros
028300100121     C                   eval      ds_ora  = *zeros
028400100121     C                   movel     stbDAS        ds_data
028500100121     C                   movel     stbORS        wHHMM
028600100121     C                   eval      ds_b_datoraev = wGG   +
028700100121     C                                             wMM   +
028800100121     C                                             wAAAA +
028900100121     C                                             ds_ora
029000091222     C                   eval      ds_b_articolo = ds_key_bolla
029100091222     C                   eval      ds_b_destinat = tasRSD
029200091222     C                   eval      ds_b_indirizz = tasIND
029300091222     C                   eval      ds_b_cap      = tasCAD
029400091222     C                   eval      ds_b_localita = tasLOD
029500091222     C                   eval      ds_b_prov     = tasPRD
029600091222     C                   movel(p)  wCAS          ds_b_contrass
029700091223     C*
029800091223     C* Se causale d tipo giacenza => reperisco
029900091223     C                   if        *in54 = *on
030000091222     C*
030100091222     C* Costruisco la chiave sul file TIGCP51L
030200091222     C                   eval      gcpFRG = *zeros
030300091222     C*
030400091222     C* Reperisco le giacenze relative alla bolla corrente
030500091222     C     KEYgcp51C     chain     tigcp51l
030600091222     C                   if        %found(tigcp51l)
030700091222     C* ...giacenza in attesa d disposizioni
030800100713     C* => richiesta d Teagno in data 2010/07/13 => by-pass causale 902
030900100713     C*                  if        gcpDUR  > *zeros AND
031000100713     C*                            gcpDDM  = *zeros AND
031100100713     C*                            gcpFAS  < 030
031200100713     C*                  eval      ds_b_codcaus = '902'
031300100713     C*                  eval      ds_b_codgiac = '060'
031400100713     C*                  endif
031500091222     C* ...giacenza con disposizioni
031600091222     C                   if        gcpDUR  > *zeros AND
031700091222     C                             gcpDDM  > *zeros AND
031800091222     C                             gcpFAS >= 030
031900091222     C                   eval      ds_b_codgiac = '060'
032000091222     C                   endif
032100091222     C                   endif
032200091223     C                   endif
032300091222     C*
032400091222     C* Considerazioni "finali"
032500091222     C                   if        ds_b_codcaus = '900'
032600100121     C                   movel(p)  stbDAS        ds_data
032700100121     C                   eval      ds_b_datacons = wGG   +
032800100121     C                                             wMM   +
032900100121     C                                             wAAAA
033000091222     C                   endif
033100091222     C*
033200091222     C                   clear                   tivaw000
033300031103     C*
033400031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
033500091222     C                   eval      VAWDTA = C_DS_Cli
033600091222     C                   write     TIVAW000
033700031103     C*
033800031103     C                   ENDSR
033900031103
034000031103
034100991027
034200991027      /TITLE Operazioni iniziali.
034300991027     C     *inzsr        BEGSR
034400991027     C*
034500991027     C     *ENTRY        PLIST
034600031103     C                   parm                    prmppi
034700991027     C     wrkesito      parm      wrkesito      prmesito
034800031103     C*
034900031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
035000031103     C                   EVAL      DSPPI = prmppi
035100030709     C*
035200030709     C* Definizione chiavi
035300030924     C*
035400031103     C* Chiave su TISTB02L - Parziale
035500031103     C     KEYstb02P     KLIST
035600031104     C                   KFLD                    wKstbKSU
035700031103     C                   KFLD                    wKstbFTR
035800031103     C*
035900031103     C* Chiave su TITAS30C - Parziale
036000031103     C     KEYtas30P     KLIST
036100031103     C                   KFLD                    stbAAS
036200031103     C                   KFLD                    stbLNP
036300031103     C                   KFLD                    stbNRS
036400031103     C                   KFLD                    stbNSP
036500091222     C*
036600091222     C* Chiave su TNCSB03L - Completa
036700091222     C     KEYcsb03C     KLIST
036800091222     C                   KFLD                    stbAAS
036900091222     C                   KFLD                    stbLNP
037000091222     C                   KFLD                    stbNRS
037100091222     C                   KFLD                    stbNSP
037200091222     C*
037300091222     C* Chiave su TIGCP51L - Completa
037400091222     C     KEYgcp51C     KLIST
037500091222     C                   KFLD                    stbAAS
037600091222     C                   KFLD                    stbLNP
037700091222     C                   KFLD                    stbNRS
037800091222     C                   KFLD                    stbNSP
037900091222     C                   KFLD                    gcpFRG
038000091223     C*
038100091223     C* Chiave su TABEL00F - Parziale
038200091223     C     KEYtab00P     KLIST
038300091223     C                   KFLD                    tblKUT
038400091223     C                   KFLD                    tblCOD
038500991027     C*
038600991027     C                   ENDSR
038700091222** CAUBAR - CAUSALI BARTOLINI
038800091222A                       L.AVVISO
038900091222ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
039000091222AVV                     LASCIATO AVVISO 1GG
039100091222AV2                     LASCIATO AVVISO 2GG
039200091222AV3                     LASCIATO AVVISO 3GG
039300091222AV5                     LASCIATO AVVISO 5GG
039400091222AV7                     LASCIATO AVVISO 7GG
039500091222AV9                     LASCIATO AVVISO 9 GIORNI
039600091222A16                     L.A. RIMANDA LO SVINCOLO
039700091222A23                     L.A. CHIUSO FINO AL
039800091222CPO                     CAMBIO PORTO
039900091222DAN                     COLLO/I DANNEGGIATO/I
040000091222DIR                     DIROTTAMENTO
040100091222F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
040200091222G                       GIACENZA GENERICA TEMPORANEA
040300091222GEN                     GIACENZA GENERICA
040400091222G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
040500091222G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
040600091222G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
040700091222G09                     ATTENDERE 9 GIORNI
040800091222IDD                     CHIUSURA SPEDIZIONE CON IDD
040900091222MAN                     COLLO/I MANCANTE/I
041000091222MIC                     MESSA IN CONSEGNA
041100091222N                       NON FATTA
041200091222NAV                     COLLO SCONDIZIONATO
041300091222NIC                     TOLTA DALLA CONSEGNA
041400091222P                       CONSEGNA PARZIALE
041500091222PAT                     FESTIVITA PATRONALE
041600091222R                       RISERVA
041700091222RDC                     RIPRISTINO
041800091222RFD                     RIFIUTATO
041900091222RFM                     RIFIUTATO
042000091222RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
042100091222RIP                     RIPRISTINO
042200091222RS                      RISERVA
042300091222T                       CHIUSO PER TURNO
042400091222WA                      AMMANCO SENZA EVENTO
042500091222WAE                     AMMANCO CON EVENTO
042600091222WM                      MANCANZA SENZA EVENTO
042700091222WME                     MANCANZA CON EVENTO
042800091222WV                      AVARIA SENZA EVENTO
042900091222WVE                     AVARIA CON EVENTO
043000091222ZAL                     ALLUVIONE
043100091222ZBS                     BLOCCO STRADALE
043200091222ZFR                     FESTIVITÀ REGIONALE
043300091222ZMM                     MARE MOLTO MOSSO
043400091222ZMP                     MANIFESTAZIONE PUBBLICA
043500091222ZNV                     NEVICATA ECCEZIONALE
043600091222ZSC                     SCIOPERO
043700091222001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
043800091222002                     MERCE NON ORDINATA O NON CONFORME
043900091222003                     MERCE SPEDITA IN RITARDO
044000091222004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
044100091222005                     MERCE SPEDITA CON TROPPO ANTICIPO
044200091222006                     MERCE RESA SENZA AUTORIZZAZIONE
044300091222007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
044400091222008                     DESTINATARIO NON PAGA IL C/ASSEGNO
044500091222009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
044600091222010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
044700091222012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
044800091222013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
044900091222014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
045000091222016                     DESTINATARIO RIMANDA LO SVINCOLO AL
045100091222017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
045200091222018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
045300091222019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
045400091222020                     DESTINATARIO RISULTA TRASFERITO
045500091222021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
045600091222022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
045700091222023                     IL DESTINATARIO E' CHIUSO FINO AL
045800091222024                     IL DESTINATARIO E' CHIUSO PER FERIE
045900091222025                     IL DESTINATARIO E' CHIUSO
046000091222026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
046100091222027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
046200091222028                     ESERCIZIO NON ANCORA IN ATTIVITA
046300091222029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
046400091222030                     IL DESTINATARIO RIFIUTA LA MERCE
046500091222031                     IL DESTINATARIO NON INTENDE
046600091222032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
046700091222033                     IL DESTINATARIO RICHIEDE
046800110506060                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
046900091222035                     DOCUMENTI INCOMPLETI O MANCANTI
047000091222036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
047100091222037                     RIFIUTA LA CONSEGNA TASSATIVA
047200091222100                     -
047300091222101                     -
047400091222999                     -
047500091222CONSOK                  CONSEGNA OK
047600091222RESO                    RESO
047700091222
047800091222** CAUCLI - CAUSALI CLIENTE
047900091222030                     L.AVVISO
048000091222030                     AVVISATO-RIMANDA SVINCOLO 1GG
048100091222030                     LASCIATO AVVISO 1GG
048200091222031                     LASCIATO AVVISO 2GG
048300091222031                     LASCIATO AVVISO 3GG
048400091222031                     LASCIATO AVVISO 5GG
048500091222031                     LASCIATO AVVISO 7GG
048600091222031                     LASCIATO AVVISO 9 GIORNI
048700091222030                     L.A. RIMANDA LO SVINCOLO
048800091222030                     L.A. CHIUSO FINO AL
048900091222NON INVIARE             CAMBIO PORTO
049000091222071                     COLLO/I DANNEGGIATO/I
049100091222NON INVIARE             DIROTTAMENTO
049200091222NON INVIARE             FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
049300091222060                     GIACENZA GENERICA TEMPORANEA
049400091222060                     GIACENZA GENERICA
049500091222NON INVIARE             ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
049600091222NON INVIARE             ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
049700091222NON INVIARE             ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
049800091222NON INVIARE             ATTENDERE 9 GIORNI
049900091222900                     CHIUSURA SPEDIZIONE CON IDD
050000091222083                     COLLO/I MANCANTE/I
050100091222899                     MESSA IN CONSEGNA
050200091222039                     NON FATTA
050300091222071                     COLLO SCONDIZIONATO
050400091222039                     TOLTA DALLA CONSEGNA
050500091223900                     CONSEGNA PARZIALE
050600091223060                     FESTIVITA PATRONALE
050700091222900                     RISERVA
050800091223899                     RIPRISTINO
050900091222036                     RIFIUTATO
051000091222036                     RIFIUTATO
051100091222899                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
051200091223899                     RIPRISTINO
051300091222900                     RISERVA
051400091223060                     CHIUSO PER TURNO
051500091222071                     AMMANCO SENZA EVENTO
051600091222071                     AMMANCO CON EVENTO
051700091222071                     MANCANZA SENZA EVENTO
051800091222071                     MANCANZA CON EVENTO
051900091222071                     AVARIA SENZA EVENTO
052000091222071                     AVARIA CON EVENTO
052100091223060                     ALLUVIONE
052200091223060                     BLOCCO STRADALE
052300091223060                     FESTIVITÀ REGIONALE
052400091223060                     MARE MOLTO MOSSO
052500091223060                     MANIFESTAZIONE PUBBLICA
052600091223060                     NEVICATA ECCEZIONALE
052700091223060                     SCIOPERO
052800091222036                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
052900091222046                     MERCE NON ORDINATA O NON CONFORME
053000091222036                     MERCE SPEDITA IN RITARDO
053100091222036                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
053200091222036                     MERCE SPEDITA CON TROPPO ANTICIPO
053300091222036                     MERCE RESA SENZA AUTORIZZAZIONE
053400091222046                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
053500091222038                     DESTINATARIO NON PAGA IL C/ASSEGNO
053600091222036                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
053700091222038                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
053800091222038                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
053900091222038                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
054000091222038                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
054100091222036                     DESTINATARIO RIMANDA LO SVINCOLO AL
054200091222030                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
054300091222036                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
054400091222033                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
054500091222033                     DESTINATARIO RISULTA TRASFERITO
054600091222034                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
054700091222032                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
054800091223060                     IL DESTINATARIO E' CHIUSO FINO AL
054900091223060                     IL DESTINATARIO E' CHIUSO PER FERIE
055000091223060                     IL DESTINATARIO E' CHIUSO
055100091222036                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
055200091222046                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
055300091222034                     ESERCIZIO NON ANCORA IN ATTIVITA
055400091222036                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
055500091222036                     IL DESTINATARIO RIFIUTA LA MERCE
055600091222036                     IL DESTINATARIO NON INTENDE
055700091222036                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
055800091222036                     IL DESTINATARIO RICHIEDE
055900091223060                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
056000091223060                     DOCUMENTI INCOMPLETI O MANCANTI
056100091223900                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
056200091222036                     RIFIUTA LA CONSEGNA TASSATIVA
056300091222NON INVIARE             -
056400091222NON INVIARE             -
056500091222NON INVIARE             -
056600091222900                     CONSEGNA OK
056700091222910                     RESO
056800080618
056900091222** DESCLI - DESCRIZIONI CAUSALI CLIENTE
057000091222L.AVVISO
057100091222AVVISATO-RIMANDA SVINCOLO 1GG
057200091222LASCIATO AVVISO 1GG
057300091222LASCIATO AVVISO 2GG
057400091222LASCIATO AVVISO 3GG
057500091222LASCIATO AVVISO 5GG
057600091222LASCIATO AVVISO 7GG
057700091222LASCIATO AVVISO 9 GIORNI
057800091222L.A. RIMANDA LO SVINCOLO
057900091222L.A. CHIUSO FINO AL
058000091222CAMBIO PORTO
058100091222COLLO/I DANNEGGIATO/I
058200091222DIROTTAMENTO
058300091222FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
058400091222GIACENZA GENERICA TEMPORANEA
058500091222GIACENZA GENERICA
058600091222ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
058700091222ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
058800091222ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
058900091222ATTENDERE 9 GIORNI
059000091222CHIUSURA SPEDIZIONE CON IDD
059100091222COLLO/I MANCANTE/I
059200091222MESSA IN CONSEGNA
059300091222NON FATTA
059400091222COLLO SCONDIZIONATO
059500091222TOLTA DALLA CONSEGNA
059600091222CONSEGNA PARZIALE
059700091222FESTIVITA PATRONALE
059800091222RISERVA
059900091222RIPRISTINO
060000091222RIFIUTATO
060100091222RIFIUTATO
060200091222RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
060300091222RIPRISTINO
060400091222RISERVA
060500091222CHIUSO PER TURNO
060600091222AMMANCO SENZA EVENTO
060700091222AMMANCO CON EVENTO
060800091222MANCANZA SENZA EVENTO
060900091222MANCANZA CON EVENTO
061000091222AVARIA SENZA EVENTO
061100091222AVARIA CON EVENTO
061200091222ALLUVIONE
061300091222BLOCCO STRADALE
061400091222FESTIVITÀ REGIONALE
061500091222MARE MOLTO MOSSO
061600091222MANIFESTAZIONE PUBBLICA
061700091222NEVICATA ECCEZIONALE
061800091222SCIOPERO
061900091222RIFIUTA SENZA SPECIFICARE IL MOTIVO
062000091222MERCE NON ORDINATA O NON CONFORME
062100091222MERCE SPEDITA IN RITARDO
062200091222MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
062300091222MERCE SPEDITA CON TROPPO ANTICIPO
062400091222MERCE RESA SENZA AUTORIZZAZIONE
062500091222DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
062600091222DESTINATARIO NON PAGA IL C/ASSEGNO
062700091222CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
062800091222DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
062900091222IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
063000091222DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
063100091222DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
063200091222DESTINATARIO RIMANDA LO SVINCOLO AL
063300091222DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
063400091222DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
063500091222DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
063600091222DESTINATARIO RISULTA TRASFERITO
063700091222DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
063800091222INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
063900091222IL DESTINATARIO E' CHIUSO FINO AL
064000091222IL DESTINATARIO E' CHIUSO PER FERIE
064100091222IL DESTINATARIO E' CHIUSO
064200091222IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
064300091222DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
064400091222ESERCIZIO NON ANCORA IN ATTIVITA
064500091222IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
064600091222IL DESTINATARIO RIFIUTA LA MERCE
064700091222IL DESTINATARIO NON INTENDE
064800091222FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
064900091222IL DESTINATARIO RICHIEDE
065000091222SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
065100091222DOCUMENTI INCOMPLETI O MANCANTI
065200091222IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
065300091222RIFIUTA LA CONSEGNA TASSATIVA
065400091222-
065500091222-
065600091222-
065700091222CONSEGNA OK
065800091222RESO
065900080618
066000091222** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
066100091222A
066200091222M
066300091222V
066400091222E
066500091222** WDCTCLI - CAUSALI CLIENTE
066600091222071
066700091222071
066800091222071
066900091222071
