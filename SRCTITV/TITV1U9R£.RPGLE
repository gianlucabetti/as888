000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100215     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300991027
000400091222     FTNTBE01L  IF   E           K DISK
000500091223     FTABEL00F  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700090323     FTISTB02L  UF   E           K DISK    commit
000800090323     FTIVAWWWT  UF A E             DISK    commit
000900091222     FTIGCP51L  IF   E           K DISK
001000100215     FFNBLT01L  IF   E           K DISK    usropn
001100100215     F                                     extfile(LibFileBLT)
001200031103
001300031103     D*-----------
001400031103     D* SCHIERE A PROGRAMMA
001500031103     D*-----------
001600091222     D CAUBAR          S             10    DIM(90) CTDATA PERRCD(1)
001700091222     D CAUCLI          S             11    DIM(90) CTDATA PERRCD(1)
001800091222     D DESCLI          S             80    DIM(90) CTDATA PERRCD(1)
001900091222     D WDCTBAR         S              1    DIM(4)  CTDATA PERRCD(1)
002000091222     D WDCTCLI         S             10    DIM(4)  CTDATA PERRCD(1)
002100041015
002200041015     D*-----------
002300041015     D* SCHIERE
002400041015     D*-----------
002500091223     D skTADCod        S              2    DIM(100)
002600091223     D skTADRag        S              1    DIM(100)
002700091222     D jTAD            S              3  0 INZ(*zeros)
002800091223     D sk2Alavv        s             10    DIM(300)
002900091223     D sk2Agiac        s             10    DIM(300)
003000030924
003100041015     D*-----------
003200041015     D* VARIABILI D WRK
003300041015     D*-----------
003400031103     D prmppi          S             50
003500030924     D prmesito        S              1
003600030924     D wrkesito        S                   like(prmesito)
003700091223     D j               S              3  0 inz
003800091223     D i               s              3  0 inz
003900091223     D ii              s              3  0 inz
004000091223     D wTADCod         S              2    inz
004100091222     D wCAS            S              9s 2 inz
004200100215
004300100215     D LibFileBLT      s             21A   inz
004400100215     D currSysNeta     s              8A   inz('*NULL')
004500091222
004600091222     D*-----------
004700091222     D* DEFINIZIONE DS ESTERNE
004800091222     D*-----------
004900091222     D DTAD          E DS
005000091223     D DS2A          E DS
005100031103
005200031103     D*-----------
005300031103     D* VARIABILI RIFERITE AL DATA-BASE
005400031103     D*-----------
005500080618     D wKstbKSU        S                   like(stbKSU)
005600080618     D wKstbFTR        S                   like(stbFTR)
005700030924
005800030924     D*-----------
005900030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
006000030924     D*-----------
006100030925     D C_DS_Cli        DS
006200100215     D  ds_b_ldv                     15a   inz
006300100215     D  ds_b_datoraev                14a   inz
006400100215     D  ds_b_codcau                   5a   inz
006500100215     D  ds_b_nclcons                  5a   inz(*zeros)
006600100215     D  ds_b_ddt                     50a   inz
006700100215     D  ds_b_nclncons                 5a   inz(*zeros)
006800100215     D  ds_b_codfil                  10a   inz
006900100215     D  ds_b_firma                   50a   inz
007000091222
007100091222     D ds_Key_bolla    ds
007200091222     D  wstbAAS                       4a
007300091222     D  wstbLNP                       3a
007400091222     D  wstbNRS                       2a
007500091222     D  wstbNSP                       7a
007600100121
007700100215     D ds_data         ds
007800100121     D  wAAAA                         4a
007900100121     D  wMM                           2a
008000100121     D  wGG                           2a
008100100121
008200100215     D ds_ora          ds
008300100121     D  wHHMM                         4a
008400100121     D  wSS                           2a
008500031103
008600031103     D*-----------
008700031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
008800031103     D*-----------
008900031103     D DSPPI           DS
009000080618     D  w§PPIKSU                      7  0
009100030924
009200100215
009300100215     D*------------------
009400100215     D* LINKING A DEFINIZIONI ESTERNE
009500100215     D*------------------
009600100215     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
009700100215     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
009800100215
009900030325
010000030924     C* MAIN/
010100100215     C*
010200100215     C* Reperisco il sistema AS/400 corrente
010300100215     C                   callp     UBRTVNETA_Rtv(currSysNeta)
010400100215     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
010500100215     C                   eval      LibFileBLT = 'FILTRA201/FNBLT01L'
010600100215     C                   else
010700100215     C                   eval      LibFileBLT = 'FILTRAPRD/FNBLT01L'
010800100215     C                   endif
010900100215     C*
011000100215     C                   if        not %open(FNBLT01L)
011100100215     C                   open      FNBLT01L
011200100215     C                   endif
011300100215     C*
011400091222     C*
011500091222     C* Carica i dati tabellati
011600091222     C                   EXSR      cartab
011700041015     C*
011800041015     C* Esegue traduzione
011900991027     C                   EXSR      traduci
012000921023     C                   SETON                                        LR
012100091222
012200091222
012300091222
012400091222     C     cartab        BEGSR
012500091223     C*
012600091223     C* Carico causali evento divise x categoria (A=Lavv G=Giac)
012700091223     C                   clear                   sk2Alavv
012800091223     C                   clear                   sk2Agiac
012900091223     C                   z-add     *zeros        i
013000091223     C                   z-add     *zeros        ii
013100091223     C                   eval      tblKUT = 1
013200091223     C                   eval      tblCOD = '2A'
013300091223     C     KEYtab00P     setll     tabel00f
013400091223     C     KEYtab00P     reade     tabel00f
013500091223     C                   dow       not %eof(tabel00f)
013600091223     C                   movel(p)  tblUNI        ds2a
013700091223     C                   if        §2aFTC = 'G'
013800091223     C                   add       1             i
013900091223     C                   eval      sk2Agiac(i) = tblKEY
014000091223     C                   endif
014100091223     C                   if        §2aFTC = 'A'
014200091223     C                   add       1             ii
014300091223     C                   eval      sk2Alavv(ii) = tblKEY
014400091223     C                   endif
014500091223     C     KEYtab00P     reade     tabel00f
014600091223     C                   enddo
014700091222     C*
014800091222     C* Caricamento tipi anomalia C/A
014900091222     C     'TAD'         SETLL     tntbe01l
015000091222     C                   IF        %equal(tntbe01l)
015100091222     C     'TAD'         READE     tntbe01l
015200091222     C                   DOW       not %eof(tntbe01l)
015300091222     C                   EVAL      DTAD = tbeUNI
015400091222     C                   ADD       1             jTAD
015500091222     C                   EVAL      skTADCod(jTAD) = tbeKE1
015600091222     C                   EVAL      skTADRag(jTAD) = §TADRAGR
015700091222     C     'TAD'         READE     tntbe01l
015800091222     C                   ENDDO
015900091222     C                   ENDIF
016000091222     C*
016100091222     C                   ENDSR
016200991027
016300030924
016400030924
016500991027     C     traduci       BEGSR
016600031103     C*
016700031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
016800031103     C* di traduzione (trattasi d "post-traduzione")
016900031104     C                   EVAL      wKstbKSU = w§PPIKSU
017000031103     C                   EVAL      wKstbFTR = *blanks
017100031103     C     KEYstb02P     SETLL     tistb02l
017200031103     C                   IF        %equal(tistb02l)
017300031103     C     KEYstb02P     READE     tistb02l
017400991027     C*
017500031103     C* Ciclo x tuttii record del cliente da trasmettere
017600031103     C                   DOW       not %eof(tistb02l)
017700031014     C*
017800030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
017900031014     C                   RESET                   C_DS_Cli
018000080618     C*
018100080618     C                   SETON                                        50
018200091223     C                   SETOFF                                       54
018300031103     C*
018400031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
018500031103     C* deve restituire al cliente nel file tradotto
018600031103     C                   IF        stbFTR = *blanks                             * ridondante
018700031103     C*
018800031103     C* Innanzitutto chaino la bolla direttamente su TITAS
018900031103     C     KEYtas30P     CHAIN     titas30c
019000031103     C                   IF        %found(titas30c)
019100091222     C*
019200091223     C* Gestisco gli stati standard
019300091223     C                   IF        stbTIS = '1'
019400091222     C*
019500100215     C                   SETOFF                                       50
019600100215     C*
019700100215     C* Inizializzo colli consegnati = NCL e colli no consegnati = 0
019800100215     C                   z-add     *zeros        wColliCons        5 0
019900100215     C                   z-add     *zeros        wColliNoCons      5 0
020000080618     C*
020100080618     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
020200031103     C                   Z-ADD     1             j
020300031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
020400080618     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
020500100215     C                   EVAL      ds_b_codcau = CAUCLI(j)
020600100215     C*
020700100215     C* Se consegna parziale verifico numero colli consegnato e numero colli nn consegnati
020800100215     C                   IF        CAUBAR(j) = 'P'
020900100215     C                   EXSR      rtvBLT
021000100215     C                   ENDIF
021100100215     C                   IF        CAUBAR(j) = 'CONSOK'
021200100215     C                   eval      wColliCons = tasNCL
021300100215     C                   ENDIF
021400100215     C*
021500091223     C                   z-add     1             i
021600091223     C     stbCOS        lookup    sk2Agiac(i)                            55
021700091223     C                   if        %found
021800091223     C                   SETON                                        54
021900091223     C                   endif
022000031103     C                   ELSE
022100031103     C                   SETON                                        50
022200031103     C                   ENDIF
022300080618     C                   ENDIF
022400041015     C*
022500091222     C* X stati provenienti dai danni, se trattasi d tipi anomalia A/M/V/E
022600080618     C                   IF        stbPRS =  '5'
022700080618     C*
022800080618     C                   SETOFF                                       50
022900080618     C*
023000080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
023100080618     C                   Z-ADD     1             jTAD
023200080618     C                   MOVEL(P)  stbCOS        wTADCod
023300080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
023400080618     C                   IF        %found
023500080618     C                   Z-ADD     1             j
023600080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
023700100215     C                   IF        %found AND WDCTBAR(j) <> 'NON INVIARE'
023800100215     C                   EVAL      ds_b_codcau = WDCTCLI(j)
023900080618     C                   ELSE
024000080618     C                   SETON                                        50
024100080618     C                   ENDIF
024200080618     C                   ELSE
024300080618     C                   SETON                                        50
024400080618     C                   ENDIF
024500080618     C                   ENDIF
024600080618     C*
024700080618     C* Scarico il buffer d output
024800080618     C  N50              EXSR      WRIREC
024900100209     C*
025000100209     C                   ENDIF
025100100209     C                   ENDIF
025200031103     C*
025300031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
025400031103     C                   EVAL      stbFTR = 'T'
025500031103     C                   UPDATE    tistb000
025600991027     C*
025700031103     C     KEYstb02P     READE     tistb02l
025800031103     C                   SETOFF                                       50
025900030325     C                   ENDDO
026000031103     C                   ENDIF
026100991027     C*
026200030325     C                   EVAL      wrkesito = '0'
026300991027     C*
026400910830     C                   ENDSR
026500100215
026600031103
026700100215
026800100215     C     rtvBLT        BEGSR
026900100215     C*
027000100215     C     KEYblt01P     setll     fnblt01l
027100100215     C                   if        %equal(fnblt01l)
027200100215     C                   z-add     *zeros        wColliCons
027300100215     C                   z-add     *zeros        wColliNoCons
027400100215     C     KEYblt01P     reade     fnblt01l
027500100215     C                   dow       not %eof(fnblt01l)
027600100215     C                   if        bltDCM > *zeros
027700100215     C                   add       1             wColliCons
027800100215     C                   else
027900100215     C                   add       1             wColliNoCons
028000100215     C                   endif
028100100215     C     KEYblt01P     reade     fnblt01l
028200100215     C                   enddo
028300100215     C                   endif
028400100215     C*
028500100215     C                   ENDSR
028600031103
028700100215
028800031103
028900091222     C     wrirec        BEGSR
029000091222     C*
029100091222     C                   movel     stbAAS        wstbAAS
029200091222     C                   movel     stbLNP        wstbLNP
029300091222     C                   movel     stbNRS        wstbNRS
029400091222     C                   movel     stbNSP        wstbNSP
029500091222     C*
029600100215     C                   eval      ds_b_ldv = ds_key_bolla
029700100215     C                   eval      ds_data  = *zeros
029800100215     C                   eval      ds_ora   = *zeros
029900100121     C                   movel     stbDAS        ds_data
030000100121     C                   movel     stbORS        wHHMM
030100100215     C                   eval      ds_b_datoraev = wAAAA +
030200100215     C                                             wMM   +
030300100215     C                                             wGG   +
030400100121     C                                             ds_ora
030500100217     C                   eval      ds_b_ddt = stbRMA
030600100215     C                   eval      ds_b_nclcons  = %char(wColliCons)
030700100215     C                   eval      ds_b_nclncons = %char(wColliNoCons)
030800091223     C*
030900091223     C* Se causale d tipo giacenza => reperisco
031000091223     C                   if        *in54 = *on
031100091222     C*
031200091222     C* Costruisco la chiave sul file TIGCP51L
031300091222     C                   eval      gcpFRG = *zeros
031400091222     C*
031500091222     C* Reperisco le giacenze relative alla bolla corrente
031600091222     C     KEYgcp51C     chain     tigcp51l
031700091222     C                   if        %found(tigcp51l)
031800091222     C* ...giacenza in attesa d disposizioni
031900091223     C                   if        gcpDUR  > *zeros AND
032000091223     C                             gcpDDM  = *zeros AND
032100091223     C                             gcpFAS  < 030
032200100215     C                   eval      ds_b_codcau = '001'
032300091222     C                   endif
032400091222     C* ...giacenza con disposizioni
032500091222     C                   if        gcpDUR  > *zeros AND
032600091222     C                             gcpDDM  > *zeros AND
032700091222     C                             gcpFAS >= 030
032800100215     C                   eval      ds_b_codcau = '018'
032900091222     C                   endif
033000091222     C                   endif
033100091223     C                   endif
033200091222     C*
033300091222     C                   clear                   tivaw000
033400031103     C*
033500031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
033600100215     C                   eval      VAWDTA =
033700100215     C                               '"'+%trim(ds_b_ldv)     +'"'+','+
033800100215     C                               '"'+%trim(ds_b_datoraev)+'"'+','+
033900100215     C                               '"'+%trim(ds_b_codcau)  +'"'+','+
034000100215     C                               '"'+%trim(ds_b_nclcons) +'"'+','+
034100100215     C                               '"'+%trim(ds_b_ddt)     +'"'+','+
034200100215     C                               '"'+%trim(ds_b_nclncons)+'"'+','+
034300100215     C                               '"'+%trim(ds_b_codfil)  +'"'+','+
034400100215     C                               '"'+%trim(ds_b_firma)   +'"'
034500091222     C                   write     TIVAW000
034600031103     C*
034700031103     C                   ENDSR
034800031103
034900031103
035000991027
035100991027      /TITLE Operazioni iniziali.
035200991027     C     *inzsr        BEGSR
035300991027     C*
035400991027     C     *ENTRY        PLIST
035500031103     C                   parm                    prmppi
035600991027     C     wrkesito      parm      wrkesito      prmesito
035700031103     C*
035800031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
035900031103     C                   EVAL      DSPPI = prmppi
036000030709     C*
036100030709     C* Definizione chiavi
036200030924     C*
036300031103     C* Chiave su TISTB02L - Parziale
036400031103     C     KEYstb02P     KLIST
036500031104     C                   KFLD                    wKstbKSU
036600031103     C                   KFLD                    wKstbFTR
036700031103     C*
036800031103     C* Chiave su TITAS30C - Parziale
036900031103     C     KEYtas30P     KLIST
037000031103     C                   KFLD                    stbAAS
037100031103     C                   KFLD                    stbLNP
037200031103     C                   KFLD                    stbNRS
037300031103     C                   KFLD                    stbNSP
037400100215     C*
037500100215     C* Chiave su FNBLT01L - Parziale
037600100215     C     KEYblt01P     KLIST
037700100215     C                   KFLD                    stbAAS
037800100215     C                   KFLD                    stbLNP
037900100215     C                   KFLD                    stbNRS
038000100215     C                   KFLD                    stbNSP
038100091222     C*
038200091222     C* Chiave su TIGCP51L - Completa
038300091222     C     KEYgcp51C     KLIST
038400091222     C                   KFLD                    stbAAS
038500091222     C                   KFLD                    stbLNP
038600091222     C                   KFLD                    stbNRS
038700091222     C                   KFLD                    stbNSP
038800091222     C                   KFLD                    gcpFRG
038900091223     C*
039000091223     C* Chiave su TABEL00F - Parziale
039100091223     C     KEYtab00P     KLIST
039200091223     C                   KFLD                    tblKUT
039300091223     C                   KFLD                    tblCOD
039400991027     C*
039500991027     C                   ENDSR
039600091222** CAUBAR - CAUSALI BARTOLINI
039700091222A                       L.AVVISO
039800091222ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
039900091222AVV                     LASCIATO AVVISO 1GG
040000091222AV2                     LASCIATO AVVISO 2GG
040100091222AV3                     LASCIATO AVVISO 3GG
040200091222AV5                     LASCIATO AVVISO 5GG
040300091222AV7                     LASCIATO AVVISO 7GG
040400091222AV9                     LASCIATO AVVISO 9 GIORNI
040500091222A16                     L.A. RIMANDA LO SVINCOLO
040600091222A23                     L.A. CHIUSO FINO AL
040700091222CPO                     CAMBIO PORTO
040800091222DAN                     COLLO/I DANNEGGIATO/I
040900091222DIR                     DIROTTAMENTO
041000091222F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
041100091222G                       GIACENZA GENERICA TEMPORANEA
041200091222GEN                     GIACENZA GENERICA
041300091222G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
041400091222G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
041500091222G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
041600091222G09                     ATTENDERE 9 GIORNI
041700091222IDD                     CHIUSURA SPEDIZIONE CON IDD
041800091222MAN                     COLLO/I MANCANTE/I
041900091222MIC                     MESSA IN CONSEGNA
042000091222N                       NON FATTA
042100091222NAV                     COLLO SCONDIZIONATO
042200091222NIC                     TOLTA DALLA CONSEGNA
042300091222P                       CONSEGNA PARZIALE
042400091222PAT                     FESTIVITA PATRONALE
042500091222R                       RISERVA
042600091222RDC                     RIPRISTINO
042700091222RFD                     RIFIUTATO
042800091222RFM                     RIFIUTATO
042900091222RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
043000091222RIP                     RIPRISTINO
043100091222RS                      RISERVA
043200091222T                       CHIUSO PER TURNO
043300091222WA                      AMMANCO SENZA EVENTO
043400091222WAE                     AMMANCO CON EVENTO
043500091222WM                      MANCANZA SENZA EVENTO
043600091222WME                     MANCANZA CON EVENTO
043700091222WV                      AVARIA SENZA EVENTO
043800091222WVE                     AVARIA CON EVENTO
043900091222ZAL                     ALLUVIONE
044000091222ZBS                     BLOCCO STRADALE
044100091222ZFR                     FESTIVITÀ REGIONALE
044200091222ZMM                     MARE MOLTO MOSSO
044300091222ZMP                     MANIFESTAZIONE PUBBLICA
044400091222ZNV                     NEVICATA ECCEZIONALE
044500091222ZSC                     SCIOPERO
044600091222001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
044700091222002                     MERCE NON ORDINATA O NON CONFORME
044800091222003                     MERCE SPEDITA IN RITARDO
044900091222004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
045000091222005                     MERCE SPEDITA CON TROPPO ANTICIPO
045100091222006                     MERCE RESA SENZA AUTORIZZAZIONE
045200091222007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
045300091222008                     DESTINATARIO NON PAGA IL C/ASSEGNO
045400091222009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
045500091222010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
045600091222012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
045700091222013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
045800091222014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
045900091222016                     DESTINATARIO RIMANDA LO SVINCOLO AL
046000091222017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
046100091222018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
046200091222019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
046300091222020                     DESTINATARIO RISULTA TRASFERITO
046400091222021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
046500091222022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
046600091222023                     IL DESTINATARIO E' CHIUSO FINO AL
046700091222024                     IL DESTINATARIO E' CHIUSO PER FERIE
046800091222025                     IL DESTINATARIO E' CHIUSO
046900091222026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
047000091222027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
047100091222028                     ESERCIZIO NON ANCORA IN ATTIVITA
047200091222029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
047300091222030                     IL DESTINATARIO RIFIUTA LA MERCE
047400091222031                     IL DESTINATARIO NON INTENDE
047500091222032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
047600091222033                     IL DESTINATARIO RICHIEDE
047700091222034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
047800091222035                     DOCUMENTI INCOMPLETI O MANCANTI
047900091222036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
048000091222037                     RIFIUTA LA CONSEGNA TASSATIVA
048100091222100                     -
048200091222101                     -
048300091222999                     -
048400091222CONSOK                  CONSEGNA OK
048500091222RESO                    RESO
048600091222
048700091222** CAUCLI - CAUSALI CLIENTE
048800100215008                     L.AVVISO
048900100215002                     AVVISATO-RIMANDA SVINCOLO 1GG
049000100215008                     LASCIATO AVVISO 1GG
049100100215008                     LASCIATO AVVISO 2GG
049200100215008                     LASCIATO AVVISO 3GG
049300100215008                     LASCIATO AVVISO 5GG
049400100215008                     LASCIATO AVVISO 7GG
049500100215008                     LASCIATO AVVISO 9 GIORNI
049600100215002                     L.A. RIMANDA LO SVINCOLO
049700100215042                     L.A. CHIUSO FINO AL
049800091222NON INVIARE             CAMBIO PORTO
049900100215NON INVIARE             COLLO/I DANNEGGIATO/I
050000091222NON INVIARE             DIROTTAMENTO
050100091222NON INVIARE             FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
050200100215001                     GIACENZA GENERICA TEMPORANEA
050300100215001                     GIACENZA GENERICA
050400091222NON INVIARE             ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
050500091222NON INVIARE             ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
050600091222NON INVIARE             ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
050700091222NON INVIARE             ATTENDERE 9 GIORNI
050800100215NON INVIARE             CHIUSURA SPEDIZIONE CON IDD
050900100215NON INVIARE             COLLO/I MANCANTE/I
051000100215004                     MESSA IN CONSEGNA
051100100215NON INVIARE             NON FATTA
051200100215NON INVIARE             COLLO SCONDIZIONATO
051300100215NON INVIARE             TOLTA DALLA CONSEGNA
051400100215000                     CONSEGNA PARZIALE
051500100215010                     FESTIVITA PATRONALE
051600100215NON INVIARE             RISERVA
051700100215004                     RIPRISTINO
051800100215007                     RIFIUTATO
051900100215007                     RIFIUTATO
052000100215004                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
052100100215NON INVIARE             RIPRISTINO
052200100215NON INVIARE             RISERVA
052300100215021                     CHIUSO PER TURNO
052400100215NON INVIARE             AMMANCO SENZA EVENTO
052500100215NON INVIARE             AMMANCO CON EVENTO
052600100215NON INVIARE             MANCANZA SENZA EVENTO
052700100215NON INVIARE             MANCANZA CON EVENTO
052800100215NON INVIARE             AVARIA SENZA EVENTO
052900100215NON INVIARE             AVARIA CON EVENTO
053000100215084                     ALLUVIONE
053100100215014                     BLOCCO STRADALE
053200100215010                     FESTIVITÀ REGIONALE
053300100215084                     MARE MOLTO MOSSO
053400100215084                     MANIFESTAZIONE PUBBLICA
053500100215084                     NEVICATA ECCEZIONALE
053600100215015                     SCIOPERO
053700100215007                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
053800100215007                     MERCE NON ORDINATA O NON CONFORME
053900100215007                     MERCE SPEDITA IN RITARDO
054000100215007                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
054100100215007                     MERCE SPEDITA CON TROPPO ANTICIPO
054200100215007                     MERCE RESA SENZA AUTORIZZAZIONE
054300100215007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
054400100215007                     DESTINATARIO NON PAGA IL C/ASSEGNO
054500100215007                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
054600100215007                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
054700100215007                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
054800100215007                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
054900100215007                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
055000100215007                     DESTINATARIO RIMANDA LO SVINCOLO AL
055100100215008                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
055200100215007                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
055300100215005                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
055400100215005                     DESTINATARIO RISULTA TRASFERITO
055500100215005                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
055600100215003                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
055700100215042                     IL DESTINATARIO E' CHIUSO FINO AL
055800100215042                     IL DESTINATARIO E' CHIUSO PER FERIE
055900100215021                     IL DESTINATARIO E' CHIUSO
056000100215002                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
056100100215007                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
056200100215005                     ESERCIZIO NON ANCORA IN ATTIVITA
056300100215007                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
056400100215007                     IL DESTINATARIO RIFIUTA LA MERCE
056500100215007                     IL DESTINATARIO NON INTENDE
056600100215FDP                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
056700100215007                     IL DESTINATARIO RICHIEDE
056800100215084                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
056900100215006                     DOCUMENTI INCOMPLETI O MANCANTI
057000100215007                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
057100100215007                     RIFIUTA LA CONSEGNA TASSATIVA
057200091222NON INVIARE             -
057300091222NON INVIARE             -
057400091222NON INVIARE             -
057500100215000                     CONSEGNA OK
057600100215RRR                     RESO
057700080618
057800091222** DESCLI - DESCRIZIONI CAUSALI CLIENTE
057900091222L.AVVISO
058000091222AVVISATO-RIMANDA SVINCOLO 1GG
058100091222LASCIATO AVVISO 1GG
058200091222LASCIATO AVVISO 2GG
058300091222LASCIATO AVVISO 3GG
058400091222LASCIATO AVVISO 5GG
058500091222LASCIATO AVVISO 7GG
058600091222LASCIATO AVVISO 9 GIORNI
058700091222L.A. RIMANDA LO SVINCOLO
058800091222L.A. CHIUSO FINO AL
058900091222CAMBIO PORTO
059000091222COLLO/I DANNEGGIATO/I
059100091222DIROTTAMENTO
059200091222FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
059300091222GIACENZA GENERICA TEMPORANEA
059400091222GIACENZA GENERICA
059500091222ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
059600091222ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
059700091222ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
059800091222ATTENDERE 9 GIORNI
059900091222CHIUSURA SPEDIZIONE CON IDD
060000091222COLLO/I MANCANTE/I
060100091222MESSA IN CONSEGNA
060200091222NON FATTA
060300091222COLLO SCONDIZIONATO
060400091222TOLTA DALLA CONSEGNA
060500091222CONSEGNA PARZIALE
060600091222FESTIVITA PATRONALE
060700091222RISERVA
060800091222RIPRISTINO
060900091222RIFIUTATO
061000091222RIFIUTATO
061100091222RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
061200091222RIPRISTINO
061300091222RISERVA
061400091222CHIUSO PER TURNO
061500091222AMMANCO SENZA EVENTO
061600091222AMMANCO CON EVENTO
061700091222MANCANZA SENZA EVENTO
061800091222MANCANZA CON EVENTO
061900091222AVARIA SENZA EVENTO
062000091222AVARIA CON EVENTO
062100091222ALLUVIONE
062200091222BLOCCO STRADALE
062300091222FESTIVITÀ REGIONALE
062400091222MARE MOLTO MOSSO
062500091222MANIFESTAZIONE PUBBLICA
062600091222NEVICATA ECCEZIONALE
062700091222SCIOPERO
062800091222RIFIUTA SENZA SPECIFICARE IL MOTIVO
062900091222MERCE NON ORDINATA O NON CONFORME
063000091222MERCE SPEDITA IN RITARDO
063100091222MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
063200091222MERCE SPEDITA CON TROPPO ANTICIPO
063300091222MERCE RESA SENZA AUTORIZZAZIONE
063400091222DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
063500091222DESTINATARIO NON PAGA IL C/ASSEGNO
063600091222CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
063700091222DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
063800091222IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
063900091222DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
064000091222DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
064100091222DESTINATARIO RIMANDA LO SVINCOLO AL
064200091222DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
064300091222DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
064400091222DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
064500091222DESTINATARIO RISULTA TRASFERITO
064600091222DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
064700091222INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
064800091222IL DESTINATARIO E' CHIUSO FINO AL
064900091222IL DESTINATARIO E' CHIUSO PER FERIE
065000091222IL DESTINATARIO E' CHIUSO
065100091222IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
065200091222DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
065300091222ESERCIZIO NON ANCORA IN ATTIVITA
065400091222IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
065500091222IL DESTINATARIO RIFIUTA LA MERCE
065600091222IL DESTINATARIO NON INTENDE
065700091222FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
065800091222IL DESTINATARIO RICHIEDE
065900091222SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
066000091222DOCUMENTI INCOMPLETI O MANCANTI
066100091222IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
066200091222RIFIUTA LA CONSEGNA TASSATIVA
066300091222-
066400091222-
066500091222-
066600091222CONSEGNA OK
066700091222RESO
066800080618
066900091222** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
067000100215A                        AMMANCO
067100100215M                        MANCA COLLO
067200100215V                        AVARIA
067300100215E                        EVENTO
067400091222** WDCTCLI - CAUSALI CLIENTE
067500100215NON INVIARE              AMMANCO
067600100215NON INVIARE              MANCA COLLO
067700100215NON INVIARE              AVARIA
067800100215NON INVIARE              EVENTO
