000100081111     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400090323     FTISTB02L  UF   E           K DISK    commit
000500090323     FTIVAWWWT  UF A E             DISK    commit
000600031103
000700031103     D*-----------
000800031103     D* SCHIERE A PROGRAMMA
000900031103     D*-----------
001000090120     D CAUBAR          S             10    DIM(43) CTDATA PERRCD(1)
001100090120     D DESBAR          S             30    DIM(43) CTDATA PERRCD(1)
001200090120     D CAUCLI          S             11    DIM(43) CTDATA PERRCD(1)
001300041015     D*-----------
001400041015     D* VARIABILI D WRK
001500041015     D*-----------
001600031103     D prmppi          S             50
001700030924     D prmesito        S              1
001800030924     D wrkesito        S                   like(prmesito)
001900080901     D j               S              2  0
002000031103
002100031103     D*-----------
002200031103     D* VARIABILI RIFERITE AL DATA-BASE
002300031103     D*-----------
002400080618     D wKstbKSU        S                   like(stbKSU)
002500080618     D wKstbFTR        S                   like(stbFTR)
002600030924
002700030924     D*-----------
002800030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
002900030924     D*-----------
003000030925     D C_DS_Cli        DS
003100080901     D  C_Filler001                  10    INZ(*zeros)
003200080901     D  C_DDT                        10    INZ(*zeros)
003300080901     D  C_DataStato                   8    INZ(*zeros)
003400080901     D  C_OraStato                    6    INZ(*zeros)
003500080901     D  C_CodStato                    1    INZ(*blanks)
003600080904     D  C_Filler002                  20    INZ(*blanks)
003700080901     D  C_IDcorriere                 10    INZ(*blanks)
003800080901     D  C_DCMprevista                 8    INZ(*zeros)
003900080901     D  C_GiacDesc                   30    INZ(*blanks)
004000080901     D  C_Filler003                  10    INZ(*blanks)
004100080901     D  C_Filler004                  20    INZ(*blanks)
004200080901     D  C_Filler005                  20    INZ(*blanks)
004300080901     D  C_Filler006                  20    INZ(*blanks)
004400080901     D  C_Logico01                    2    INZ(*blanks)
004500080901     D  C_Logico02                    2    INZ(*blanks)
004600031103
004700031103     D*-----------
004800031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
004900031103     D*-----------
005000031103     D DSPPI           DS
005100080618     D  w§PPIKSU                      7  0
005200080901
005300080901     D*-----------
005400080901     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
005500080901     D*-----------
005600080901     D                 DS
005700080901     D  dataAAS                1      4
005800080901     D  dataMGS                5      8
005900080901     D  dataMM                 5      6
006000080901     D  dataGG                 7      8
006100080901     D DS_DATA                 1      8
006200031103
006300031103     D*------------------
006400031103     D* DS "XSRDA8" - CONTROLLA DATA (8)
006500031103     D*------------------
006600031103     D WLBDA8          DS                  INZ
006700031103     D  G08DAT                 1      8  0
006800031103     D  G08INV                 9     16  0
006900031103     D  G08ERR                17     17
007000031103     D  G08TGI                18     22  0
007100081111
007200081111
007300081111     D*------------------
007400081111     D* PASSAGGIO PARAMETRI A PROCEDURE UBLBLSPE
007500081111     D*------------------
007600081111     D InAnno          S              4S 0
007700081111     D InLineaPar      S              3S 0
007800081111     D InSerie         S              2S 0
007900081111     D InNumSped       S              7S 0
008000081111     D OutLblTyp       S              1A
008100081111     D OutAnnoBO       S              4S 0
008200081111     D OutLineaParBO   S              3S 0
008300081111     D OutSerieBO      S              2S 0
008400081111     D OutNumSpedBO    S              7S 0
008500081111     D OutDcmBO        S              8S 0
008600081111     D OutCcaBO        S              1A
008700081111     D OutRblBO        S              1A
008800081111     D OutAnnoFI       S              4S 0
008900081111     D OutLineaParFI   S              3S 0
009000081111     D OutSerieFI      S              2S 0
009100081111     D OutNumSpedFI    S              7S 0
009200081111     D OutDcmFI        S              8S 0
009300081111     D OutCcaFI        S              1A
009400081111     D*------------------
009500081111     D* LINKING A DEFINIZIONI ESTERNE
009600081111     D*------------------
009700081111     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
009800081111
009900081111
010000030924
010100030325
010200030924     C* MAIN/
010300041015     C*
010400041015     C* Esegue traduzione
010500991027     C                   EXSR      traduci
010600921023     C                   SETON                                        LR
010700991027
010800030924
010900030924
011000991027     C     traduci       BEGSR
011100031103     C*
011200031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
011300031103     C* di traduzione (trattasi d "post-traduzione")
011400031104     C                   EVAL      wKstbKSU = w§PPIKSU
011500031103     C                   EVAL      wKstbFTR = *blanks
011600031103     C     KEYstb02P     SETLL     tistb02l
011700031103     C                   IF        %equal(tistb02l)
011800031103     C     KEYstb02P     READE     tistb02l
011900991027     C*
012000031103     C* Ciclo x tuttii record del cliente da trasmettere
012100031103     C                   DOW       not %eof(tistb02l)
012200031014     C*
012300030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
012400031014     C                   RESET                   C_DS_Cli
012500080618     C*
012600080618     C                   SETON                                        50
012700031103     C*
012800031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
012900031103     C* deve restituire al cliente nel file tradotto
013000031103     C                   IF        stbFTR = *blanks                             * ridondante
013100031103     C*
013200080618     C* Gestisco solamente gli stati standard
013300080618     C                   IF        stbTIS = '1'
013400080618     C*
013500080901     C                   SETON                                        50
013600081111     C*
013700081111     C* Inizializzo i campi d wrk
013800081111     C                   movel     *blanks       wEsito1           1
013900080902     C*
014000081111     C* Occorre ignorare le bolle figlie (chiamata metodo GetLblTyp)
014100081111     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
014200081111     C                                                STBAAS
014300081111     C                                               :STBLNP
014400081111     C                                               :STBNRS
014500081111     C                                               :STBNSP
014600081111     C                                               :OutLblTyp
014700081111     C                                               :OutAnnoBO
014800081111     C                                               :OutLineaParBO
014900081111     C                                               :OutSerieBO
015000081111     C                                               :OutNumSpedBO
015100081111     C                                               :OutDcmBO
015200081111     C                                               :OutCcaBO
015300081111     C                                               :OutRblBO))
015400081111     C*
015500081111     C* Se trattasi d bolla originale proseguo con le verifiche
015600081111     C                   IF        OutLblTyp = 'O'
015700081111     C*
015800081111     C* No dirottamenti e no resi
015900081111     C                   IF        stbCOS = 'DIR' OR
016000081111     C                             stbCOS = 'RESO'
016100080902     C                   ELSE
016200080901     C*
016300080916     C* Gestione stati cliente 1 (consegne OK)
016400081111     C                   IF        stbCOS = 'CONSOK' AND
016500081111     C                             OutDcmBO > *zeros
016600080901     C                   EVAL      C_CodStato = '1'
016700090113     C                   EVAL      C_GiacDesc = 'CONSEGNA EFFETTUATA'
016800080901     C                   SETOFF                                       50
016900080901     C                   ELSE
017000080901     C*
017100080916     C* Gestione stati cliente 2/3
017200080901     C* - Effettuo l'abbinamento tra causali Bartolini e causali cliente
017300031103     C                   Z-ADD     1             j
017400031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
017500080618     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
017600080901     C                   EVAL      C_CodStato = %trim(CAUCLI(j))
017700090112     C                   EVAL      C_GiacDesc = %trim(DESBAR(j))
017800080901     C                   SETOFF                                       50
017900031103     C                   ELSE
018000080901     C*
018100080916     C* Gestione stati cliente 0
018200080916     C                   EVAL      C_CodStato = '0'
018300080901     C                   SETOFF                                       50
018400080618     C                   ENDIF
018500080901     C                   ENDIF
018600080618     C*
018700080618     C* Scarico il buffer d output
018800080618     C  N50              EXSR      WRIREC
018900080901     C*
019000080901     C                   ENDIF
019100080902     C                   ENDIF
019200031103     C*
019300031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
019400031103     C                   EVAL      stbFTR = 'T'
019500031103     C                   UPDATE    tistb000
019600041015     C                   ENDIF
019700080618     C                   ENDIF
019800991027     C*
019900031103     C     KEYstb02P     READE     tistb02l
020000031103     C                   SETOFF                                       50
020100030325     C                   ENDDO
020200031103     C                   ENDIF
020300991027     C*
020400030325     C                   EVAL      wrkesito = '0'
020500991027     C*
020600910830     C                   ENDSR
020700031103
020800031103
020900031103
021000080618     C     wrirec        BEGSR
021100031103     C*
021200080901     C                   MOVE(P)   stbRMN        C_DDT
021300080618     C                   MOVEL(P)  STBDAS        DS_DATA
021400080904     C                   EVAL      C_DataStato   = dataAAS+dataMM+dataGG
021500080904     C                   MOVEL     STBORS        C_OraStato
021600031103     C*
021700031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
021800031103     C                   EVAL      VAWDTA = C_DS_Cli
021900031103     C                   WRITE     TIVAW000
022000031103     C*
022100031103     C                   ENDSR
022200031103
022300031103
022400991027
022500991027      /TITLE Operazioni iniziali.
022600991027     C     *inzsr        BEGSR
022700991027     C*
022800991027     C     *ENTRY        PLIST
022900031103     C                   parm                    prmppi
023000991027     C     wrkesito      parm      wrkesito      prmesito
023100031103     C*
023200031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
023300031103     C                   EVAL      DSPPI = prmppi
023400031103     C*
023500031103     C* Calcola la data corrente
023600031103     C                   time                    wn14             14 0
023700031103     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
023800031103     C                   z-add     wn8           g08dat
023900031103     C                   z-add     *zeros        g08inv
024000031103     C                   movel     '0'           g08err
024100031103     C                   call      'XSRDA8'
024200031103     C                   parm                    wlbda8
024300031103     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
024400030709     C*
024500030709     C* Definizione chiavi
024600030924     C*
024700031103     C* Chiave su TISTB02L - Parziale
024800031103     C     KEYstb02P     KLIST
024900031104     C                   KFLD                    wKstbKSU
025000031103     C                   KFLD                    wKstbFTR
025100991027     C*
025200991027     C                   ENDSR
025300031103** CAUBAR - CAUSALI BARTOLINI
025400080618001
025500080618002
025600080618003
025700080618004
025800080618005
025900080618006
026000080618007
026100080618008
026200080618009
026300080618010
026400080618012
026500080618013
026600080618014
026700080618027
026800080618030
026900080904RESO
027000080904DIR
027100080904MIC
027200080904N
027300080904NIC
027400080904T
027500080916016
027600080916017
027700080916018
027800080916019
027900080916020
028000080916021
028100080916022
028200080916023
028300080916024
028400080916026
028500080916028
028600080916029
028700080916031
028800080916032
028900080916033
029000080916034
029100080916035
029200080916036
029300080916037
029400080916100
029500080916101
029600090120P
029700090112** DESBAR - CAUSALI BARTOLINI
029800090112RIFIUTA SENZA SPECIFICARE IL MOTIVO
029900090112MERCE NON ORDINATA O NON CONFORME
030000090112MERCE SPEDITA IN RITARDO
030100090112MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
030200090112MERCE SPEDITA CON TROPPO ANTICIPO
030300090112MERCE RESA SENZA AUTORIZZAZIONE
030400090112DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
030500090112DESTINATARIO NON PAGA IL C/ASSEGNO
030600090112CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
030700090112DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
030800090112IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
030900090112DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
031000090112DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
031100090112DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
031200090112IL DESTINATARIO RIFIUTA LA MERCE
031300090112RESO
031400090112DIROTTAMENTO
031500090112MESSA IN CONSEGNA
031600090112NON FATTA
031700090112TOLTA DALLA CONSEGNA
031800090112CHIUSO PER TURNO
031900090112DESTINATARIO RIMANDA LO SVINCOLO AL
032000090112DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
032100090112DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
032200090112DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERIT
032300090112DESTINATARIO RISULTA TRASFERITO
032400090112DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DEL DDT
032500090112INDIRIZZO INDICATO SUL DDT ERRATO/INESISTENTE
032600090112IL DESTINATARIO E' CHIUSO FINO AL
032700090112IL DESTINATARIO E' CHIUSO PER FERIE
032800090112IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
032900090112ESERCIZIO NON ANCORA IN ATTIVITA'
033000090112IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
033100090112IL DESTINATARIO NON INTENDE
033200090112FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
033300090112IL DESTINATARIO RICHIEDE
033400090112SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
033500090112DOCUMENTI INCOMPLETI O MANCANTI
033600090112IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
033700090112RIFIUTA LA CONSEGNA TASSATIVA
033800090112CONSEGNA RICHIESTA OLTRE 72 ORE
033900090112A SEGUITO L.AVV.CONCORDATA CONSEGNA OLTRE 72 ORE
034000090120CONSEGNA PARZIALE
034100080901** CAUCLI - CAUSALI CLIENTE
0342000809012
0343000809012
0344000809012
0345000809012
0346000809012
0347000809012
0348000809012
0349000809012
0350000809012
0351000809012
0352000809012
0353000809012
0354000809012
0355000809012
0356000809012
035700080904NON INVIARE
035800080904NON INVIARE
0359000809040
0360000809040
0361000809040
0362000809160
0363000809163
0364000809163
0365000809163
0366000809163
0367000809163
0368000809163
0369000809163
0370000809163
0371000809163
0372000809163
0373000809163
0374000809163
0375000809163
0376000809163
0377000809163
0378000809163
0379000809163
0380000809163
0381000809163
0382000809163
0383000809163
0384000901203
