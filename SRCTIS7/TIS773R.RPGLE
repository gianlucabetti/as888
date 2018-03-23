000100991020     h*--------------------------------------------------------------------------------------------*
000200991020     h* Visualizza bolle da selezionare
000300991020     h*--------------------------------------------------------------------------------------------*
000400991020      /TITLE Carica righe spedizione trovate
000500990907     Hdatedit(*DMY)
000600060612     H BNDDIR('TIS':'QC2LE') DFTACTGRP(*NO) ACTGRP(*CALLER)
000700991020     f*--------------------------------------------------------------------------------------------*
000800991020     f* Data base
000900991020     f*--------------------------------------------------------------------------------------------*
001000991020     f*---
001100991105     f* Estratto bolle di sede -ordinamento per cliente unificante + data spedizione-
001200991020     f*---
001300090901     ftivta12l  IF   E           K DISK    USROPN
001400090901     ftivta15l  IF   E           K DISK    USROPN
001500000316     f                                     RENAME(tivta000:tivta5)
001600991020     d*--------------------------------------------------------------------------------------------*
001700991020     d* Data structure
001800991020     d*--------------------------------------------------------------------------------------------*
001900991021     d*---
002000991021     d* Costanti
002100991021     d*---
002200991110     d minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
002300991110     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
002400060608     d*---
002500060608     d* Procedure chiamate.
002600060608     d*---
002700060612     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
002800060612     D  msgId                         7A   CONST
002900060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003000060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003100060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
003200060612     D                                     VARYING
003300060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
003400060612     D                                     VARYING
003500060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
003600060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
003700090901     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
003800090901     d*---
003900991110     d* Schiere a tempo di compilazione
004000991110     d*---
004100991110     d ssta            S             15    DIM(10) CTDATA PERRCD(1)             *Stato bolla
004200991020     d*---
004300991020     d* Variabili riferite al data base
004400991020     d*---
004500090901     d ktaksu          S                   LIKE(vtaksu)                         *lettura tivta12l
004600991021     d ktadsp          S                   LIKE(vtadsp)
004700991021     d ktalnp          S                   LIKE(vtalnp)
004800000316     d ktarmn          S                   LIKE(vtarmn)
004900991021     d ktanrs          S                   LIKE(vtanrs)
005000991021     d ktansp          S                   LIKE(vtansp)
005100991021     d ktatbl          S                   LIKE(vtatbl)
005200991105     d depksu          S                   LIKE(vtaksu)                         *depositi
005300991021     d depdsp          S                   LIKE(vtadsp)
005400991021     d deplnp          S                   LIKE(vtalnp)
005500991021     d depnrs          S                   LIKE(vtanrs)
005600991021     d depnsp          S                   LIKE(vtansp)
005700991021     d deptbl          S                   LIKE(vtatbl)
005800000316     d deprmn          S                   LIKE(vtarmn)
005900991108     d ndsti72         S                   LIKE(dsti72)                         *campi"normalizzati"
006000010125     d nmiti72         S                   LIKE(miti72)
006100010125     d ndesi72         S                   LIKE(desi72)
006200991108     d ndini72         S                   LIKE(vtadsp)
006300991108     d ndfii72         S                   LIKE(vtadsp)
006400030806     d nnspedizi72     S                   LIKE(nspedizi72)
006500040920     d cRifI72         S                   LIKE(rifi72)                         *confronto -CIRCA-
006600040920     d nRifI72         S                   LIKE(VtaRMN)
006700040920     d N15cRifI72      S                   LIKE(vtarma)                         *confronto -PRECISO-
006800040920     d A15cRifI72      S                   LIKE(vtarma)
006900040920     d P20cRifI72      S                   LIKE(vtarpt)
007000991020     d*---
007100991020     d* Variabili
007200991020     d*---
007300991021     d $fvta           S              1    INZ('N')                             *fine lettura
007400991021     d $recok          S              1    INZ('N')                             *validità record
007500991111     d $letrif         S              1                                         *riferim preciso/C.a
007600000316     d $letrmn         S              1                                         *rif.num preciso/C.a
007700000316     d $rifrmnok       S              1                                         *rif.num=*all numeri
007800991029     d contbol         S              5  0 INZ                                  *contatore bolle
007900040920     d i               S              5I 0                                      *indici
008000040920     d z               S              5I 0                                      *indici
008100000223     d a40             S             40    INZ                                  *Alfanumerico 40
008200991109     d a30             S             30                                         *Alfanumerico 30
008300991109     d a30bis          S             30                                         *Alfanumerico 30
008400991109     d a30tris         S             30                                         *Alfanumerico 30
008500991109     d a20             S             20                                         *Alfanumerico 20
008600991108     d a15             S             15                                         *Alfanumerico 15
008700991108     d a15bis          S             15                                         *Alfanumerico 15
008800000316     d a1              S              1                                         *Alfanumerico 1
008900991105     d n14             S             14  0                                      *Numerico 14
009000991021     d n8              S              8  0                                      *Numerico 8
009100991021     d n7              S              7  0                                      *numerco 7
009200040920     d pos             S              5I 0                                      *posizione SCAN
009300040920     d LenRif          S              5I 0
009400991021     d datcor          S              8  0                                      *data corrente
009500991216     d oracor          S              6  0                                      *ora corrente
009600991108     d wpgm            S             20                                         *programma
009700060612     d* Input routine XALLINEA
009800991109     d walfa15         S             15                                         *alfanumerico 15
009900991109     d walfa15bis      S             15                                         *alfanumerico 15
010000991109     d wzeri           S             15                                         *alfa con zeri
010100991021     d flag            S              1                                         *flag
010200991021     d Operazione      S              1
010300991028     d RigheOut        S              5s 0
010400991021     d RigheRic        S              5s 0
010500991021     d Esito           S              1
010600090901     d rpyEsito        S             10I 0
010700991020     d*---
010800991020     d* Ds
010900991020     d*---
011000991021      * DS controlla data
011100991021     d wlbda8          DS                  INZ                                  *controlla data
011200991021     d  g08dat                        8  0                                       -data dritta
011300991021     d  g08inv                        8  0                                       -data invertita
011400991021     d  g08err                        1                                          -errore
011500991021     d  g08tgi                        5  0                                       -giorni fra date
011600991021      * Scomposizione data editata
011700991025     d                 DS                  INZ
011800991025     d  dsgioe                 1      2                                          -giorno
011900991025     d  dsvd1                  3      3                                          -'/'
012000991025     d  dsmese                 4      5                                          -mese
012100991025     d  dsvd2                  6      6                                          -'/'
012200991025     d  dsanne                 7     10                                          -anno
012300991025     d dsdate                  1     10                                         *data editata
012400011128      * Composizione data NON editata -alfabetica-
012500011128     d                 DS                  INZ
012600011128     d  dsgioa                 1      2                                          -giorno
012700011128     d  dsmesa                 3      4                                          -mese
012800011128     d  dsanna                 5      8                                          -anno
012900011128     d dsdata                  1      8                                         *data NON editata
013000991021      * scomposizione data generica
013100991025     d                 DS                  INZ
013200991025     d  dsann                  1      4  0                                       -anno
013300991025     d  dsmes                  5      6  0                                       -mese
013400991025     d  dsgio                  7      8  0                                       -giorno
013500991025     d dsdat                   1      8  0                                      *data
013600991025      * composizione spedizione
013700991025     d                 DS                  INZ
013800991025     d  vtalnp                 1      3  0                                       -linea partenza
013900991025     d  vtanrs                 4      5  0                                       -serie
014000991025     d  vtansp                 6     12  0                                       -spedizione
014100991025     d vtaspe                  1     12                                         *codice spedizione
014200991021      * DS contenente i dati dI INPUT
014300991021     d TIS172DSI     E DS
014400991020     d* DS ritornata dal programma di accesso ai dati -> contiene una riga di dati
014500991217     d TIS173DSO     E DS                  OCCURS(20)
014600090902     D tis7700i1     E DS                  QUALIFIED
014700090901     D                                     INZ(*EXTDFT)
014800090902     D tis7700o1     E DS                  QUALIFIED
014900090902     D                                     INZ(*EXTDFT)
015000090901
015100040920     D atof            PR             8F
015200040920     D                                     EXTPROC('atof')
015300040920     D   Char                          *
015400040920     D                                     VALUE
015500040920     D                                     OPTIONS(*STRING)
015600040920
015700991020     c*--------------------------------------------------------------------------------------------*
015800991021     c* Main lines
015900991020     c*--------------------------------------------------------------------------------------------*
016000991022     c*--- --- --- --- --- --- ---
016100991022     c* Operazione
016200991022     c* '1' = Inizio nuova richiesta
016300991022     c* '2' = Dati trovati, richiesta successiva
016400991022     c* '9' = Non trovati dati
016500991022     c*--- --- --- --- --- --- ---
016600991028     c*
016700991028     c* operazioni iniziali -da fare sempre-
016800991111     c                   EXSR      rutinz
016900991111     c*
017000991112     c* Se operazione = 1 esegue un tentativo per trovare in modo univoco il riferimeento ALTRIMENTI
017100991112     c*                   trova le bolle con iferimento circa uguale
017200991111if  1c                   IF        Operazione = '1'
017300991112     c*
017400991112     c* operazioni per nuova richiesta
017500991111     c                   EXSR      newric
017600000316     c*
017700000316     c* imposta i depositi della chiave di lettura per la prima lettura
017800000316     c                   EXSR      firstlet
017900000316     c*
018000000316     c* controlla se leggere successivamente per riferimento numerico -PRECISO-
018100000316     c                   EXSR      choisletvta0
018200991111     c*
018300991111     c* imposta i depositi della chiave di lettura per la prima lettura
018400000316if  2c                   IF        $letrmn <> 'S'                               *no rif.num. PRECISO
018500991111     c                   EXSR      firstlet
018600991108     c*
018700991108     c* controlla se leggere successivamente per riferimento -CIRCA- oppure -PRECISO-
018800000316     c                   EXSR      choisletvta1
018900000316e   2c                   ENDIF
019000991111     c*
019100991112     c* REimposta i depositi della chiave di lettura per la prima lettura
019200991111     c                   EXSR      firstlet
019300991022     c*
019400991022     c* elabora il file TIVTA -estratto bolle-
019500991022     c                   EXSR      elavta
019600991108e   1c                   ENDIF
019700990907     c*
019800990907     c* Se operazione = '2' esegue letture successive
019900991021if  1c                   IF        Operazione = '2'
020000991022     c*
020100991022     c* a fine file -giro precedente- imposta Operazione = '9'
020200991025if  2c                   IF        $fvta = 'S'                                  *fine file
020300991028     c                   EVAL      Operazione = '9'
020400991022     c                   EVAL      RigheOut =  *zeros
020500991022x   2c                   ELSE                                                   *no fine file
020600991022     c*
020700991022     c* elabora il file TIVTA -estratto bolle-
020800991022     c                   EXSR      elavta
020900991021e   2c                   ENDIF
021000991022e   1c                   ENDIF
021100991020     c*
021200991108     c                   RETURN
021300000316     c*--------------------------------------------------------------------------------------------*
021400000316     c* controlla se leggere successivamente per riferimento numerico -PRECISO-
021500000316     c*--------------------------------------------------------------------------------------------*
021600000316     c     choisletvta0  BEGSR
021700000316     c*
021800000316     c* solo se richiesto il riferimento mittente numerico
021900040920if  1c                   IF        N15cRifI72 > *zeros                          *chiesto rif.numer.
022000000316if  2c                   IF        $rifrmnok = 'S'                              *SI rif.num = numeri
022100000316     c*
022200000316     c* elabora il file TIVTA -estratto bolle-
022300000316     c                   MOVEL     depksu        ktaksu                         *deposito unificante
022400000316     c                   Z-ADD     deprmn        ktarmn                         *deposito rifer.num.
022500000316     c                   Z-ADD     depdsp        ktadsp                         *deposito data sped.
022600090901     c     ke2vta        SETLL     tivta15l
022700090901     c     ke3vta        READE     tivta15l                               99
022800000316do  3c                   DOW       NOT *in99
022900000316if  4c                   IF        contbol > 999                                *massimo 1000 bolle
023000000316     c                   LEAVE                                                  *uscita ciclo
023100000316e   4c                   ENDIF
023200000316     c                   MOVEL     'S'           $recok
023300000316     c                   EXSR      chkvtadsp                                    *chk data spedizione
023400000316     c                   EXSR      chkvtadst                                    *chk destinatario
023500010125     c                   EXSR      chkvtamit                                    *chk mittente
023600010125     c                   EXSR      chkvtades                                    *chk destinazione
023700000316     c                   EXSR      chkvtacon                                    *chk stato consegna
023800030806     c                   EXSR      chkvtaspe                                    *chk n.spedizione
023900000316if  4c                   IF        $recok = 'S'                                 *record valido
024000140218     c*                  EVAL      $letrmn = 'S'                                *SI riferim PRECISO
024100000316     c                   EVAL      contbol = contbol + 1                        *conteggio righe
024200000316     c                   LEAVE                                                  *uscita ciclo
024300000316e   4c                   ENDIF
024400090901     c     ke3vta        READE     tivta15l                               99
024500000316e   3c                   ENDDO
024600000316e   2c                   ENDIF
024700000316e   1c                   ENDIF
024800000316     c*
024900000316     c                   ENDSR
025000991108     c*--------------------------------------------------------------------------------------------*
025100991108     c* controlla se leggere successivamente per riferimento -CIRCA- oppure -PRECISO-
025200991108     c*--------------------------------------------------------------------------------------------*
025300000316     c     choisletvta1  BEGSR
025400991108     c*
025500991108     c* solo se richiesto il riferimento mittente
025600991108if  1c                   IF        rifi72 <> *blanks                            *chiesto riferimento
025700991108     c*
025800991108     c* elabora il file TIVTA -estratto bolle-
025900000316     c                   MOVEL     depksu        ktaksu                         *deposito unificante
026000000316     c                   Z-ADD     depdsp        ktadsp                         *depositi chiave
026100000316     c                   Z-ADD     deplnp        ktalnp
026200000316     c                   Z-ADD     depnrs        ktanrs
026300000316     c                   Z-ADD     depnsp        ktansp
026400000316     c                   MOVEL     deptbl        ktatbl
026500090901     c     keyvta        SETLL     tivta12l
026600090901     c     ktaksu        READE     tivta12l                               99
026700991108do  2c                   DOW       NOT *in99
026800991217if  3c                   IF        contbol > 999                                *massimo 1000 bolle
026900991112     c                   LEAVE                                                  *uscita ciclo
027000991108e   3c                   ENDIF
027100090901     c                   DO
027200991108     c                   MOVEL     'S'           $recok
027300991108     c                   EXSR      chkvtadsp                                    *chk data spedizione
027400090901     C                   IF        $recok = 'N'
027500090901     C                   LEAVE
027600090901     C                   ENDIF
027700991108     c                   EXSR      chkvtadst                                    *chk destinatario
027800090901     C                   IF        $recok = 'N'
027900090901     C                   LEAVE
028000090901     C                   ENDIF
028100010125     c                   EXSR      chkvtamit                                    *chk mittente
028200090901     C                   IF        $recok = 'N'
028300090901     C                   LEAVE
028400090901     C                   ENDIF
028500010125     c                   EXSR      chkvtades                                    *chk destinazione
028600090901     C                   IF        $recok = 'N'
028700090901     C                   LEAVE
028800090901     C                   ENDIF
028900991108     c                   EXSR      chkvtacon                                    *chk stato consegna
029000090901     C                   IF        $recok = 'N'
029100090901     C                   LEAVE
029200090901     C                   ENDIF
029300991108     c                   EXSR      chkvtarifp                                   *chk riferim PRECISO
029400090901     C                   IF        $recok = 'N'
029500090901     C                   LEAVE
029600090901     C                   ENDIF
029700030806     c                   EXSR      chkvtaspe                                    *chk n.spedizione
029800090901     C                   IF        $recok = 'N'
029900090901     C                   LEAVE
030000090901     C                   ENDIF
030100090901     c                   EXSR      chkSubK
030200090901     C                   IF        $recok = 'N'
030300090901     C                   LEAVE
030400090901     C                   ENDIF
030500090901     c                   ENDDO
030600991108if  3c                   IF        $recok = 'S'                                 *record valido
030700991108     c                   EVAL      $letrif = 'S'                                *SI riferim PRECISO
030800991108     c                   EVAL      contbol = contbol + 1                        *conteggio righe
030900991108     c                   LEAVE                                                  *uscita ciclo
031000991108e   3c                   ENDIF
031100090901     c     ktaksu        READE     tivta12l                               99
031200991108e   2c                   ENDDO
031300991108e   1c                   ENDIF
031400991108     c*
031500991108     c                   ENDSR
031600991022     c*--------------------------------------------------------------------------------------------*
031700991022     c* elabora il file TIVTA -estratto bolle-
031800991022     c*--------------------------------------------------------------------------------------------*
031900991022     c     elavta        BEGSR
032000991022     c*
032100991028     c* posizionamento su TIVTA -estratto bolle-
032200991028     c                   EXSR      setvta
032300991028     c*
032400991022     c* carica la prima pagina di 20 righe
032500991028do  1c                   DOW       RigheOut < RigheRic AND                      *riempimento pagina
032600991029     c                             $fvta = 'N'                                  *fine file
032700991028     c*
032800991028     c* lettura di TIVTA -estratto bolle-
032900991028     c                   EXSR      letvta
033000991022     c*
033100991028     c* memorizza le spedizioni da paginare, se corrette
033200991028if  2c                   IF        $recok = 'S'  AND
033300991028     c                             $fvta = 'N'
033400991022     c                   EXSR      memvta
033500991028e   2c                   ENDIF
033600991022e   1c                   ENDDO
033700991022     c*
033800991022     c                   ENDSR
033900991021     c*--------------------------------------------------------------------------------------------*
034000991028     c* posizionamento su TIVTA -estratto bolle-
034100991021     c*--------------------------------------------------------------------------------------------*
034200991021     c     setvta        BEGSR
034300991021     c*
034400991021     c* posizionamento dall'ultimo record letto
034500991025     c* . se 1^ volta la chiave è impostata con cliente unificante + data inizio + zeri
034600000316     c*               oppure con cliente unificante + riferimento numerico + data inizio + zeri
034700991021     c* . se n^ volte la chiave è impostata con dati ultima bolla letta
034800991105     c                   MOVEL     depksu        ktaksu                         *deposito unificante
034900000316     c                   Z-ADD     deprmn        ktarmn                         *deposito rif.numer.
035000991021     c                   Z-ADD     depdsp        ktadsp                         *depositi chiave
035100991021     c                   Z-ADD     deplnp        ktalnp
035200991021     c                   Z-ADD     depnrs        ktanrs
035300991021     c                   Z-ADD     depnsp        ktansp
035400991025     c                   MOVEL     deptbl        ktatbl
035500000316if  1C                   IF        $letrmn = 'S'                                *SI rif.num.PRECISO
035600090901     c     ke2vta        SETGT     tivta15l                           99
035700000316x   1c                   ELSE                                                   *NO rif.num.PRECISO
035800090901     c     keyvta        SETGT     tivta12l                           99
035900000316e   1c                   ENDIF
036000991028if  1c                   IF        *in99                                        *fine file
036100991025     c                   MOVEL     'S'           $fvta                          *fine lettura
036200991029     c                   EVAL      Operazione = '9'
036300991021e   1c                   ENDIF
036400991021     c*
036500991021     c                   ENDSR
036600991021     c*--------------------------------------------------------------------------------------------*
036700991028     c* lettura TIVTA -estratto bolle-
036800991021     c*--------------------------------------------------------------------------------------------*
036900991021     c     letvta        BEGSR
037000991028     c*
037100000316if  1C                   IF        $letrmn = 'S'                                *SI rif.num.PRECISO
037200090901     c     ke3vta        READE     tivta15l                               99
037300000316x   1c                   ELSE                                                   *NO rif.num.PRECISO
037400090901     c     ktaksu        READE     tivta12l                               99
037500000316e   1c                   ENDIF
037600991028if  1c                   IF        NOT *in99                                    *record trovato
037700991021     c                   EXSR      chkvta                                       *controlla record
037800991028x   1c                   ELSE                                                   *record non trovato
037900991022     c                   MOVEL     'S'           $fvta                          *fine lettura
038000991029     c                   EVAL      Operazione = '9'
038100991028e   1c                   ENDIF
038200991021     c*
038300991021     c                   ENDSR
038400991021     c*--------------------------------------------------------------------------------------------*
038500991021     c* controlla record di TIVTA -estratto bolle-
038600991021     c*--------------------------------------------------------------------------------------------*
038700991029     c     chkvta        BEGSR
038800991021     c*
038900991029     c                   MOVEL     'S'           $recok                         *record valido
039000991029     c*
039100991029     c* le lette più di 1000 bolle, va a fine lettura
039200991217if  1c                   IF        contbol > 999                                *massimo 1000 bolle
039300991029     c                   MOVEL     'S'           $fvta                          *fine lettura
039400991029     c                   EVAL      Operazione = '9'
039500991029e   1c                   ENDIF
039600090901     C*
039700090901     C                   DO
039800991025     c*
039900991025     c* controlla validità date
040000991108     c                   EXSR      chkvtadsp
040100090901     c
040200090901     C                   IF        $recok = 'N'
040300090901     C                   LEAVE
040400090901     C                   ENDIF
040500991021     c*
040600991022     c* controlla il destinatario
040700991108     c                   EXSR      chkvtadst
040800090901     c
040900090901     C                   IF        $recok = 'N'
041000090901     C                   LEAVE
041100090901     C                   ENDIF
041200010125     c*
041300010125     c* controlla il mittente
041400010125     c                   EXSR      chkvtamit
041500090901     c
041600090901     C                   IF        $recok = 'N'
041700090901     C                   LEAVE
041800090901     C                   ENDIF
041900010125     c*
042000010125     c* controlla la destinazione
042100010125     c                   EXSR      chkvtades
042200090901     c
042300090901     C                   IF        $recok = 'N'
042400090901     C                   LEAVE
042500090901     C                   ENDIF
042600991025     c*
042700991025     c* controlla lo stato consegna
042800991108     c                   EXSR      chkvtacon
042900090901     c
043000090901     C                   IF        $recok = 'N'
043100090901     C                   LEAVE
043200090901     C                   ENDIF
043300991108     c*
043400991108     c* controlla il riferimento mittente nel riferimento -CIRCA- oppure -PRECISO-
043500000317if  1c                   IF        $letrmn = 'N'                                *NO rif.num PRECISO
043600000316if  2c                   IF        $letrif = 'N'                                *NO riferim PRECISO
043700991108     c                   EXSR      chkvtarifc                                   *controllo -CIRCA-
043800000316x   2c                   ELSE                                                   *SI riferim PRECISO
043900000317     c                   EXSR      chkvtarifp                                   *controllo -PRECISO-
044000000316e   2c                   ENDIF
044100090901     C                   IF        $recok = 'N'
044200090901     C                   LEAVE
044300090901     C                   ENDIF
044400000316e   1c                   ENDIF
044500030806     c*
044600030806     c* controlla il numero di spedizione
044700030806     c                   EXSR      chkvtaspe
044800090901     c
044900090901     C                   IF        $recok = 'N'
045000090901     C                   LEAVE
045100090901     C                   ENDIF
045200090901     c*
045300090901     c* Controlla il subunificante.
045400090901     c                   EXSR      chkSubK
045500090901     c
045600090901     C                   IF        $recok = 'N'
045700090901     C                   LEAVE
045800090901     C                   ENDIF
045900090901     C*
046000090901     C                   ENDDO
046100090901     c*
046200991028     c* memorizza in un deposito l'ultimo record letto
046300991105     c                   MOVEL     vtaksu        depksu                         *depositi
046400991025     c                   Z-ADD     vtadsp        depdsp
046500000316     c                   Z-ADD     vtarmn        deprmn
046600991025     c                   Z-ADD     vtalnp        deplnp
046700991025     c                   Z-ADD     vtanrs        depnrs
046800991025     c                   Z-ADD     vtansp        depnsp
046900991025     c                   MOVEL     vtatbl        deptbl
047000991022     c*
047100991021     c                   ENDSR
047200991108     c*--------------------------------------------------------------------------------------------*
047300991108     c* controlla validità date
047400991108     c*--------------------------------------------------------------------------------------------*
047500991108     c     chkvtadsp     BEGSR
047600991108     c*
047700991108if  1c                   IF        vtadsp >= ndini72 AND
047800991108     c                             vtadsp <= ndfii72
047900991108x   1c                   ELSE
048000991108     c                   MOVEL     'N'           $recok                         *record non valido
048100991108e   1c                   ENDIF
048200991108     c*
048300991108     c                   ENDSR
048400991108     c*--------------------------------------------------------------------------------------------*
048500991108     c* controlla il destinatario
048600991108     c*--------------------------------------------------------------------------------------------*
048700991108     c     chkvtadst     BEGSR
048800991108     c*
048900010202if  1c                   IF        ndsti72 <> *blanks                           *se richiesto
049000000223     c                   EVAL      a40 = %TRIM(vtarsd)
049100000223     c                   Z-ADD     *zeros        pos
049200000223     C                   EVAL      pos =  %SCAN(%TRIM(ndsti72):a40)
049300991108if  2C                   IF        pos <= *zeros                                *non trovato
049400991108     c                   MOVEL     'N'           $recok                         *record non valido
049500991108e   2c                   ENDIF
049600991108e   1c                   ENDIF
049700991108     c*
049800991108     c                   ENDSR
049900010125     c*--------------------------------------------------------------------------------------------*
050000010125     c* controlla il mittente
050100010125     c*--------------------------------------------------------------------------------------------*
050200010125     c     chkvtamit     BEGSR
050300010125     c*
050400010202if  1c                   IF        nmiti72 <> *blanks                           *se richiesto
050500010125     c                   EVAL      a40 = %TRIM(vtarsm)
050600010125     c                   Z-ADD     *zeros        pos
050700010125     C                   EVAL      pos =  %SCAN(%TRIM(nmiti72):a40)
050800010125if  2C                   IF        pos <= *zeros                                *non trovato
050900010125     c                   MOVEL     'N'           $recok                         *record non valido
051000010125e   2c                   ENDIF
051100010125e   1c                   ENDIF
051200010125     c*
051300010125     c                   ENDSR
051400010125     c*--------------------------------------------------------------------------------------------*
051500010125     c* controlla la destinazione
051600010125     c*--------------------------------------------------------------------------------------------*
051700010125     c     chkvtades     BEGSR
051800010125     c*
051900010202if  1c                   IF        ndesi72 <> *blanks                           *se richiesto
052000010125     c                   EVAL      a40 = %TRIM(vtalna)
052100010125     c                   Z-ADD     *zeros        pos
052200010125     C                   EVAL      pos =  %SCAN(%TRIM(ndesi72):a40)
052300010125if  2C                   IF        pos <= *zeros                                *non trovato
052400010125     c                   MOVEL     'N'           $recok                         *record non valido
052500010125e   2c                   ENDIF
052600010125e   1c                   ENDIF
052700010125     c*
052800010125     c                   ENDSR
052900991108     c*--------------------------------------------------------------------------------------------*
053000991108     c* controlla lo stato consegna
053100991108     c*--------------------------------------------------------------------------------------------*
053200991108     c     chkvtacon     BEGSR
053300991108     c*
053400991108if  1c                   IF        vtasta = '1'                                 *consegnate
053500991108if  2c                   IF        ck1i72 = 'o' OR                              *chiesto CONSEGNATE
053600991108     c                             ck1i72 <> 'o' AND ck2i72 <> 'o'              *nessuna richiesta
053700991108x   2c                   ELSE                                                   *non chieste
053800991108     c                   MOVEL     'N'           $recok                         *record non valido
053900991108e   2c                   ENDIF
054000991108e   1c                   ENDIF
054100991108     c*
054200991108if  1c                   IF        vtasta = '2'                                 *non consegnate
054300991108if  2c                   IF        ck2i72 = 'o' OR                              *chiesto NON CONSEG
054400991108     c                             ck2i72 <> 'o' AND ck1i72 <> 'o'              *nessuna richiesta
054500991108x   2c                   ELSE                                                   *non chieste
054600991108     c                   MOVEL     'N'           $recok                         *record non valido
054700991108e   2c                   ENDIF
054800991108e   1c                   ENDIF
054900991108     c*
055000991108     c                   ENDSR
055100991108     c*--------------------------------------------------------------------------------------------*
055200991108     c* controlla il riferimento nel riferimento numerico o alfabetico o partner -CIRCA-
055300991108     c*--------------------------------------------------------------------------------------------*
055400991108     c     chkvtarifc    BEGSR
055500991108     c*
055600991108if  1c                   IF        rifi72 <> *blanks                            *se richiesto
055700991108     c* ALFABETICO
055800040920     c*                  EVAL      a40 = %TRIM(vtarma)
055900040920     c*                  Z-ADD     *zeros        pos
056000040920     c*                  EVAL      pos = %SCAN(%TRIM(cRifI72):a40)
056100040920if  2c*                  IF        pos <= *zeros                                *non trovato
056200040920     C                   IF        %LEN(%TRIM(VtaRMA)) < LenRif OR
056300040920     C                             %SUBST(cRifI72:1:LenRif) <>
056400040920     C                             %SUBST(%TRIM(VtaRMA):1:LenRif)
056500991108     c*
056600991108     c* ALFABETICO PARTNER
056700040920     c*                  EVAL      a40 = %TRIM(vtarpt)
056800040920     c*                  Z-ADD     *zeros        pos
056900040920     c*                  EVAL      pos = %SCAN(%TRIM(cRifI72):a40)
057000040920if  3c*                  IF        pos <= *zeros                                *non trovato
057100040920     C                   IF        %LEN(%TRIM(VtaRPt)) < LenRif OR
057200040920     C                             %SUBST(cRifI72:1:LenRif) <>
057300040920     C                             %SUBST(%TRIM(VtaRPt):1:LenRif)
057400991108     c* NUMERICO
057500040920     c*                  EVAL      a40 = %TRIM(%EDITC(vtarmn:'Z'))
057600040920     c*                  Z-ADD     *zeros        pos
057700040920     c*                  EVAL      pos = %SCAN(%TRIM(cRifI72):a40)
057800040920if  4c*                  IF        pos <= *zeros                                *non trovato
057900040920     C                   IF        VtaRMN = 0 OR nRifI72 = 0 OR
058000040920     C                             VtaRMN <> nRifI72
058100991108     c                   MOVEL     'N'           $recok                         *record non valido
058200991108e   4c                   ENDIF
058300991108e   3c                   ENDIF
058400991108e   2c                   ENDIF
058500991108e   1c                   ENDIF
058600991109     c*
058700991108     c                   ENDSR
058800991108     c*--------------------------------------------------------------------------------------------*
058900991108     c* controlla il riferimento nel riferimento numerico o alfabetico o partner -PRECISO-
059000000316     c*--------------------------------------------------------------------------------------------*
059100991108     c     chkvtarifp    BEGSR
059200991108     c*
059300991108if  1c                   IF        rifi72 <> *blanks                            *se richiesto
059400991108     c* ALFABETICO
059500180215     c*                  EVAL      a15 = vtarma
059501180215     c                   CLEAR                   a15
059502180215     c     minu:maiu     XLATE     vtaRma        a15
059600991108     c* ALFABETICO PARTNER
059700991109     c                   EVAL      a20 = vtarpt
059800991108     c* NUMERICO
059900991109     c                   MOVEL     vtarmn        a15bis
060000991109     c*
060100040920if  2c                   IF        %TRIM(a15)    = %TRIM(A15cRifI72) OR
060200040920     c                             %TRIM(a20)    = %TRIM(P20cRifI72) OR
060300040920     c                             a15bis = N15cRifI72
060400991108x   2c                   ELSE
060500991108     c                   MOVEL     'N'           $recok                         *record non valido
060600991108e   2c                   ENDIF
060700991108e   1c                   ENDIF
060800991108     c*
060900991108     c                   ENDSR
061000030806     c*--------------------------------------------------------------------------------------------*
061100030806     c* controlla il numero di spedizione
061200030806     c*--------------------------------------------------------------------------------------------*
061300030806     c     chkvtaspe     BEGSR
061400030806     c*
061500030806if  1c                   IF        nnspedizi72 > *zeros                         *se richiesto
061600030806if  2C                   IF        nnspedizi72 <> vtaspe
061700030806     c                   MOVEL     'N'           $recok                         *record non valido
061800030806e   2c                   ENDIF
061900030806e   1c                   ENDIF
062000030806     c*
062100030806     c                   ENDSR
062200090901     c*--------------------------------------------------------------------------------------------*
062300090901     c* Controlla il subunificante.
062400090901     c*--------------------------------------------------------------------------------------------*
062500090902     c     chkSubk       BEGSR
062600090902     c                   IF        tis7700o1.subkSun <> *BLANK AND
062700090902     c                             %LEN(vtaSubk) > *ZERO AND
062800090902     c                             vtaSubk <> *BLANK AND
062900090902     c                             tis7700o1.subkSun <> vtaSubk
063000090902     c                   EVAL      $recOk = 'N'
063100090902     c                   ENDIF
063200090901     c                   ENDSR
063300991108     c*--------------------------------------------------------------------------------------------*
063400991022     c* memorizza le spedizioni da paginare
063500991022     c*--------------------------------------------------------------------------------------------*
063600991108     c     memvta        BEGSR
063700991022     c*
063800991022     c* incrementa il n° di righe memorizzate
063900991022     c                   EVAL      RigheOut = RigheOut + 1
064000991022     c*
064100991022     c* imposta la memorizzazione di almeno una riga
064200991022     c                   EVAL      Esito = '0'
064300991025     c*
064400991025     c* incrementa il n° di bolle memorizzate
064500991025     c                   EVAL      contbol = contbol + 1
064600991022     c*
064700991022     c* memorizza la riga nella DS di Output
064800991022     c     RigheOut      OCCUR     tis173dso
064900991025     c                   Z-ADD     vtadsp        dsdat
065000991025     c                   MOVEL     dsgio         dsgioe
065100991025     c                   MOVEL     '/'           dsvd1
065200991025     c                   MOVEL     dsmes         dsmese
065300991025     c                   MOVEL     '/'           dsvd2
065400991025     c                   MOVEL     dsann         dsanne
065500991025     c                   EVAL      Dato73 = dsdate
065600110127     c                   EVAL      Rifo73 = %CHAR(vtaRmn)
065700110127     C                   IF        vtaRma <> *BLANK AND
065800110127     C                             %TRIM(vtaRma) <> %CHAR(vtaRmn)
065900110127     C                   EVAL      %SUBST(Rifo73:16) = vtaRma
066000110127     C                   ENDIF
066100110127     C                   IF        vtaRma = *BLANK AND vtaRpt <> *BLANK
066200110127     C                   EVAL      %SUBST(Rifo73:16) = vtaRpt
066300110127     C                   ENDIF
066400090901     c                   EVAL      Deso73 = vtalna + ' ' + vtaprd
066500000317     c                   EVAL      Dsto73 = vtarsd
066600991025     c                   EVAL      Speo73 = vtaspe
066700010125     c                   EVAL      Mito73 = vtarsm
066800991029if  1c                   IF        vtasta = '1'                                 *consegnata
066900060608     c*                  EVAL      Stao73 = ssta(1)
067000060612     C                   EVAL      Stao73 = rtvMsgLang('TIS0091':langI72)
067100060608x   1c                   ELSE                                                   *non consegnata
067200060608     c*                  EVAL      Stao73 = ssta(2)
067300060612     C                   EVAL      Stao73 = rtvMsgLang('TIS0103':langI72)
067400060608e   1c                   ENDIF
067500060608     c*
067600991022     c                   ENDSR
067700991112     c*--------------------------------------------------------------------------------------------*
067800991112     c* operazioni per nuova richiesta
067900991112     c*--------------------------------------------------------------------------------------------*
068000991112     c     newric        BEGSR
068100000316     c*
068200000316     c* imposta variabili di lavoro
068300000316     c                   EVAL      $letrif = 'N'                                *NO riferim PRECISO
068400000316     c                   EVAL      $letrmn = 'N'                                *NO rif.num PRECISO
068500000316     c                   EVAL      $rifrmnok = 'N'                              *NO rif.num = numeri
068600991112     c*
068700991112     c* normalizza i campi richiesti a video
068800991112     c                   EXSR      normricvid
068900090902     c*
069000090902     C                   RESET                   tis7700i1
069100090902     C                   EVAL      tis7700i1.ksu = kscI72
069200090902     C                   EVAL      tis7700i1.sun = %EDITC(rqsCidI72 : 'X')
069300090902     C                   CALLP     Selettore_bolla_subUnificante('GETSUBKSUN'
069400090902     C                             : rpyEsito
069500090902     C                             : 'TIS7700I1' : tis7700i1 : %SIZE(tis7700i1)
069600090902     C                             : 'TIS7700O1' : tis7700o1 : %SIZE(tis7700o1)
069700090902     C                             )
069800991112     c*
069900991112     c                   ENDSR
070000991021     c*--------------------------------------------------------------------------------------------*
070100991021     c* normalizza i campi richiesti a video
070200991021     c*--------------------------------------------------------------------------------------------*
070300991021     c     normricvid    BEGSR
070400991111     c*
070500991111     c* pulisce le variabili di lavoro
070600991111     c                   CLEAR                   ndsti72
070700010125     c                   CLEAR                   nmiti72
070800010125     c                   CLEAR                   ndesi72
070900991111     c                   CLEAR                   ndini72
071000991111     c                   CLEAR                   ndfii72
071100040920     c                   CLEAR                   cRifI72
071200040920     c                   CLEAR                   nRifI72
071300030806     c                   CLEAR                   nnspedizi72
071400040920     c                   CLEAR                   N15cRifI72
071500040920     c                   CLEAR                   A15cRifI72
071600040920     c                   CLEAR                   P20cRifI72
071700991029     c*
071800010125     c* destinatario, si considera pieno se ha almeno 2 caratteri
071900010202if  1c                   IF        %LEN(%TRIM(dsti72)) >= 2                     *almeno 2 caratteri
072000991029     c     minu:maiu     XLATE     dsti72        ndsti72                        *Minus -> Maiuscolo
072100010125x   1c                   ELSE                                                   *meno di 2 caratteri
072200010125     c                   CLEAR                   dsti72                         *non considera
072300010125e   1C                   ENDIF
072400010125     c*
072500010125     c* mittente, si considera pieno se ha almeno 2 caratteri
072600010202if  1c                   IF        %LEN(%TRIM(miti72)) >= 2                     *almeno 2 caratteri
072700010125     c     minu:maiu     XLATE     miti72        nmiti72                        *Minus -> Maiuscolo
072800010125x   1c                   ELSE                                                   *meno di 2 caratteri
072900010125     c                   CLEAR                   miti72                         *non considera
073000010125e   1C                   ENDIF
073100010125     c*
073200010125     c* destinazione, si considera pieno se ha almeno 2 caratteri
073300010202if  1c                   IF        %LEN(%TRIM(desi72)) >= 2                     *almeno 2 caratteri
073400010125     c     minu:maiu     XLATE     desi72        ndesi72                        *Minus -> Maiuscolo
073500010125x   1c                   ELSE                                                   *meno di 2 caratteri
073600010125     c                   CLEAR                   desi72                         *non considera
073700010125e   1C                   ENDIF
073800991022     c*
073900991105     c* data fine, se nesssna richiesta = data corrente
074000991105     c                   Z-ADD     datcor        ndfii72                        *nessuna richiesta
074100991029     c*
074200011128     c* controlla se la data fine è numerica
074300011128     c                   MOVEL     dfii72        dsdate
074400011128     c                   MOVEL     dsanne        dsanna
074500011128     c                   MOVEL     dsmese        dsmesa
074600011128     c                   MOVEL     dsgioe        dsgioa
074700011128     c                   MOVE      dsdata        a1                             *ultimo carattere
074800011128     c                   SETOFF                                       98
074900011128     c                   TESTN                   dsdata               98
075000011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
075100011128     c                             a1 >= '0'                                    *NON trovata lettera
075200011128     c                   MOVEL     dsanna        dsann
075300011128     c                   MOVEL     dsmesa        dsmes
075400011128     c                   MOVEL     dsgioa        dsgio
075500991025     c                   Z-ADD     dsdat         g08inv
075600991025     c                   Z-ADD     *zeros        g08dat
075700991025     C                   MOVEL     '3'           g08err
075800991025     c                   CALL      'XSRDA8'
075900991025     c                   PARM                    wlbda8
076000991025if  2c                   IF        g08err <> '1'                                *no errore
076100991025     c                   Z-ADD     g08inv        ndfii72
076200991025e   2c                   ENDIF
076300011128e   1c                   ENDIF
076400991022     c*
076500991105     c* data inizio, se nessuna richiesta = data corrente - 1 anno
076600991105     c                   Z-ADD     datcor        dsdat
076700991025     c                   EVAL      dsann = dsann - 1
076800991105     c                   Z-ADD     dsdat         ndini72                        *nessuna richiesta
076900991025     c*
077000011128     c* controlla se la data inizio è numerica
077100991025     c                   MOVEL     dini72        dsdate
077200011128     c                   MOVEL     dsanne        dsanna
077300011128     c                   MOVEL     dsmese        dsmesa
077400011128     c                   MOVEL     dsgioe        dsgioa
077500011128     c                   MOVE      dsdata        a1                             *ultimo carattere
077600011128     c                   SETOFF                                       98
077700011128     c                   TESTN                   dsdata               98
077800011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
077900011128     c                             a1 >= '0'                                    *NON trovata lettera
078000011128     c                   MOVEL     dsanna        dsann
078100011128     c                   MOVEL     dsmesa        dsmes
078200011128     c                   MOVEL     dsgioa        dsgio
078300991025     c                   Z-ADD     dsdat         g08inv
078400991025     c                   Z-ADD     *zeros        g08dat
078500991025     C                   MOVEL     '3'           g08err
078600991025     c                   CALL      'XSRDA8'
078700991025     c                   PARM                    wlbda8
078800991025if  2c                   IF        g08err <> '1'                                *no errore
078900991025     c                   Z-ADD     g08inv        ndini72
079000991025e   2c                   ENDIF
079100991025e   1c                   ENDIF
079200991025     c*
079300991025     c* data fine + data inizio, deve esserci congruenza
079400991025if  2c                   IF        ndini72 > ndfii72                            *incongruenza
079500991025     c                   Z-ADD     ndfii72       ndini72                        *data fine = inizio
079600991025e   1c                   ENDIF
079700991108     c*
079800991108     c* riferimenti
079900040920if  1c                   IF        RifI72 <> *blanks
080000040920     C
080100040920     c     minu:maiu     XLATE     rifi72        cRifI72                        *Minus -> Maiuscolo
080200040920     C                   EVAL      cRifI72 = %TRIML(cRifI72)
080300040920     C                   EVAL      LenRif = %LEN(%TRIMR(cRifI72))
080400040920     C
080500090901     C                   TESTN                   cRifI72                98
080600040920     C                   IF        *IN98
080700040920     C                   EVAL      nRifI72 = atof(cRifI72)
080800040920     C                   ENDIF
080900040920     c* rende il riferimento inserito controllabile con il riferimento -PRECISO-
081000991109     c* NUMERICO
081100040920     c                   EVAL      walfa15 = %TRIM(cRifI72)
081200991108     c                   EXSR      xallinea1                                    *allinea campo
081300090901     c                   EVAL      N15cRifI72 = %TRIMR(walfa15)                 *confronto -PRECISO-
081400000316     c                   SETOFF                                       98
081500040920     c                   TESTN                   N15cRifI72           98
081600000316if  1c                   IF        *in98                                        *da TESTN è numero
081700040920     c                   MOVE      N15cRifI72    a1                             *ultimo carattere
081800000405if  2c                   IF        a1 >= '0'                                    *NON trovata lettera
081900000316     c                   MOVEL     'S'           $rifrmnok                      *NO rif.num = numeri
082000000316e   2c                   ENDIF
082100000316e   1c                   ENDIF
082200991109     c* ALFANUMERICO
082300090901     c                   EVAL      A15cRifI72 = cRifI72                         *confronto -PRECISO-
082400991109     c* PARTNER
082500090901     c                   EVAL      P20cRifI72 = cRifI72                         *confronto -PRECISO-
082600991108e   1c                   ENDIF
082700030806     c*
082800030806     c* controlla se il numero di spedizione è numerico
082900030806     c                   MOVE      nspedizi72    a1                             *ultimo carattere
083000030806     c                   SETOFF                                       98
083100030806     c                   TESTN                   nspedizi72           98
083200030806if  1c                   IF        *in98  AND                                   *da TESTN è numero
083300030806     c                             a1 >= '0'                                    *NON trovata lettera
083400030806     c                   MOVEL     nspedizi72    nnspedizi72
083500030806     c                   ELSE
083600030806     c                   MOVEL     *zeros        nnspedizi72                    *non valido: annulla
083700030806     c                   CLEAR                   nspedizi72
083800030806e   1c                   ENDIF
083900030806     c*
084000991109     c                   ENDSR
084100991108     c*--------------------------------------------------------------------------------------------*
084200991108     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
084300991109     c* Input:   Walfa15 = Alfanumerico di 15
084400991109     c* Output:  Walfa15 = Alfanumerico di 15 allineato a Dx con zeri
084500991108     c*--------------------------------------------------------------------------------------------*
084600991108     c     xallinea1     BEGSR
084700991108     c*
084800991108     c* reimposta variabli di lavoro
084900991109     c                   CLEAR                   i
085000991109     c                   CLEAR                   z
085100991109     c                   CLEAR                   walfa15bis
085200991108     c                   EVAL      wzeri = *ALL'0'
085300991108     c*
085400991109     c     ' '           CHECKR    walfa15:15    i
085500991108if  1c                   IF        i <> 0
085600991109     c     i             SUBST     walfa15       walfa15bis
085700991108e   1c                   ENDIF
085800991109if  1c                   IF        walfa15bis <>  *blanks
085900991109     c     15            SUB       i             z
086000991108if  2c                   IF        z <> 0
086100991109     c                   eval      %subst(walfa15:1:z) = %subst(wzeri:1:z)
086200991108e   2c                   ENDIF
086300991108     c                   EVAL      z = z+ 1
086400991109     c                   EVAL      %subst(walfa15:z:i) = %subst(walfa15bis:1:i)
086500991108e   1c                   ENDIF
086600991108     c*
086700991111     c                   ENDSR
086800991111     c*--------------------------------------------------------------------------------------------*
086900991111     c* imposta i depositi della chiave di lettura per la prima lettura
087000991111     c*--------------------------------------------------------------------------------------------*
087100991111     c     firstlet      BEGSR
087200991111     c*
087300991111     c* variabili di lavoro
087400991111     c                   EVAL      $fvta = 'N'                                  *no fine file
087500991111     c                   EVAL      contbol = 0                                  *contatore bolle
087600991111     c* chiave
087700991111     c                   MOVEL     ksci72        depksu                         *deposito unificante
087800991111     c                   Z-ADD     ndini72       depdsp                         *data inizio
087900000316if  1c                   IF        $rifrmnok = 'S'                              *SI rif.num = numeri
088000040920     c                   MOVEL     N15cRifI72    deprmn                         *rif.mitt.numerico
088100000316x   1c                   ELSE                                                   *NO rif.num = numeri
088200000316     c                   Z-ADD     *zeros        deprmn                         *rif.mitt.numerico
088300000316e   1c                   ENDIF
088400991111     c                   Z-ADD     *zeros        deplnp
088500991111     c                   Z-ADD     *zeros        depnrs
088600991111     c                   Z-ADD     *zeros        depnsp
088700991111     c                   MOVEL     *blanks       deptbl
088800991111     c*
088900991111     c                   ENDSR
089000991028     c*--------------------------------------------------------------------------------------------*
089100991028     c* operazioni iniziali -da fare sempre-
089200991028     c*--------------------------------------------------------------------------------------------*
089300991111     c     rutinz        BEGSR
089400991216     c*
089500991216     c* reperimento data corrente
089600991216     C                   TIME                    n14                            *ora (6)+ data (8)
089700991216     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
089800991216     C                   MOVE      n14           n8                             *data (8) in g/m/a
089900991216     C                   Z-ADD     n8            g08dat
090000991216     C                   Z-ADD     *zeros        g08inv
090100991216     C                   MOVEL     '0'           g08err
090200991216     c                   CALL      'XSRDA8'
090300991216     c                   PARM                    wlbda8
090400991216     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
090500991028     c*
090600991028     c* imposta variabli di lavoro
090700991216     c                   EVAL      RigheOut =  *zeros
090800991028     c*
090900991028     c                   ENDSR
091000991020     c*--------------------------------------------------------------------------------------------*
091100991028     c* impostazioni iniziali -la prima volta-
091200991020     c*--------------------------------------------------------------------------------------------*
091300991028     c     *inzsr        BEGSR
091400991020     c*
091500991020     c* ricevimento parametri
091600991021     c     *ENTRY        PLIST
091700991021     c                   PARM                    TIS172DSI
091800991021     c                   PARM                    Operazione
091900991021     c                   PARM                    RigheRic
092000991021     c                   PARM                    TIS173DSO
092100991021     c                   PARM                    RigheOut
092200991021     c                   PARM                    Esito
092300991020     c*
092400991020     c* chiavi di lettura
092500090901     c     keyvta        KLIST                                                  tivta12l
092600991105     c                   KFLD                    ktaksu                         -cliente unificante
092700991021     c                   KFLD                    ktadsp                         -data spedizione
092800991021     c                   KFLD                    ktalnp                         -linea partenza
092900991021     c                   KFLD                    ktanrs                         -serie
093000991021     c                   KFLD                    ktansp                         -spedizione
093100991021     c                   KFLD                    ktatbl                         -tipo bolla
093200090901     c     ke2vta        KLIST                                                  tivta15l
093300000316     c                   KFLD                    ktaksu                         -cliente unificante
093400000316     c                   KFLD                    ktarmn                         -rifer.numerico
093500000316     c                   KFLD                    ktadsp                         -data spedizione
093600000316     c                   KFLD                    ktalnp                         -linea partenza
093700000316     c                   KFLD                    ktanrs                         -serie
093800000316     c                   KFLD                    ktansp                         -spedizione
093900000316     c                   KFLD                    ktatbl                         -tipo bolla
094000090901     c     ke3vta        KLIST                                                  tivta15l
094100000316     c                   KFLD                    ktaksu                         -cliente unificante
094200000316     c                   KFLD                    ktarmn                         -rifer.numerico
094300991217     c*
094400991217     c* agiunge le librerie in lista
094500060608     c*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
094600060608     c*                  CALL      wpgm
094700991217     c*
094800991217     c* apre i files
094900090901     c                   OPEN      tivta12l
095000090901     c                   OPEN      tivta15l
095100991020     c*
095200991108     c                   ENDSR
095300991110     o*--------------------------------------------------------------------------------------------*
095400060608
095500991110** ssta (15) - Stato bolla
095600991110Consegnata     1
095700991110Da consegnare  2
0958009911103
0959009911104
0960009911105
0961009911106
0962009911107
0963009911108
0964009911109
09650099111010
