000100001005     h*--------------------------------------------------------------------------------------------*
000200001005     h* Visualizza bolle da selezionare
000300001005     h*--------------------------------------------------------------------------------------------*
000400001005      /TITLE Carica righe spedizione trovate
000500060613     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS')
000600060613     Hdatedit(*DMY)
000700001005     f*--------------------------------------------------------------------------------------------*
000800001005     f* Data base
000900001005     f*--------------------------------------------------------------------------------------------*
001000001005     f*---
001100001005     f* Estratto bolle di sede
001200001005     f*---
001300090903     ftivta12l  IF   E           K DISK    USROPN
001400001005     f*---
001500001005     f* Bolle di sede
001600001005     f*---
001700001005     Ftitas30c  if   e           k disk    USROPN IGNORE(titasp00)
001800011005     f*---
001900011005     f* Bolle di sede - Riferimenti
002000011005     f*---
002100011005     ftita430c  IF   E           K DISK    USROPN
002200021108     f*---
002300021108     f* Giacenze
002400021108     f*---
002500050321     ftigcp51l  IF   E           K DISK    USROPN
002600050211     f*---
002700050211     f* Eventi
002800050211     f*---
002900050211     ffnevb04l  IF   E           K DISK    USROPN
003000050211     f*---
003100050211     f* Tabelle
003200050211     f*---
003300050211     Ftabel00f  if   e           k disk    usropn
003400001005     d*--------------------------------------------------------------------------------------------*
003500001005     d* Data structure
003600001005     d*--------------------------------------------------------------------------------------------*
003700001005     d*---
003800001005     d* Costanti
003900001005     d*---
004000001005     d minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004100001005     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004200060613     d*---
004300060613     d* Procedure
004400060613     d*---
004500060613     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
004600060613     D  msgId                         7A   CONST
004700060613     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
004800060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
004900060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
005000060613     D                                     VARYING
005100060613     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
005200060613     D                                     VARYING
005300060613     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
005400060613     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
005500060613     d*---
005600001005     d* Schiere a tempo di compilazione
005700001005     d*---
005800050211     d sdsta           S             15    DIM(8) CTDATA PERRCD(1)              *Stato bolla
005900001006     d*---
006000001006     d* Schiere di memorizzazione
006100001006     d*---
006200001006     d* 20 righe per pagina
006300001006     d SmRif           S             30    DIM(20)                              *riferimento
006400001006     d SmDat           S             10    DIM(20)                              *data spedizione
006500001006     d SmDes           S             30    DIM(20)                              *destinazione
006600001006     d SmDst           S             30    DIM(20)                              *Destinatario
006700001006     d SmSpe           S             12    DIM(20)                              *n° spedizione
006800001006     d SmSta           S             15    DIM(20)                              *stato
006900001005     d*---
007000001005     d* Variabili riferite al data base
007100001005     d*---
007200090903     d ktaksu          S                   LIKE(vtaksu)                         *lettura tivta12l
007300001005     d ktadsp          S                   LIKE(vtadsp)
007400001005     d ktalnp          S                   LIKE(vtalnp)
007500001005     d ktanrs          S                   LIKE(vtanrs)
007600001005     d ktansp          S                   LIKE(vtansp)
007700001005     d ktatbl          S                   LIKE(vtatbl)
007800001005     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
007900001005     d kaslnp          S                   LIKE(taslnp)
008000001005     d kasnrs          S                   LIKE(tasnrs)
008100001005     d kasnsp          S                   LIKE(tasnsp)
008200001005     d kastbl          S                   LIKE(tastbl)
008300011005     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
008400011005     d ka4lnp          S                   LIKE(ta4lnp)
008500011005     d ka4nrs          S                   LIKE(ta4nrs)
008600011005     d ka4nsp          S                   LIKE(ta4nsp)
008700011005     d ka4trc          S                   LIKE(ta4trc)
008800050321     d kcpaas          S                   LIKE(gcpaas)                         *lettura tigcp00f
008900021108     d kcplnp          S                   LIKE(gcplnp)
009000021108     d kcpnrs          S                   LIKE(gcpnrs)
009100021108     d kcpnsp          S                   LIKE(gcpnsp)
009200021108     d kcpfrg          S                   LIKE(gcpfrg)
009300050211     d kevaas          S                   LIKE(evbaas)                         *lettura fnevb00f
009400050211     d kevlnp          S                   LIKE(evblnp)
009500050211     d kevnrs          S                   LIKE(evbnrs)
009600050211     d kevnsp          S                   LIKE(evbnsp)
009700050211     d kevcev          S                   LIKE(evbcev)
009800050211     D tute            S                   LIKE(tblkut) inz(1)
009900050211     D tcod            S                   LIKE(tblcod)
010000001005     d depksu          S                   LIKE(vtaksu)                         *depositi
010100001005     d depdsp          S                   LIKE(vtadsp)
010200001005     d deplnp          S                   LIKE(vtalnp)
010300001005     d depnrs          S                   LIKE(vtanrs)
010400001005     d depnsp          S                   LIKE(vtansp)
010500001005     d deptbl          S                   LIKE(vtatbl)
010600001005     d NInpDin         S                   LIKE(vtaDsp)                          *campi"normalizzati
010700001005     d NInpDfi         S                   LIKE(vtaDsp)
010800001005     d NInpPrd         S                   LIKE(InpPrd)
010900001005     d NInpLod         S                   LIKE(InpLod)
011000001005     d NInpMit         S                   LIKE(InpMit)
011100001005     d NInpDst         S                   LIKE(InpDst)
011200001005     d*---
011300001005     d* Variabili
011400001005     d*---
011500001005     d $fvta           S              1    INZ('N')                             *fine lettura
011600001005     d $recok          S              1    INZ('N')                             *validità record
011700001005     d $stato          S              1                                         *stati bolla
011800010926     d $statoOK        S              1                                         *stato bolla OK
011900011005     d $notgia         S              1    INZ('N')                             *note "giacenza"
012000050321     d $gcp            S              1    INZ('N')                             *giacenza in TIGCP
012100050211     d $parz           S              1    INZ('N')                             *cons.parziale
012200050211     d $lav            S              1    INZ('N')                             *l.avviso in FNEVB
012300001005     d i               S              5  0                                      *indici
012400001006     d r               S              5  0
012500001006     d z               S              5  0
012600011213     d a35             S             35                                         *Alfabetico 35
012700001005     d a14             S             14                                         *alfabetico 14
012800001005     d n14             S             14  0                                      *Numerico 14
012900001005     d n8              S              8  0                                      *Numerico 8
013000001005     d n4              S              4  0                                      *Numerico 4
013100011128     d a1              S              1                                         *Alfabetico 1
013200001005     d pos             S              5  0                                      *posizione SCAN
013300001005     d datcor          S              8  0                                      *data corrente
013400001005     d oracor          S              6  0                                      *ora corrente
013500001005     d wpgm            S             20                                         *programma
013600001005     d Operazione      S              1
013700001005     D pardaia         s              8    inz(*blanks)
013800001005     D pardafa         s              8    inz(*blanks)
013900001005     D wrkore          s             20  0 inz(*zeros)
014000001005     D wrkdata         s               d   DATFMT(*ISO) INZ(D'1999-01-01')
014100080903     D sk2Alav         S              3    DIM(200)                             *eventi L/A
014200001005     d*---
014300001005     d* Ds
014400001005     d*---
014500011128      * Scomposizione data editata
014600011128     d                 DS                  INZ
014700011128     d  dsgioe                 1      2                                          -giorno
014800011128     d  dsvd1                  3      3                                          -'/'
014900011128     d  dsmese                 4      5                                          -mese
015000011128     d  dsvd2                  6      6                                          -'/'
015100011128     d  dsanne                 7     10                                          -anno
015200011128     d dsdate                  1     10                                         *data editata
015300011128      * Composizione data NON editata -alfabetica-
015400011128     d                 DS                  INZ
015500011128     d  dsgioa                 1      2                                          -giorno
015600011128     d  dsmesa                 3      4                                          -mese
015700011128     d  dsanna                 5      8                                          -anno
015800011128     d dsdata                  1      8                                         *data NON editata
015900011128      * scomposizione data generica
016000011128     d                 DS                  INZ
016100011128     d  dsann                  1      4  0                                       -anno
016200011128     d  dsmes                  5      6  0                                       -mese
016300011128     d  dsgio                  7      8  0                                       -giorno
016400011128     d dsdat                   1      8  0                                      *data
016500001005      * DS controlla data
016600001005     d wlbda8          DS                  INZ                                  *controlla data
016700001005     d  g08dat                        8  0                                       -data dritta
016800001005     d  g08inv                        8  0                                       -data invertita
016900001005     d  g08err                        1                                          -errore
017000001005     d  g08tgi                        5  0                                       -giorni fra date
017100001005      * composizione spedizione
017200001005     d                 DS                  INZ
017300001005     d  vtalnp                 1      3  0                                       -linea partenza
017400001005     d  vtanrs                 4      5  0                                       -serie
017500001005     d  vtansp                 6     12  0                                       -spedizione
017600001005     d vtaspe                  1     12                                         *codice spedizione
017700001005      * DS contenente i dati dI INPUT
017800001005     d TIS1W2IDS     E DS                  prefix(Inp)
017900001005     d* Input
018000001005     d RigheOut        S              5s 0
018100001005     d RigheRic        S              5s 0
018200001005     d Esito           S              1
018300001005     d* DS ritornata dal programma di accesso ai dati -> contiene una riga di dati
018400001006     d*TIS1W2PDS     E DS                  OCCURS(20) PREFIX(Pag)
018500001006     d TIS1W2PDS     E DS                  PREFIX(Pag)
018600050211     D*---
018700050211     D* DS per scomposizione tabella codici eventi (2A)
018800050211     D*---
018900050211     D DS2A          e ds                  inz
019000001005     c*--------------------------------------------------------------------------------------------*
019100001005     c* Main lines
019200001005     c*--------------------------------------------------------------------------------------------*
019300001005     c*--- --- --- --- --- --- ---
019400001005     c* Operazione
019500001005     c* '1' = Inizio nuova richiesta
019600001005     c* '2' = Dati trovati, richiesta successiva
019700001005     c* '9' = Non trovati dati
019800001005     c*--- --- --- --- --- --- ---
019900001005     c*
020000001005     c* operazioni iniziali -da fare sempre-
020100001005     c                   EXSR      rutinz
020200001005     c*
020300001005     c* Se operazione = '1' esegue prime letture
020400001005if  1c                   IF        Operazione = '1'
020500001005     c*
020600001005     c* operazioni per nuova richiesta
020700001005     c                   EXSR      newric
020800001005     c*
020900001005     c* imposta i depositi della chiave di lettura per la prima lettura
021000001005     c                   EXSR      firstlet
021100001005     c*
021200001005     c* elabora il file TIVTA -estratto bolle-
021300001005     c                   EXSR      elavta
021400001005e   1c                   ENDIF
021500001005     c*
021600001005     c* Se operazione = '2' esegue letture successive
021700001005if  1c                   IF        Operazione = '2'
021800001005     c*
021900001005     c* a fine file -giro precedente- imposta Operazione = '9'
022000001005if  2c                   IF        $fvta = 'S'                                  *fine file
022100001005     c                   EVAL      Operazione = '9'
022200001005     c                   EVAL      RigheOut =  *zeros
022300001005x   2c                   ELSE                                                   *no fine file
022400001005     c*
022500001005     c* elabora il file TIVTA -estratto bolle-
022600001005     c                   EXSR      elavta
022700001005e   2c                   ENDIF
022800001005e   1c                   ENDIF
022900001005     c*
023000001005     c                   RETURN
023100001005     c*--------------------------------------------------------------------------------------------*
023200001005     c* elabora il file TIVTA -estratto bolle-
023300001005     c*--------------------------------------------------------------------------------------------*
023400001005     c     elavta        BEGSR
023500001005     c*
023600001005     c* posizionamento su TIVTA -estratto bolle-
023700001005     c                   EXSR      setvta
023800001005     c*
023900001005     c* carica la prima pagina di 20 righe
024000001005do  1c                   DOW       RigheOut < RigheRic AND                      *riempimento pagina
024100001005     c                             $fvta = 'N'                                  *fine file
024200001005     c*
024300001005     c* lettura di TIVTA -estratto bolle-
024400001005     c                   EXSR      letvta
024500001005     c*
024600001005     c* memorizza le spedizioni da paginare, se corrette
024700001005if  2c                   IF        $recok = 'S'  AND
024800001005     c                             $fvta = 'N'
024900001005     c                   EXSR      MemVta
025000001005e   2c                   ENDIF
025100001005e   1c                   ENDDO
025200001005     c*
025300001006     c* porta le schiere di memorizzazione nella DS di Output
025400001006     c                   MOVEA     SmRif         PagRif
025500001006     c                   MOVEA     SmDat         PagDat
025600001006     c                   MOVEA     SmDes         PagDes
025700001006     c                   MOVEA     SmDst         PagDst
025800001006     c                   MOVEA     SmSpe         PagSpe
025900001006     c                   MOVEA     SmSta         PagSta
026000001006     c*
026100001005     c                   ENDSR
026200001005     c*--------------------------------------------------------------------------------------------*
026300001005     c* posizionamento su TIVTA -estratto bolle-
026400001005     c*--------------------------------------------------------------------------------------------*
026500001005     c     setvta        BEGSR
026600001005     c*
026700001005     c* posizionamento dall'ultimo record letto
026800001005     c* . se 1^ volta la chiave è impostata con cliente unificante + data inizio + zeri
026900001005     c* . se n^ volte la chiave è impostata con dati ultima bolla letta
027000001005     c                   MOVEL     depksu        ktaksu                         *deposito unificante
027100001005     c                   Z-ADD     depdsp        ktadsp                         *depositi chiave
027200001005     c                   Z-ADD     deplnp        ktalnp
027300001005     c                   Z-ADD     depnrs        ktanrs
027400001005     c                   Z-ADD     depnsp        ktansp
027500001005     c                   MOVEL     deptbl        ktatbl
027600090903     c     keyvta        SETGT     tivta12l                           99
027700001005if  1c                   IF        *in99                                        *fine file
027800001005     c                   MOVEL     'S'           $fvta                          *fine lettura
027900001005     c                   EVAL      Operazione = '9'
028000001005e   1c                   ENDIF
028100001005     c*
028200001005     c                   ENDSR
028300001005     c*--------------------------------------------------------------------------------------------*
028400001005     c* lettura TIVTA -estratto bolle-
028500001005     c*--------------------------------------------------------------------------------------------*
028600001005     c     letvta        BEGSR
028700001005     c*
028800090903     c     ktaksu        READE     tivta12l                               99
028900001005if  1c                   IF        NOT *in99                                    *record trovato
029000001005     c                   EXSR      chkvta                                       *controlla record
029100001005x   1c                   ELSE                                                   *record non trovato
029200001005     c                   MOVEL     'S'           $fvta                          *fine lettura
029300001005     c                   EVAL      Operazione = '9'
029400001005e   1c                   ENDIF
029500001005     c*
029600001005     c                   ENDSR
029700001005     c*--------------------------------------------------------------------------------------------*
029800001005     c* controlla record di TIVTA -estratto bolle-
029900001005     c*--------------------------------------------------------------------------------------------*
030000001005     c     chkvta        BEGSR
030100001005     c*
030200001005     c                   MOVEL     'S'           $recok                         *record valido
030300001005     c*
030400001005     c* se le date non sono nel range richiesto, esclude
030500001005if  1c                   IF        $recok = 'S'                                 *ancora valido
030600001005     c                   EXSR      ChkVtaDsp
030700001005e   1c                   ENDIF
030800001005     c*
030900001005     c* se il punto operativo di arrivo è <> da quello richiesto, esclude
031000001005if  1c                   IF        $recok = 'S'                                 *ancora valido
031100001005     c                   EXSR      ChkVtaLna
031200001005e   1c                   ENDIF
031300001005     c*
031400001005     c* se la provincia di arrivo è <> da quello richiesto, esclude
031500001005if  1c                   IF        $recok = 'S'                                 *ancora valido
031600001005     c                   EXSR      ChkVtaPrd
031700001005e   1c                   ENDIF
031800001005     c*
031900001005     c* se la località di arrivo è <> da quello richiesto, esclude
032000001005if  1c                   IF        $recok = 'S'                                 *ancora valido
032100001005     c                   EXSR      ChkVtaLod
032200001005e   1c                   ENDIF
032300001005     c*
032400001005     c* se il mittente è <> da quello richiesto, esclude
032500001005if  1c                   IF        $recok = 'S'                                 *ancora valido
032600001005     c                   EXSR      ChkVtaMit
032700001005e   1c                   ENDIF
032800001005     c*
032900001005     c* se il destinatario è <> da quello richiesto, esclude
033000001005if  1c                   IF        $recok = 'S'                                 *ancora valido
033100001005     c                   EXSR      ChkVtaDst
033200001005e   1c                   ENDIF
033300001005     c*
033400001005     c* legge la bolla
033500001005if  1c                   IF        $recok = 'S'                                 *ancora valido
033600001005     c                   EXSR      LetTas
033700001005e   1c                   ENDIF
033800001005     c*
033900010926     c* se lo stato di consegna / NON consegna è <> da quello richiesto, esclude
034000001005if  1c                   IF        $recok = 'S'                                 *ancora valido
034100050211     c                   EXSR      ChkTas
034200001005e   1c                   ENDIF
034300001005     c*
034400001005     c* memorizza in un deposito l'ultimo record letto
034500001005     c                   MOVEL     vtaksu        depksu                         *depositi
034600001005     c                   Z-ADD     vtadsp        depdsp
034700001005     c                   Z-ADD     vtalnp        deplnp
034800001005     c                   Z-ADD     vtanrs        depnrs
034900001005     c                   Z-ADD     vtansp        depnsp
035000001005     c                   MOVEL     vtatbl        deptbl
035100001005     c*
035200001005     c                   ENDSR
035300001005     c*--------------------------------------------------------------------------------------------*
035400001005     c* controlla le date
035500001005     c*--------------------------------------------------------------------------------------------*
035600001005     c     ChkVtaDsp     BEGSR
035700001005     c*
035800001005if  1c                   IF        vtadsp >= NInpDin AND
035900001005     c                             vtadsp <= NInpDfi
036000001005x   1c                   ELSE
036100001005     c                   MOVEL     'N'           $recok                         *record non valido
036200001005e   1c                   ENDIF
036300001005     c*
036400001005     c                   ENDSR
036500001005     c*--------------------------------------------------------------------------------------------*
036600001005     c* controlla il punto operativo
036700001005     c*--------------------------------------------------------------------------------------------*
036800001005     c     ChkVtaLna     BEGSR
036900001005     C*
037000090325if  1c                   IF        InpLna > *zeros and InpLna < 999
037100001005if  2c                   IF        vtapoa <> InpLna
037200001005     c                   MOVEL     'N'           $recok                         *record NON valido
037300001005e   2c                   ENDIF
037400001005e   1c                   ENDIF
037500001005     c*
037600001005     c                   ENDSR
037700001005     c*--------------------------------------------------------------------------------------------*
037800001005     c* controlla la provincia di arrivo
037900001005     c*--------------------------------------------------------------------------------------------*
038000001005     c     ChkVtaPrd     BEGSR
038100001005     c*
038200001005     C*
038300001005     C* Controllo se richiesta una particolare provincia di arrivo
038400001005if  1C                   IF        %TRIM(NInpPrd) > *blanks
038500001005if  2C                   IF        vtaprd <> NInpPrd
038600001005     c                   MOVEL     'N'           $recok                         *record NON valido
038700001005e   2c                   ENDIF
038800001005e   1c                   ENDIF
038900001005     C*
039000001005     c                   ENDSR
039100001005     c*--------------------------------------------------------------------------------------------*
039200001005     c* controlla la località di arrivo
039300001005     c*--------------------------------------------------------------------------------------------*
039400001005     c     ChkVtaLod     BEGSR
039500001005     c*
039600001005if  1C                   IF        %TRIM(NInpLod) > *blanks
039700011213     C                   EVAL      a35 = %TRIM(vtalna)
039800011213     C                   EVAL      pos = %SCAN(%TRIM(NInpLod):a35)
039900001005if  2C                   IF        pos <= *zeros
040000001005     c                   MOVEL     'N'           $recok                         *record NON valido
040100001005e   2c                   ENDIF
040200001005e   1c                   ENDIF
040300001005     C*
040400001005     c                   ENDSR
040500001005     c*--------------------------------------------------------------------------------------------*
040600001005     c* controlla il mittente
040700001005     c*--------------------------------------------------------------------------------------------*
040800001005     c     ChkVtaMit     BEGSR
040900001005     c*
041000001005if  1C                   IF        %TRIM(NInpMit) > *blanks
041100011213     C                   EVAL      a35 = %TRIM(vtarsm)
041200011213     C                   EVAL      pos = %SCAN(%TRIM(NInpMit):a35)
041300001005if  2C                   IF        pos <= *zeros
041400001005     c                   MOVEL     'N'           $recok                         *record NON valido
041500001005e   2c                   ENDIF
041600001005e   1c                   ENDIF
041700001005     c*
041800001005     c                   ENDSR
041900001005     C*--------------------------------------------------------------------------------------------*
042000001005     c* controlla il destinatario
042100001005     C*--------------------------------------------------------------------------------------------*
042200001005     c     ChkVtaDst     BEGSR
042300001005     c*
042400001005if  1C                   IF        %TRIM(NInpDst) > *blanks
042500011213     C                   EVAL      a35 = %TRIM(vtarsd)
042600011213     C                   EVAL      pos = %SCAN(%TRIM(NInpDst):a35)
042700001005if  2C                   IF        pos <= *zeros
042800001005     c                   MOVEL     'N'           $recok                         *record NON valido
042900001005e   2c                   ENDIF
043000001005e   1c                   ENDIF
043100001005     c*
043200001005     c                   ENDSR
043300001005     c*--------------------------------------------------------------------------------------------*
043400001005     c* lettura TITAS -bolle sede-
043500001005     c*--------------------------------------------------------------------------------------------*
043600001005     c     LetTas        BEGSR
043700001005     c*
043800001005     c                   MOVEL     vtadsp        kasaas                         *chiave bolla
043900001005     c                   Z-ADD     vtalnp        kaslnp
044000001005     c                   Z-ADD     vtanrs        kasnrs
044100001005     c                   Z-ADD     vtansp        kasnsp
044200001005     c                   MOVEL     vtatbl        kastbl
044300001005     c     keytas        CHAIN     titas30c
044400001005if  1c                   IF        NOT %FOUND                                   *non trovato
044500001005     c                   MOVEL     'N'           $recok                         *record NON valido
044600080606     c                   ELSE
044700080606     ** Tipo servizio.
044800080606     C                   IF        inpServizio_C <> tasTsp
044900080606     C                             AND
045000080606     C                             inpServizio_D <> tasTsp
045100080606     C                             AND
045200080606     C                             inpServizio_E <> tasTsp
045300100113     C                             AND
045400100113     C                             inpServizio_H <> tasTsp
045500080606     C                   EVAL      $recOK = 'N'
045600080606     C                   ENDIF
045700001005e   1c                   ENDIF
045800001005     c*
045900001005     c                   ENDSR
046000050211     c*--------------------------------------------------------------------------------------------*
046100050211     c* reperisce la giacenza
046200050211     c*--------------------------------------------------------------------------------------------*
046300050211     c     repgcp        BEGSR
046400050211     c*
046500050211     c* imposta le variabili di lavoro
046600050321     c                   EVAL      $gcp = 'N'                                   NO giacenza in TIGCP
046700050211     c*
046800050211     c* legge la giacenza
046900050211     c                   Z-ADD     tasaas        kcpaas
047000050211     c                   Z-ADD     taslnp        kcplnp
047100050211     c                   Z-ADD     tasnrs        kcpnrs
047200050211     c                   Z-ADD     tasnsp        kcpnsp
047300050211     c                   Z-ADD     *zeros        kcpfrg
047400050321     c     keygcp        CHAIN     tigcp51l                           99
047500050211if  1c                   IF        NOT *in99        AND                         *esistente
047600050211     c                             gcpatb = *blanks AND                         *no annullata
047700050211     c                             gcptcm <> 'Z'
047800050211     c                   MOVEL     'S'           $gcp
047900050211e   1c                   ENDIF
048000050211     c*
048100050211     c                   ENDSR
048200050211     c*--------------------------------------------------------------------------------------------*
048300050211     c* reperisce se la spedizone ha/avuto un evento d consegna parziale
048400050211     c*--------------------------------------------------------------------------------------------*
048500050211     c     repparz       BEGSR
048600050211     c*
048700050211     c* imposta le variabili di lavoro
048800050211     c                   EVAL      $parz = 'N'                                  NO evento Cons.Parz.
048900050211     c*
049000050211     c* legge la giacenza
049100050211     c                   Z-ADD     tasaas        kevaas
049200050211     c                   Z-ADD     taslnp        kevlnp
049300050211     c                   Z-ADD     tasnrs        kevnrs
049400050211     c                   Z-ADD     tasnsp        kevnsp
049500050211     c                   MOVEL(P)  'P'           kevcev
049600050211     c     keyevb2       SETLL     fnevb04l                               99
049700050211     c                   IF        %equal(fnevb04l)
049800050211     c     keyevb2       READE     fnevb04l
049900050211     c                   DOW       not %eof(fnevb04l)
050000050211     c                   IF        evbatb = *blanks                             *no annullata
050100050211     c                   EVAL      $parz= 'S'                                   evento d cons.parz.
050200050211     c                   LEAVE
050300050211     c                   ENDIF
050400050211     c     keyevb2       READE     fnevb04l
050500050211     c                   ENDDO
050600050211     c                   ENDIF
050700050211     c*
050800050211     c                   ENDSR
050900050211     c*--------------------------------------------------------------------------------------------*
051000050211     c* reperisce se ultimo evento è di tipo lasciato avviso
051100050211     c*--------------------------------------------------------------------------------------------*
051200050211     c     replav        BEGSR
051300050211     c*
051400050211     c* imposta le variabili di lavoro
051500050211     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
051600050211     c*
051700050211     c* legge gli eventi
051800050211     c                   Z-ADD     tasaas        kevaas
051900050211     c                   Z-ADD     taslnp        kevlnp
052000050211     c                   Z-ADD     tasnrs        kevnrs
052100050211     c                   Z-ADD     tasnsp        kevnsp
052200050211     c     keyevb        SETLL     fnevb04l                               99
052300050211     c                   IF        %equal(fnevb04l)
052400050211     c     keyevb        READE     fnevb04l
052500050211     c                   DOW       not %eof(fnevb04l)
052600050211     c                   IF        evbatb = *blanks                             *no annullata
052700050211     c     evbCEV        LOOKUP    sk2Alav                                20
052800050211     c                   IF        *IN20 = *ON
052900050211     c                   EVAL      $lav = 'S'                                   evento d tipo L/A
053000050211     c                   ELSE
053100050211     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
053200050211     c                   ENDIF
053300050211     c                   ENDIF
053400050211     c     keyevb        READE     fnevb04l
053500050211     c                   ENDDO
053600050211     c                   ENDIF
053700050211     c*
053800050211     c                   ENDSR
053900050211     C*--------------------------------------------------------------------------------------------*
054000050211     C* ESEGUO CHECK X VERIFICARE SE CONSIDERARE O MENO IL RECORD CORRENTE SU TITAS
054100050211     C*--------------------------------------------------------------------------------------------*
054200050211     c     ChkTas        BEGSR
054300050211     c*
054400050211     c* Reimposta le variabili di lavoro
054500050211     c                   EVAL      $statoOK = 'N'                               *stato NON ok
054600050211     c                   EVAL      $notgia  = 'N'                               *note "giacenza"
054700050211     C*
054800050211     C* Consegnate - NORMALI
054900050211     C                   if        tasdcm > *zeros AND
055000050211     c                             tascca  = *blanks                            *SENZA anomalie
055100050211     C                   if        Inpckc <> *blanks
055200050211     c                   EVAL      $statoOK = 'S'                               *stato OK
055300050211     C                   endif
055400050211     C                   endif
055500050211     c*
055600050211     C* consegnate - ANOMALE
055700050211     C                   if        tasdcm > *zeros AND
055800050211     c                             tascca  <> *blanks                           *CON   anomalie
055900050211     C                   if        Inpckm <> *blanks
056000050211     c                   EVAL      $statoOK = 'S'                               *stato OK
056100050211     C                   endif
056200050211     C                   endif
056300050211     c*
056400050211     c* reperisce evenutale GIACENZA
056500050211     c                   EXSR      repgcp
056600050211     c*
056700050211     c* reperisce motivi della NON consegna per GIACENZA
056800050211     c                   Z-ADD     tasaas        ka4aas
056900050211     c                   Z-ADD     taslnp        ka4lnp
057000050211     c                   Z-ADD     tasnrs        ka4nrs
057100050211     c                   Z-ADD     tasnsp        ka4nsp
057200050211     c     ke2ta4        SETLL     tita430c
057300050211     c     ke2ta4        READE     tita430c                               98
057400050211     c                   DOW       NOT *in98
057500050211     c                   IF        ta4trc = 'G' OR                              *Giacenza
057600050211     c                             ta4trc = 'H'
057700050211     c                   EVAL      $notgia = 'S'                                *stato GIACIENTE
057800050211     c                   ENDIF
057900050211     c     ke2ta4        READE     tita430c                               98
058000050211     c                   ENDDO
058100050211     C*
058200050211     C* Non consenate - Giacenti
058300050211     C                   IF        tasdcm = *zeros   AND
058400050211     C                             (tasfgc<>*blanks OR $notgia='S' OR $gcp='S')
058500050211     C                   if        Inpckg <> *blanks
058600050211     c                   EVAL      $statoOK = 'S'                               *stato OK
058700050211     C                   endif
058800050211     C                   endif
058900050211     C*
059000050309     C* Non consenate  - Non giacenti
059100050211     C                   IF        tasdcm = *zeros AND
059200050211     C                             (tasfgc=*blank AND $notgia='N' AND $gcp='N')
059300050211     C*
059400050211     C* reperisce evenutale CONSEGNA PARZIALE
059500050211     C                   EXSR      repparz
059600050211     C                   if        Inpckp <> *blanks
059700050211     C                   EVAL      $statoOK = 'S'                               *stato OK
059800050211     C                   endif
059900050211     C*
060000050211     C* verifica evenutale APPUNTAMENTO
060100050211     C                   if        (tasftc = 'A' OR tastc2 = 'A') and
060200050211     C                             Inpckt <> *blanks
060300050211     C                   EVAL      $statoOK = 'S'                               *stato OK
060400050211     C                   endif
060500050211     C*
060600050211     C* reperisce evenutale LASCIATO AVVISO
060700050211     C                   EXSR      replav
060800050211     C                   if        Inpckl <> *blanks
060900050211     C                   EVAL      $statoOK = 'S'                               *stato OK
061000050211     C                   endif
061100050309     C*
061200050309     C* se scelte ALTRE record cmq sempre da considerare
061300050309     C                   if        Inpcka <> *blanks
061400050309     C                   EVAL      $statoOK = 'S'                               *stato OK
061500050309     C                   endif
061600050211     C*
061700050211     C                   endif
061800050211     c*
061900050211     c                   if        $statoOK <> 'S'
062000050211     c                   MOVEL     'N'           $recok                         *record NON valido
062100050211     C                   else
062200050211     C                   exsr      ChkVtaCkx
062300050211     C                   endif
062400050211     C*
062500050211     C                   ENDSR
062600050211     C*--------------------------------------------------------------------------------------------*
062700050211     C* Verifica gli "stati" bolla relativamente ai flag richiesti nel lancio
062800050211     C*--------------------------------------------------------------------------------------------*
062900050211     C     ChkVtaCkx     BEGSR
063000050211     c*
063100050211     c                   MOVEL     'N'           $recok                         *record NON valido
063200050211     c                   CLEAR                   $stato
063300050211     C*---
063400050211     C* Consegnate
063500050211     C*---
063600050211     C                   if        tasdcm > *zeros
063700050211     c* SENZA anomalia
063800050310     C                   if        tascca = *blanks
063900050310     C                   if        InpCkc <> *blanks
064000050211     c                   MOVEL     'S'           $recok
064100050211     c                   EVAL      $stato = 'C'                                 *stato CONSEGNATA
064200050310     C                   endif
064300050211     c* CON anomalia
064400050211     C                   else
064500050310     C                   if        tascca <> *blanks
064600050310     C                   if        InpCkm <> *blanks
064700050211     c                   MOVEL     'S'           $recok
064800050211     c                   EVAL      $stato = 'M'                                 *stato CONS ANOMALA
064900050310     C                   endif
065000050211     C                   endif
065100050211     C                   endif
065200050211     C*---
065300050211     C* Non consegnate
065400050211     C*---
065500050211     C                   else
065600050211     C                   if        (tasfgc=*blank AND $notgia='N' AND $gcp='N')
065700050211     C* - Consegna parziale
065800050310     C                   if        ($parz = 'S')
065900050310     C                   if        InpCkp <> *blanks
066000050211     c                   MOVEL     'S'           $recok
066100050211     c                   EVAL      $stato = 'P'                                 *stato CONS.PARZ.
066200050310     C                   endif
066300050211     C                   else
066400050211     C* - Appuntamento
066500050310     C                   if        (tasftc = 'A' OR tastc2 = 'A')
066600050310     C                   if        InpCkt <> *blanks
066700050211     c                   MOVEL     'S'           $recok
066800050211     c                   EVAL      $stato = 'T'                                 *stato APPUNTAMENTO
066900050310     C                   endif
067000050211     C                   else
067100050211     C* - Lasciato avviso
067200050310     C                   if        ($lav = 'S')
067300050310     C                   if        InpCkl <> *blanks
067400050211     c                   MOVEL     'S'           $recok
067500050211     c                   EVAL      $stato = 'V'                                 *stato LASC./AVV.
067600050310     C                   endif
067700050211     C                   else
067800050211     C* - Altre
067900050211     C                   if        InpCka <> *blanks
068000050211     c                   MOVEL     'S'           $recok
068100050211     c                   EVAL      $stato = 'A'                                 *stato ALTRE
068200050211     C                   endif
068300050211     C                   endif
068400050211     C                   endif
068500050211     C                   endif
068600050211     C* Giacenti
068700050211     C                   else
068800050211     C                   if        InpCkg <> *blanks
068900050211     c                   MOVEL     'S'           $recok
069000050211     C                   endif
069100050211     C                   endif
069200050211     C                   endif
069300050211     C*
069400050211     C                   ENDSR
069500001006     c*--------------------------------------------------------------------------------------------*
069600001006     c* memorizza le spedizioni da paginare nelle schiere
069700001006     c*--------------------------------------------------------------------------------------------*
069800001006     c     MemVta        BEGSR
069900001006     c*
070000001006     c* incrementa il n° di righe memorizzate
070100001006     c                   EVAL      RigheOut = RigheOut + 1
070200001006     c                   EVAL      r = Righeout                                 *solo + corto
070300001006     c*
070400001006     c* imposta la memorizzazione di almeno una riga
070500001006     c                   EVAL      Esito = '0'
070600001006     c*
070700001006     c* memorizza la riga nella DS di Output
070800001006     c                   MOVEL     vtadsp        a14
070900001006     c                   EVAL      SmDat(r) = %SUBST(a14:7:2) +
071000001006     c                                        '/' +
071100001006     c                                        %SUBST(a14:5:2) +
071200001006     c                                        '/' +
071300001006     c                                        %SUBST(a14:1:4)
071301110211    >***************************************************************************<
071400110211    >C*                  EVAL      SmRif(r) = %TRIM(%EDITC(vtarmn:'Z')) +       <
071500110211    >C*                                       ' ' +                             <
071600110211    >C*                                       %TRIM(vtarma) +                   <
071700110211    >C*                                       ' ' +                             <
071800110211    >C*                                       %TRIM(vtarpt)                     <
071801110211    >C                   EVAL      smRif(r) = %CHAR(vtaRmn)                     <
071802110211    >C                   IF        vtaRma <> *BLANK AND                         <
071803110211    >C                             %CHAR(vtaRmn) <> %TRIM(vtaRma)               <
071804110211    >C                   EVAL      %SUBST(smRif(r):16) = vtaRma                 <
071805110211    >C                   ELSE                                                   <
071806110211    >C                   EVAL      %SUBST(smRif(r):16) = vtaRpt                 <
071807110211    >C                   ENDIF                                                  <
071808110211    >***************************************************************************<
071900090903     c                   EVAL      SmDes(r)  = vtalna + ' ' + vtaprd
072000001006     c                   EVAL      SmDst(r)  = vtarsd
072100001006     c                   EVAL      SmSpe(r)  = vtaspe
072200001006if  1c                   IF        $stato    = 'C'                               *consegnata
072300060613     c*                  EVAL      SmSta(r)  = sdsta(1)
072400060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0091':inpLang)
072500001006e   1c                   ENDIF
072600011030if  1c                   IF        $stato    = 'M'                               *cons.con anomalia
072700060613     c*                  EVAL      SmSta(r)  = sdsta(2)
072800060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0083':inpLang)
072900011030e   1c                   ENDIF
073000050211if  1c                   IF        $stato    = 'A'                               *nn cons. (altre)
073100060613     c*                  EVAL      SmSta(r)  = sdsta(3)
073200060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0103':inpLang)
073300001006e   1c                   ENDIF
073400050211if  1c                   IF        $stato    = 'G'                               *giacente
073500060613     c*                  EVAL      SmSta(r)  = sdsta(4)
073600060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0193':inpLang)
073700001006e   1c                   ENDIF
073800050211if  1c                   IF        $stato    = 'P'                               *cons. parziale
073900060613     c*                  EVAL      SmSta(r)  = sdsta(3)
074000060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0103':inpLang)
074100050211e   1c                   ENDIF
074200050211if  1c                   IF        $stato    = 'T'                               *appuntamento
074300060613     c*                  EVAL      SmSta(r)  = sdsta(3)
074400060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0103':inpLang)
074500050211e   1c                   ENDIF
074600050211if  1c                   IF        $stato    = 'V'                               *lasciato avviso
074700060613     c*                  EVAL      SmSta(r)  = sdsta(3)
074800060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0103':inpLang)
074900050211e   1c                   ENDIF
075000001006     c*
075100001006     c                   ENDSR
075200001005     c*--------------------------------------------------------------------------------------------*
075300001005     c* operazioni per nuova richiesta
075400001005     c*--------------------------------------------------------------------------------------------*
075500001005     c     newric        BEGSR
075600001005     c*
075700001005     c* normalizza i campi in Input
075800001005     c                   EXSR      NormInput
075900001005     c*
076000001005     c                   ENDSR
076100001005     c*--------------------------------------------------------------------------------------------*
076200001005     c* normalizza i campi in Input
076300001005     c*--------------------------------------------------------------------------------------------*
076400001005     c     NormInput     BEGSR
076500001005     c*
076600001005     c* pulisce le variabili di lavoro
076700001005     c                   CLEAR                   NInpDin
076800001005     c                   CLEAR                   NInpDfi
076900001005     c                   CLEAR                   NInpPrd
077000001005     c                   CLEAR                   NInpLod
077100001005     c                   CLEAR                   NInpMit
077200001005     c                   CLEAR                   NInpDst
077300011128     c*
077400011128     C* data fine, se non inserita = data corrente
077500011128     C                   Z-ADD     datcor        NInpDfi
077600011128     c*
077700011128     c* controlla se la data fine è numerica
077800011128     c                   MOVEL     InpDfi        dsdate
077900011128     c                   MOVEL     dsanne        dsanna
078000011128     c                   MOVEL     dsmese        dsmesa
078100011128     c                   MOVEL     dsgioe        dsgioa
078200011128     c                   MOVE      dsdata        a1                             *ultimo carattere
078300011128     c                   SETOFF                                       98
078400011128     c                   TESTN                   dsdata               98
078500011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
078600011128     c                             a1 >= '0'                                    *NON trovata lettera
078700011128     c                   MOVEL     dsanna        dsann
078800011128     c                   MOVEL     dsmesa        dsmes
078900011128     c                   MOVEL     dsgioa        dsgio
079000011128     c                   Z-ADD     dsdat         g08inv
079100011128     c                   Z-ADD     *zeros        g08dat
079200011128     C                   MOVEL     '3'           g08err
079300011128     c                   CALL      'XSRDA8'
079400011128     c                   PARM                    wlbda8
079500011128if  2c                   IF        g08err <> '1'                                *no errore
079600011128     c                   Z-ADD     g08inv        NInpDfi
079700011128e   2c                   ENDIF
079800011128e   1c                   ENDIF
079900011128     c*
080000011128     c* data inizio, se nessuna richiesta = data corrente - 1 anno
080100011128     c                   Z-ADD     datcor        dsdat
080200011128     c                   EVAL      dsann = dsann - 1
080300011128     c                   Z-ADD     dsdat         NInpDin                        *nessuna richiesta
080400011128     c*
080500011128     c* controlla se la data inizio è numerica
080600011128     c                   MOVEL     InpDin        dsdate
080700011128     c                   MOVEL     dsanne        dsanna
080800011128     c                   MOVEL     dsmese        dsmesa
080900011128     c                   MOVEL     dsgioe        dsgioa
081000011128     c                   MOVE      dsdata        a1                             *ultimo carattere
081100011128     c                   SETOFF                                       98
081200011128     c                   TESTN                   dsdata               98
081300011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
081400011128     c                             a1 >= '0'                                    *NON trovata lettera
081500011128     c                   MOVEL     dsanna        dsann
081600011128     c                   MOVEL     dsmesa        dsmes
081700011128     c                   MOVEL     dsgioa        dsgio
081800011128     c                   Z-ADD     dsdat         g08inv
081900011128     c                   Z-ADD     *zeros        g08dat
082000011128     C                   MOVEL     '3'           g08err
082100011128     c                   CALL      'XSRDA8'
082200011128     c                   PARM                    wlbda8
082300011128if  2c                   IF        g08err <> '1'                                *no errore
082400011128     c                   Z-ADD     g08inv        NInpDin
082500011128e   2c                   ENDIF
082600011128e   1c                   ENDIF
082700001005     C*
082800001005     C* provincia di arrivo
082900001005     C     minu:maiu     XLATE     InpPrd        NInpPrd                        *Minus -> Maiuscolo
083000001005     c*
083100001005     c* località di arrivo
083200001005     c     minu:maiu     XLATE     InpLod        NInpLod                        *Minus -> Maiuscolo
083300001005     c*
083400001005     c* mittente
083500001005     c     minu:maiu     XLATE     InpMit        NInpMit                        *Minus -> Maiuscolo
083600001005     c*
083700001005     c* destinatario
083800001005     c     minu:maiu     XLATE     InpDst        NInpDst                        *Minus -> Maiuscolo
083900001005     c*
084000001005     c                   ENDSR
084100001005     c*--------------------------------------------------------------------------------------------*
084200001005     c* imposta i depositi della chiave di lettura per la prima lettura
084300001005     c*--------------------------------------------------------------------------------------------*
084400001005     c     firstlet      BEGSR
084500001005     c*
084600001005     c* variabili di lavoro
084700001005     c                   EVAL      $fvta = 'N'                                  *no fine file
084800001005     c* chiave
084900001005     c                   MOVEL     InpKsc        depksu                         *deposito unificante
085000001005     c                   Z-ADD     NInpDin       depdsp                         *data inizio
085100001005     c                   Z-ADD     *zeros        deplnp
085200001005     c                   Z-ADD     *zeros        depnrs
085300001005     c                   Z-ADD     *zeros        depnsp
085400001005     c                   MOVEL     *blanks       deptbl
085500001005     c*
085600001005     c                   ENDSR
085700001005     c*--------------------------------------------------------------------------------------------*
085800001005     c* operazioni iniziali -da fare sempre-
085900001005     c*--------------------------------------------------------------------------------------------*
086000001005     c     rutinz        BEGSR
086100001005     c*
086200001005     c* reperimento data corrente
086300001005     C                   TIME                    n14                            *ora (6)+ data (8)
086400001005     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
086500001005     C                   MOVE      n14           n8                             *data (8) in g/m/a
086600001005     C                   Z-ADD     n8            g08dat
086700001005     C                   Z-ADD     *zeros        g08inv
086800001005     C                   MOVEL     '0'           g08err
086900001005     c                   CALL      'XSRDA8'
087000001005     c                   PARM                    wlbda8
087100001005     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
087200001005     c*
087300001005     c* imposta variabli di lavoro
087400001005     c                   EVAL      RigheOut =  *zeros
087500001006     c*
087600001006     c* azzera schiere di memorizzazione
087700001006     c                   CLEAR                   SmRif
087800001006     c                   CLEAR                   SmDat
087900001006     c                   CLEAR                   SmDes
088000001006     c                   CLEAR                   SmDst
088100001006     c                   CLEAR                   SmSpe
088200001006     c                   CLEAR                   SmSta
088300050211     C*
088400050211     C* carica le tabelle occorrenti
088500050211     C                   exsr      cartab
088600001005     c*
088700001005     c                   ENDSR
088800050211     C*--------------------------------------------------------------------------------------------*
088900050211     C* CARICA LE TABELLE OCCORRENTI
089000050211     C*--------------------------------------------------------------------------------------------*
089100050211     C     cartab        BEGSR
089200050211     c*---
089300050211     c* Eventi d tipo "lasciato avviso"
089400050211     c*---
089500050211     C                   clear                   sk2Alav
089600050211     C                   z-add     *zeros        j                 3 0
089700050211     C                   eval      tute = 1
089800050211     C                   eval      tcod = '2A'
089900050211     C     ke2tab        setll     tabel00f
090000050211     C     ke2tab        reade     tabel00f
090100050211     C                   dow       not %eof(tabel00f)
090200050211     C                   movel(p)  tblUNI        ds2a
090300050211     C                   if        §2aFTC = 'A'
090400050211     C                   add       1             j
090500050211     C                   eval      sk2Alav(j) = tblKEY
090600050211     C                   endif
090700050211     C     ke2tab        reade     tabel00f
090800050211     C                   enddo
090900050211     C*
091000050211     c                   ENDSR
091100001005     c*--------------------------------------------------------------------------------------------*
091200001005     c* impostazioni iniziali -la prima volta-
091300001005     c*--------------------------------------------------------------------------------------------*
091400001005     c     *inzsr        BEGSR
091500001005     c*
091600001005     c* ricevimento parametri
091700001005     c     *ENTRY        PLIST
091800001005     c                   PARM                    TIS1W2IDS
091900001005     c                   PARM                    Operazione
092000001005     c                   PARM                    RigheRic
092100001005     c                   PARM                    TIS1W2PDS
092200001005     c                   PARM                    RigheOut
092300001005     c                   PARM                    Esito
092400001005     c*
092500001005     c* chiavi di lettura
092600090903     c     keyvta        KLIST                                                  tivta12l
092700001005     c                   KFLD                    ktaksu                         -cliente unificante
092800001005     c                   KFLD                    ktadsp                         -data spedizione
092900001005     c                   KFLD                    ktalnp                         -linea partenza
093000001005     c                   KFLD                    ktanrs                         -serie
093100001005     c                   KFLD                    ktansp                         -spedizione
093200001005     c                   KFLD                    ktatbl                         -tipo bolla
093300001005     c     keytas        KLIST                                                  titas30c
093400001005     c                   KFLD                    kasaas                         -anno spedizione
093500001005     c                   KFLD                    kaslnp                         -linea partenza
093600001005     c                   KFLD                    kasnrs                         -serie
093700001005     c                   KFLD                    kasnsp                         -spedizione
093800001005     c                   KFLD                    kastbl                         -tipo bolla
093900011005     c     ke2ta4        KLIST                                                  tita430c
094000011005     c                   KFLD                    ka4aas                         -anno spedizione
094100011005     c                   KFLD                    ka4lnp                         -linea partenza
094200011005     c                   KFLD                    ka4nrs                         -serie
094300011005     c                   KFLD                    ka4nsp                         -spedizione
094400050321     c     keygcp        KLIST                                                  *tigcp51l
094500021108     c                   KFLD                    kcpaas                         -anno spedizione
094600021108     c                   KFLD                    kcplnp                         -linea partenza
094700021108     c                   KFLD                    kcpnrs                         -serie
094800021108     c                   KFLD                    kcpnsp                         -spedizione
094900021108     c                   KFLD                    kcpfrg                         -prg aperture
095000050211     c     keyevb        KLIST                                                  *fnevb04l
095100050211     c                   KFLD                    kevaas                         -anno spedizione
095200050211     c                   KFLD                    kevlnp                         -linea partenza
095300050211     c                   KFLD                    kevnrs                         -serie
095400050211     c                   KFLD                    kevnsp                         -spedizione
095500050211     c     keyevb2       KLIST                                                  *fnevb04l
095600050211     c                   KFLD                    kevaas                         -anno spedizione
095700050211     c                   KFLD                    kevlnp                         -linea partenza
095800050211     c                   KFLD                    kevnrs                         -serie
095900050211     c                   KFLD                    kevnsp                         -spedizione
096000050211     c                   KFLD                    kevcev                         -codice evento
096100050211     C     ke2tab        KLIST
096200050211     C                   KFLD                    tute
096300050211     C                   KFLD                    tcod
096400001005     c*
096500001005     c* agiunge le librerie in lista
096600060613     c*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
096700060613     c*                  CALL      wpgm
096800001005     c*
096900001005     c* apre i files
097000090903     c                   OPEN      tivta12l
097100001005     c                   OPEN      titas30c
097200011005     c                   OPEN      tita430c
097300050321     c                   OPEN      tigcp51l
097400050211     c                   OPEN      fnevb04l
097500050211     c                   OPEN      tabel00f
097600001005     c*
097700001005     c                   ENDSR
097800001005     o*--------------------------------------------------------------------------------------------*
097900001006** sdsta (15) - Stato bolla
098000001005Consegnata     1
098100011030Cons.con anom. 2
098200011030Da consegnare  3
098300021108Giacente       4
098400050211Cons. parziale 5
098500050211Appuntamento   6
098600050211Lasciato avviso7
0987000502118
