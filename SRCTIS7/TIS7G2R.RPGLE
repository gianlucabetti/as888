000100001005     h*--------------------------------------------------------------------------------------------*
000200030213     h* T&T - Gestione giacenze: visualizza bolle da selezionare
000300001005     h*--------------------------------------------------------------------------------------------*
000400060613     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS')
000500001005     Hdatedit(*DMY)
000600001005     f*--------------------------------------------------------------------------------------------*
000700001005     f* Data base
000800001005     f*--------------------------------------------------------------------------------------------*
000900001005     f*---
001000001005     f* Estratto bolle di sede
001100001005     f*---
001200090902     ftivta12l  IF   E           K DISK    USROPN
001300001005     f*---
001400001005     f* Bolle di sede
001500001005     f*---
001600001005     Ftitas30c  if   e           k disk    USROPN IGNORE(titasp00)
001700021108     f*---
001800021108     f* Giacenze
001900021108     f*---
002000050309     ftigcp51l  IF   E           K DISK    USROPN
002100030213     fazdst01l  IF   E           K DISK    USROPN
002200001005     d*--------------------------------------------------------------------------------------------*
002300001005     d* Data structure
002400001005     d*--------------------------------------------------------------------------------------------*
002500001005     d*---
002600001005     d* Costanti
002700001005     d*---
002800001005     d minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
002900001005     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003000060613     d*---
003100060613     d* Procedure
003200060613     d*---
003300060613     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
003400060613     D  msgId                         7A   CONST
003500060613     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003600060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003700060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
003800060613     D                                     VARYING
003900060613     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
004000060613     D                                     VARYING
004100060613     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
004200060613     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
004300090902     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
004400090902     d*---
004500001005     d* Schiere a tempo di compilazione
004600001005     d*---
004700050419     d sdsta           S             15    DIM(5) CTDATA PERRCD(1)              Fase giacenza
004800001006     d*---
004900001006     d* Schiere di memorizzazione
005000001006     d*---
005100001006     d* 20 righe per pagina
005200001006     d SmRif           S             30    DIM(20)                              *riferimento
005300001006     d SmDat           S             10    DIM(20)                              *data spedizione
005400001006     d SmDes           S             30    DIM(20)                              *destinazione
005500001006     d SmDst           S             30    DIM(20)                              *Destinatario
005600001006     d SmSpe           S             12    DIM(20)                              *n° spedizione
005700001006     d SmSta           S             15    DIM(20)                              *stato
005800001005     d*---
005900001005     d* Variabili riferite al data base
006000001005     d*---
006100090902     d ktaksu          S                   LIKE(vtaksu)                         *lettura tivta12l
006200001005     d ktadsp          S                   LIKE(vtadsp)
006300001005     d ktalnp          S                   LIKE(vtalnp)
006400001005     d ktanrs          S                   LIKE(vtanrs)
006500001005     d ktansp          S                   LIKE(vtansp)
006600001005     d ktatbl          S                   LIKE(vtatbl)
006700001005     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
006800001005     d kaslnp          S                   LIKE(taslnp)
006900001005     d kasnrs          S                   LIKE(tasnrs)
007000001005     d kasnsp          S                   LIKE(tasnsp)
007100001005     d kastbl          S                   LIKE(tastbl)
007200021108     d kcpaas          S                   LIKE(gcpaas)                         *lettura figcp00f
007300021108     d kcplnp          S                   LIKE(gcplnp)
007400021108     d kcpnrs          S                   LIKE(gcpnrs)
007500021108     d kcpnsp          S                   LIKE(gcpnsp)
007600021108     d kcpfrg          S                   LIKE(gcpfrg)
007700001005     d depksu          S                   LIKE(vtaksu)                         *depositi
007800001005     d depdsp          S                   LIKE(vtadsp)
007900001005     d deplnp          S                   LIKE(vtalnp)
008000001005     d depnrs          S                   LIKE(vtanrs)
008100001005     d depnsp          S                   LIKE(vtansp)
008200001005     d deptbl          S                   LIKE(vtatbl)
008300001005     d NInpDin         S                   LIKE(vtaDsp)                          *campi"normalizzati
008400001005     d NInpDfi         S                   LIKE(vtaDsp)
008500030213     d NInpRif         S                   LIKE(InpRif)
008600001005     d NInpPrd         S                   LIKE(InpPrd)
008700001005     d NInpLod         S                   LIKE(InpLod)
008800001005     d NInpMit         S                   LIKE(InpMit)
008900001005     d NInpDst         S                   LIKE(InpDst)
009000050420     d nInpDtApe1      S                   LIKE(gcpdur)
009100050420     d nInpDtApe2      S                   LIKE(gcpdur)
009200050420     d nInpDtChi1      S                   LIKE(gcpdcg)
009300050420     d nInpDtChi2      S                   LIKE(gcpdcg)
009400001005     d*---
009500001005     d* Variabili
009600001005     d*---
009700001005     d $fvta           S              1    INZ('N')                             *fine lettura
009800001005     d $recok          S              1    INZ('N')                             *validità record
009900001005     d $stato          S              1                                         *stati bolla
010000001005     d i               S              5  0                                      *indici
010100001006     d r               S              5  0
010200001006     d z               S              5  0
010300011213     d a35             S             35                                         *Alfabetico 35
010400030213     d a40             S             40    INZ                                  *Alfanumerico 40
010500001005     d a14             S             14                                         *alfabetico 14
010600001005     d n14             S             14  0                                      *Numerico 14
010700001005     d n8              S              8  0                                      *Numerico 8
010800001005     d n4              S              4  0                                      *Numerico 4
010900011128     d a1              S              1                                         *Alfabetico 1
011000001005     d pos             S              5  0                                      *posizione SCAN
011100001005     d datcor          S              8  0                                      *data corrente
011200001005     d oracor          S              6  0                                      *ora corrente
011300001005     d wpgm            S             20                                         *programma
011400001005     d Operazione      S              1
011500001005     D pardaia         s              8    inz(*blanks)
011600001005     D pardafa         s              8    inz(*blanks)
011700001005     D wrkore          s             20  0 inz(*zeros)
011800001005     D wrkdata         s               d   DATFMT(*ISO) INZ(D'1999-01-01')
011900050420     D WrkDtIso        S               D
012000090902     D rpyEsito        S             10I 0
012100001005     d*---
012200001005     d* Ds
012300001005     d*---
012400011128      * Scomposizione data editata
012500011128     d                 DS                  INZ
012600011128     d  dsgioe                 1      2                                          -giorno
012700011128     d  dsvd1                  3      3                                          -'/'
012800011128     d  dsmese                 4      5                                          -mese
012900011128     d  dsvd2                  6      6                                          -'/'
013000011128     d  dsanne                 7     10                                          -anno
013100011128     d dsdate                  1     10                                         *data editata
013200011128      * Composizione data NON editata -alfabetica-
013300011128     d                 DS                  INZ
013400011128     d  dsgioa                 1      2                                          -giorno
013500011128     d  dsmesa                 3      4                                          -mese
013600011128     d  dsanna                 5      8                                          -anno
013700011128     d dsdata                  1      8                                         *data NON editata
013800011128      * scomposizione data generica
013900011128     d                 DS                  INZ
014000011128     d  dsann                  1      4  0                                       -anno
014100011128     d  dsmes                  5      6  0                                       -mese
014200011128     d  dsgio                  7      8  0                                       -giorno
014300011128     d dsdat                   1      8  0                                      *data
014400001005      * DS controlla data
014500001005     d wlbda8          DS                  INZ                                  *controlla data
014600001005     d  g08dat                        8  0                                       -data dritta
014700001005     d  g08inv                        8  0                                       -data invertita
014800001005     d  g08err                        1                                          -errore
014900001005     d  g08tgi                        5  0                                       -giorni fra date
015000001005      * composizione spedizione
015100001005     d                 DS                  INZ
015200001005     d  vtalnp                 1      3  0                                       -linea partenza
015300001005     d  vtanrs                 4      5  0                                       -serie
015400001005     d  vtansp                 6     12  0                                       -spedizione
015500001005     d vtaspe                  1     12                                         *codice spedizione
015600001005      * DS contenente i dati dI INPUT
015700030213     d TIS1G2IDS     E DS                  prefix(Inp)
015800001005     d* Input
015900001005     d RigheOut        S              5s 0
016000001005     d RigheRic        S              5s 0
016100001005     d Esito           S              1
016200001005     d* DS ritornata dal programma di accesso ai dati -> contiene una riga di dati
016300030213     d TIS1G2PDS     E DS                  PREFIX(Pag)
016400090902     D tis7700i1     E DS                  QUALIFIED
016500090902     D                                     INZ(*EXTDFT)
016600090902     D tis7700o1     E DS                  QUALIFIED
016700090902     D                                     INZ(*EXTDFT)
016800090902
016900090902     c*--------------------------------------------------------------------------------------------*
017000001005     c* Main lines
017100001005     c*--------------------------------------------------------------------------------------------*
017200001005     c*--- --- --- --- --- --- ---
017300001005     c* Operazione
017400001005     c* '1' = Inizio nuova richiesta
017500001005     c* '2' = Dati trovati, richiesta successiva
017600001005     c* '9' = Non trovati dati
017700001005     c*--- --- --- --- --- --- ---
017800001005     c*
017900001005     c* operazioni iniziali -da fare sempre-
018000001005     c                   EXSR      rutinz
018100001005     c*
018200001005     c* Se operazione = '1' esegue prime letture
018300001005if  1c                   IF        Operazione = '1'
018400001005     c*
018500001005     c* operazioni per nuova richiesta
018600001005     c                   EXSR      newric
018700001005     c*
018800001005     c* imposta i depositi della chiave di lettura per la prima lettura
018900001005     c                   EXSR      firstlet
019000001005     c*
019100001005     c* elabora il file TIVTA -estratto bolle-
019200001005     c                   EXSR      elavta
019300001005e   1c                   ENDIF
019400001005     c*
019500001005     c* Se operazione = '2' esegue letture successive
019600001005if  1c                   IF        Operazione = '2'
019700001005     c*
019800001005     c* a fine file -giro precedente- imposta Operazione = '9'
019900001005if  2c                   IF        $fvta = 'S'                                  *fine file
020000001005     c                   EVAL      Operazione = '9'
020100001005     c                   EVAL      RigheOut =  *zeros
020200001005x   2c                   ELSE                                                   *no fine file
020300001005     c*
020400001005     c* elabora il file TIVTA -estratto bolle-
020500001005     c                   EXSR      elavta
020600001005e   2c                   ENDIF
020700001005e   1c                   ENDIF
020800001005     c*
020900001005     c                   RETURN
021000001005     c*--------------------------------------------------------------------------------------------*
021100001005     c* elabora il file TIVTA -estratto bolle-
021200001005     c*--------------------------------------------------------------------------------------------*
021300001005     c     elavta        BEGSR
021400001005     c*
021500001005     c* posizionamento su TIVTA -estratto bolle-
021600001005     c                   EXSR      setvta
021700001005     c*
021800001005     c* carica la prima pagina di 20 righe
021900001005do  1c                   DOW       RigheOut < RigheRic AND                      *riempimento pagina
022000001005     c                             $fvta = 'N'                                  *fine file
022100001005     c*
022200001005     c* lettura di TIVTA -estratto bolle-
022300001005     c                   EXSR      letvta
022400001005     c*
022500001005     c* memorizza le spedizioni da paginare, se corrette
022600001005if  2c                   IF        $recok = 'S'  AND
022700001005     c                             $fvta = 'N'
022800001005     c                   EXSR      MemVta
022900001005e   2c                   ENDIF
023000001005e   1c                   ENDDO
023100001005     c*
023200001006     c* porta le schiere di memorizzazione nella DS di Output
023300001006     c                   MOVEA     SmRif         PagRif
023400001006     c                   MOVEA     SmDat         PagDat
023500001006     c                   MOVEA     SmDes         PagDes
023600001006     c                   MOVEA     SmDst         PagDst
023700001006     c                   MOVEA     SmSpe         PagSpe
023800001006     c                   MOVEA     SmSta         PagSta
023900001006     c*
024000001005     c                   ENDSR
024100001005     c*--------------------------------------------------------------------------------------------*
024200001005     c* posizionamento su TIVTA -estratto bolle-
024300001005     c*--------------------------------------------------------------------------------------------*
024400001005     c     setvta        BEGSR
024500001005     c*
024600001005     c* posizionamento dall'ultimo record letto
024700001005     c* . se 1^ volta la chiave è impostata con cliente unificante + data inizio + zeri
024800001005     c* . se n^ volte la chiave è impostata con dati ultima bolla letta
024900001005     c                   MOVEL     depksu        ktaksu                         *deposito unificante
025000001005     c                   Z-ADD     depdsp        ktadsp                         *depositi chiave
025100001005     c                   Z-ADD     deplnp        ktalnp
025200001005     c                   Z-ADD     depnrs        ktanrs
025300001005     c                   Z-ADD     depnsp        ktansp
025400001005     c                   MOVEL     deptbl        ktatbl
025500090902     c     keyvta        SETGT     tivta12l                           99
025600001005if  1c                   IF        *in99                                        *fine file
025700001005     c                   MOVEL     'S'           $fvta                          *fine lettura
025800001005     c                   EVAL      Operazione = '9'
025900001005e   1c                   ENDIF
026000001005     c*
026100001005     c                   ENDSR
026200001005     c*--------------------------------------------------------------------------------------------*
026300001005     c* lettura TIVTA -estratto bolle-
026400001005     c*--------------------------------------------------------------------------------------------*
026500001005     c     letvta        BEGSR
026600001005     c*
026700090902     c     ktaksu        READE     tivta12l                               99
026800001005if  1c                   IF        NOT *in99                                    *record trovato
026900001005     c                   EXSR      chkvta                                       *controlla record
027000001005x   1c                   ELSE                                                   *record non trovato
027100001005     c                   MOVEL     'S'           $fvta                          *fine lettura
027200001005     c                   EVAL      Operazione = '9'
027300001005e   1c                   ENDIF
027400001005     c*
027500001005     c                   ENDSR
027600001005     c*--------------------------------------------------------------------------------------------*
027700001005     c* controlla record di TIVTA -estratto bolle-
027800001005     c*--------------------------------------------------------------------------------------------*
027900001005     c     chkvta        BEGSR
028000001005     c*
028100001005     c                   MOVEL     'S'           $recok                         *record valido
028200001005     c*
028300001005     c* se le date non sono nel range richiesto, esclude
028400001005if  1c                   IF        $recok = 'S'                                 *ancora valido
028500001005     c                   EXSR      ChkVtaDsp
028600001005e   1c                   ENDIF
028700001005     c*
028800030213     c* se il numero documento mittente è -CIRCA- <> da quello richiesto, esclude
028900001005if  1c                   IF        $recok = 'S'                                 *ancora valido
029000030213     c                   EXSR      ChkVtaRifC
029100001005e   1c                   ENDIF
029200001005     c*
029300001005     c* se la provincia di arrivo è <> da quello richiesto, esclude
029400001005if  1c                   IF        $recok = 'S'                                 *ancora valido
029500001005     c                   EXSR      ChkVtaPrd
029600001005e   1c                   ENDIF
029700001005     c*
029800001005     c* se la località di arrivo è <> da quello richiesto, esclude
029900001005if  1c                   IF        $recok = 'S'                                 *ancora valido
030000001005     c                   EXSR      ChkVtaLod
030100001005e   1c                   ENDIF
030200001005     c*
030300001005     c* se il mittente è <> da quello richiesto, esclude
030400001005if  1c                   IF        $recok = 'S'                                 *ancora valido
030500001005     c                   EXSR      ChkVtaMit
030600001005e   1c                   ENDIF
030700001005     c*
030800001005     c* se il destinatario è <> da quello richiesto, esclude
030900001005if  1c                   IF        $recok = 'S'                                 *ancora valido
031000001005     c                   EXSR      ChkVtaDst
031100001005e   1c                   ENDIF
031200090902     c*
031300090902     c* Subunificante.
031400090902if  1c                   IF        $recok = 'S'                                 *ancora valido
031500090902     c                   EXSR      ChkSubk
031600090902e   1c                   ENDIF
031700001005     c*
031800001005     c* legge la bolla
031900001005if  1c                   IF        $recok = 'S'                                 *ancora valido
032000001005     c                   EXSR      LetTas
032100001005e   1c                   ENDIF
032200001005     c*
032300030213     c* se lo stato della giacenza è <> da quello richiesto, esclude
032400001005if  1c                   IF        $recok = 'S'                                 *ancora valido
032500030213     c                   EXSR      ChkVtaGcx
032600001005e   1c                   ENDIF
032700001005     c*
032800001005     c* memorizza in un deposito l'ultimo record letto
032900001005     c                   MOVEL     vtaksu        depksu                         *depositi
033000001005     c                   Z-ADD     vtadsp        depdsp
033100001005     c                   Z-ADD     vtalnp        deplnp
033200001005     c                   Z-ADD     vtanrs        depnrs
033300001005     c                   Z-ADD     vtansp        depnsp
033400001005     c                   MOVEL     vtatbl        deptbl
033500001005     c*
033600001005     c                   ENDSR
033700001005     c*--------------------------------------------------------------------------------------------*
033800001005     c* controlla le date
033900001005     c*--------------------------------------------------------------------------------------------*
034000001005     c     ChkVtaDsp     BEGSR
034100001005     c*
034200001005if  1c                   IF        vtadsp >= NInpDin AND
034300001005     c                             vtadsp <= NInpDfi
034400001005x   1c                   ELSE
034500001005     c                   MOVEL     'N'           $recok                         *record non valido
034600001005e   1c                   ENDIF
034700001005     c*
034800001005     c                   ENDSR
034900030213     c*--------------------------------------------------------------------------------------------*
035000030213     c* controlla il riferimento nel riferimento numerico o alfabetico o partner -CIRCA-
035100030213     c*--------------------------------------------------------------------------------------------*
035200030213     c     ChkVtaRifC    BEGSR
035300030213     c*
035400030213if  1c                   IF        InpRif <> *blanks                            *se richiesto
035500030213     c* alfabetico
035600030213     c                   EVAL      a40 = %TRIM(vtarma)
035700030213     c                   Z-ADD     *zeros        pos
035800030213     c                   EVAL      pos = %SCAN(%TRIM(NInpRif):a40)
035900030213if  2c                   IF        pos <= *zeros                                *non trovato
036000030213     c*
036100030213     c* alfabetico partner
036200030213     c                   EVAL      a40 = %TRIM(vtarpt)
036300030213     c                   Z-ADD     *zeros        pos
036400030213     c                   EVAL      pos = %SCAN(%TRIM(NInpRif):a40)
036500030213if  3c                   IF        pos <= *zeros                                *non trovato
036600030213     c* numerico
036700030213     c                   EVAL      a40 = %TRIM(%EDITC(vtarmn:'Z'))
036800030213     c                   Z-ADD     *zeros        pos
036900030213     c                   EVAL      pos = %SCAN(%TRIM(NInpRif):a40)
037000030213if  4c                   IF        pos <= *zeros                                *non trovato
037100030213     c                   MOVEL     'N'           $recok                         *record non valido
037200030213e   4c                   ENDIF
037300030213e   3c                   ENDIF
037400030213e   2c                   ENDIF
037500030213e   1c                   ENDIF
037600030213     c*
037700030213     c                   ENDSR
037800001005     c*--------------------------------------------------------------------------------------------*
037900001005     c* controlla la provincia di arrivo
038000001005     c*--------------------------------------------------------------------------------------------*
038100001005     c     ChkVtaPrd     BEGSR
038200001005     c*
038300001005     C*
038400001005     C* Controllo se richiesta una particolare provincia di arrivo
038500001005if  1C                   IF        %TRIM(NInpPrd) > *blanks
038600001005if  2C                   IF        vtaprd <> NInpPrd
038700001005     c                   MOVEL     'N'           $recok                         *record NON valido
038800001005e   2c                   ENDIF
038900001005e   1c                   ENDIF
039000001005     C*
039100001005     c                   ENDSR
039200001005     c*--------------------------------------------------------------------------------------------*
039300001005     c* controlla la località di arrivo
039400001005     c*--------------------------------------------------------------------------------------------*
039500001005     c     ChkVtaLod     BEGSR
039600001005     c*
039700001005if  1C                   IF        %TRIM(NInpLod) > *blanks
039800011213     C                   EVAL      a35 = %TRIM(vtalna)
039900011213     C                   EVAL      pos = %SCAN(%TRIM(NInpLod):a35)
040000001005if  2C                   IF        pos <= *zeros
040100001005     c                   MOVEL     'N'           $recok                         *record NON valido
040200001005e   2c                   ENDIF
040300001005e   1c                   ENDIF
040400001005     C*
040500001005     c                   ENDSR
040600001005     c*--------------------------------------------------------------------------------------------*
040700001005     c* controlla il mittente
040800001005     c*--------------------------------------------------------------------------------------------*
040900001005     c     ChkVtaMit     BEGSR
041000001005     c*
041100001005if  1C                   IF        %TRIM(NInpMit) > *blanks
041200011213     C                   EVAL      a35 = %TRIM(vtarsm)
041300011213     C                   EVAL      pos = %SCAN(%TRIM(NInpMit):a35)
041400001005if  2C                   IF        pos <= *zeros
041500001005     c                   MOVEL     'N'           $recok                         *record NON valido
041600001005e   2c                   ENDIF
041700001005e   1c                   ENDIF
041800001005     c*
041900001005     c                   ENDSR
042000001005     C*--------------------------------------------------------------------------------------------*
042100001005     c* controlla il destinatario
042200001005     C*--------------------------------------------------------------------------------------------*
042300001005     c     ChkVtaDst     BEGSR
042400001005     c*
042500001005if  1C                   IF        %TRIM(NInpDst) > *blanks
042600011213     C                   EVAL      a35 = %TRIM(vtarsd)
042700011213     C                   EVAL      pos = %SCAN(%TRIM(NInpDst):a35)
042800001005if  2C                   IF        pos <= *zeros
042900001005     c                   MOVEL     'N'           $recok                         *record NON valido
043000001005e   2c                   ENDIF
043100001005e   1c                   ENDIF
043200001005     c*
043300001005     c                   ENDSR
043400090902     c*--------------------------------------------------------------------------------------------*
043500090902     c* Controlla il subunificante.
043600090902     c*--------------------------------------------------------------------------------------------*
043700090902     c     chkSubk       BEGSR
043800090902     c                   IF        tis7700o1.subkSun <> *BLANK AND
043900090902     c                             %LEN(vtaSubk) > *ZERO AND
044000090902     c                             vtaSubk <> *BLANK AND
044100090902     c                             tis7700o1.subkSun <> vtaSubk
044200090902     c                   EVAL      $recOk = 'N'
044300090902     c                   ENDIF
044400090902     c                   ENDSR
044500090902     c*--------------------------------------------------------------------------------------------*
044600001005     c* lettura TITAS -bolle sede-
044700001005     c*--------------------------------------------------------------------------------------------*
044800001005     c     LetTas        BEGSR
044900001005     c*
045000001005     c                   MOVEL     vtadsp        kasaas                         *chiave bolla
045100001005     c                   Z-ADD     vtalnp        kaslnp
045200001005     c                   Z-ADD     vtanrs        kasnrs
045300001005     c                   Z-ADD     vtansp        kasnsp
045400001005     c                   MOVEL     vtatbl        kastbl
045500001005     c     keytas        CHAIN     titas30c
045600001005if  1c                   IF        NOT %FOUND                                   *non trovato
045700001005     c                   MOVEL     'N'           $recok                         *record NON valido
045800001005e   1c                   ENDIF
045900001005     c*
046000001005     c                   ENDSR
046100001005     c*--------------------------------------------------------------------------------------------*
046200030213     c* controlla lo stato della giacenza
046300001005     c*--------------------------------------------------------------------------------------------*
046400030213     c     ChkVtaGcx     BEGSR
046500010926     c*
046600010926     c* REimposta le variabili di lavoro
046700010926     c                   CLEAR                   $stato
046800030213     c*
046900030213     c* reperisce eventuale giacenza
047000030213     c                   Z-ADD     tasaas        kcpaas
047100030213     c                   Z-ADD     taslnp        kcplnp
047200030213     c                   Z-ADD     tasnrs        kcpnrs
047300030213     c                   Z-ADD     tasnsp        kcpnsp
047400030213     c                   Z-ADD     *zeros        kcpfrg
047500050309     c     keygcp        CHAIN     tigcp51l                           99
047600050415     **?Se la giacenza è alla 1a apertura la ignoro fino a che non raggiunge la fase 20
047700050415     **?(STAMPATA COMUNICAZIONE APERTURA GIACENZA).
047800030213if  1c                   IF        NOT *in99        AND                         *esistente
047900030213     c                             gcpatb = *blanks AND                         *no annullata
048000050415     c                             gcptcm <> 'Z'    AND
048100050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
048200030213     c*
048300030213     c                   MOVEL     'G'           $stato                         *AVUTO giacenza
048400030213     c*
048500030213if  2c                   IF        tasdcm = *zeros
048600030213     c                   MOVEL     'A'           $stato                         *APERTA
048700030213e   2c                   ENDIF
048800030213     c*
048900050405     c                   IF        gcpfas = 20
049000030213     c                   MOVEL     'D'           $stato                         *ATTESA disposizioni
049100030213e   2c                   ENDIF
049200030213e   1c                   ENDIF
049300030213     c*
049400030213     C* la bolla NON è o NON è stata in giacenza, esclude
049500030213if  1C                   IF        $stato = *blanks
049600030213     c                   MOVEL     'N'           $recok                         *record NON valido
049700030213x   1c                   ELSE
049800001005     C*
049900030213     C* stato giacenza <> da quello richiesto, esclude
050000030218if  2C                   IF        ($stato = InpAPE) OR
050100030218     c                             ($stato = InpDIS) OR
050200030218     c                             (InpAPE = *blanks AND InpDIS = *blanks)
050300030218x   2c                   ELSE
050400030218     c                   MOVEL     'N'           $recok                         *record NON valido
050500030218e   2c                   ENDIF
050600050420     **?Filtro sulla data apertura.
050700050420     C                   IF        gcpdur < nInpDtApe1 OR gcpdur > nInpDtApe2
050800050420     C                   EVAL      $RecOk = 'N'
050900050420     C                   ENDIF
051000050420     **?Filtro sulla data chiusura.
051100050420     C                   IF        gcpdcg = 0
051200050420     C                   EVAL      gcpdcg = 00010101
051300050420     C                   ENDIF
051400050420     C                   IF         gcpdcg < nInpDtChi1 OR gcpdcg > nInpDtChi2
051500050420     C                   EVAL      $RecOk = 'N'
051600050420     C                   ENDIF
051700050420     C*
051800030213e   1c                   ENDIF
051900011005     c*
052000001005     c                   ENDSR
052100001006     c*--------------------------------------------------------------------------------------------*
052200001006     c* memorizza le spedizioni da paginare nelle schiere
052300001006     c*--------------------------------------------------------------------------------------------*
052400001006     c     MemVta        BEGSR
052500001006     c*
052600001006     c* incrementa il n° di righe memorizzate
052700001006     c                   EVAL      RigheOut = RigheOut + 1
052800001006     c                   EVAL      r = Righeout                                 *solo + corto
052900001006     c*
053000001006     c* imposta la memorizzazione di almeno una riga
053100001006     c                   EVAL      Esito = '0'
053200001006     c*
053300001006     c* memorizza la riga nella DS di Output
053400001006     c                   MOVEL     vtadsp        a14
053500001006     c                   EVAL      SmDat(r) = %SUBST(a14:7:2) +
053600050420     c                                        '.' +
053700001006     c                                        %SUBST(a14:5:2) +
053800050420     c                                        '.' +
053900001006     c                                        %SUBST(a14:1:4)
053902110211    >***************************************************************************<
054005110211    >C*                  EVAL      SmRif(r) = %TRIM(%EDITC(vtarmn:'Z')) +       <
054100110211    >C*                                       ' ' +                             <
054200110211    >C*                                       %TRIM(vtarma) +                   <
054300110211    >C*                                       ' ' +                             <
054400110211    >C*                                       %TRIM(vtarpt)                     <
054401110211    >C                   EVAL      SmRif(r) = %CHAR(vtaRmn)                     <
054402110211    >C                   IF        vtaRma <> *BLANK AND                         <
054403110211    >C                             %CHAR(vtaRmn) <> %TRIM(vtaRma)               <
054405110211    >C                   EVAL      %SUBST(SmRif(r):16) = vtaRma                 <
054406110211    >C                   ELSE                                                   <
054407110211    >C                   IF        vtaRpt <> *BLANK                             <
054408110211    >C                   EVAL      %SUBST(SmRif(r):16) = vtaRpt                 <
054409110211    >C                   ENDIF                                                  <
054410110211    >C                   ENDIF                                                  <
054411110211    >***************************************************************************<
054500090903     c                   EVAL      SmDes(r)  = vtalna + ' ' + vtaprd
054600001006     c                   EVAL      SmDst(r)  = vtarsd
054700001006     c                   EVAL      SmSpe(r)  = vtaspe
054800030218if  1c                   IF        $stato    = 'G'                               *AVUTO giacenza
054900060613     c*                  EVAL      SmSta(r)  = sdsta(1)
055000060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0185':inpLang)
055100030218e   1c                   ENDIF
055200030218if  1c                   IF        $stato    = 'A'                               *APERTA
055300060613     c*                  EVAL      SmSta(r)  = sdsta(2)
055400060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0175':inpLang)
055500011030e   1c                   ENDIF
055600030218if  1c                   IF        $stato    = 'D'                               *ATTESA DISPOS.
055700060613     c*                  EVAL      SmSta(r)  = sdsta(3)
055800060613     c                   EVAL      SmSta(r)  = rtvMsgLang('TIS0029':inpLang)
055900001006e   1c                   ENDIF
056000001006     c*
056100001006     c                   ENDSR
056200001005     c*--------------------------------------------------------------------------------------------*
056300001005     c* operazioni per nuova richiesta
056400001005     c*--------------------------------------------------------------------------------------------*
056500001005     c     newric        BEGSR
056600001005     c*
056700001005     c* normalizza i campi in Input
056800001005     c                   EXSR      NormInput
056900090902     c*
057000090902     C                   RESET                   tis7700i1
057100090902     C                   RESET                   tis7700o1
057200090902     C                   EVAL      tis7700i1.ksu = inpKsc
057300090902     C                   EVAL      tis7700i1.sun = %EDITC(inpRqsCid : 'X')
057400090902     C                   CALLP     Selettore_bolla_subUnificante('GETSUBKSUN'
057500090902     C                             : rpyEsito
057600090902     C                             : 'TIS7700I1' : tis7700i1 : %SIZE(tis7700i1)
057700090902     C                             : 'TIS7700O1' : tis7700o1 : %SIZE(tis7700o1)
057800090902     C                             )
057900090902     c*
058000001005     c                   ENDSR
058100001005     c*--------------------------------------------------------------------------------------------*
058200001005     c* normalizza i campi in Input
058300001005     c*--------------------------------------------------------------------------------------------*
058400001005     c     NormInput     BEGSR
058500001005     c*
058600001005     c* pulisce le variabili di lavoro
058700001005     c                   CLEAR                   NInpDin
058800001005     c                   CLEAR                   NInpDfi
058900030213     c                   CLEAR                   NInpRif
059000030213     c                   CLEAR                   NInpPrd
059100001005     c                   CLEAR                   NInpLod
059200001005     c                   CLEAR                   NInpMit
059300001005     c                   CLEAR                   NInpDst
059400050420     C                   CLEAR                   nInpDtApe1
059500050420     C                   CLEAR                   nInpDtApe2
059600050420     C                   CLEAR                   nInpDtChi1
059700050420     C                   CLEAR                   nInpDtChi2
059800011128     c*
059900011128     C* data fine, se non inserita = data corrente
060000011128     C                   Z-ADD     datcor        NInpDfi
060100011128     c*
060200011128     c* controlla se la data fine è numerica
060300011128     c                   MOVEL     InpDfi        dsdate
060400011128     c                   MOVEL     dsanne        dsanna
060500011128     c                   MOVEL     dsmese        dsmesa
060600011128     c                   MOVEL     dsgioe        dsgioa
060700011128     c                   MOVE      dsdata        a1                             *ultimo carattere
060800011128     c                   SETOFF                                       98
060900011128     c                   TESTN                   dsdata               98
061000011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
061100011128     c                             a1 >= '0'                                    *NON trovata lettera
061200011128     c                   MOVEL     dsanna        dsann
061300011128     c                   MOVEL     dsmesa        dsmes
061400011128     c                   MOVEL     dsgioa        dsgio
061500011128     c                   Z-ADD     dsdat         g08inv
061600011128     c                   Z-ADD     *zeros        g08dat
061700011128     C                   MOVEL     '3'           g08err
061800011128     c                   CALL      'XSRDA8'
061900011128     c                   PARM                    wlbda8
062000011128if  2c                   IF        g08err <> '1'                                *no errore
062100011128     c                   Z-ADD     g08inv        NInpDfi
062200011128e   2c                   ENDIF
062300011128e   1c                   ENDIF
062400011128     c*
062500011128     c* data inizio, se nessuna richiesta = data corrente - 1 anno
062600011128     c                   Z-ADD     datcor        dsdat
062700011128     c                   EVAL      dsann = dsann - 1
062800011128     c                   Z-ADD     dsdat         NInpDin                        *nessuna richiesta
062900011128     c*
063000011128     c* controlla se la data inizio è numerica
063100011128     c                   MOVEL     InpDin        dsdate
063200011128     c                   MOVEL     dsanne        dsanna
063300011128     c                   MOVEL     dsmese        dsmesa
063400011128     c                   MOVEL     dsgioe        dsgioa
063500011128     c                   MOVE      dsdata        a1                             *ultimo carattere
063600011128     c                   SETOFF                                       98
063700011128     c                   TESTN                   dsdata               98
063800011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
063900011128     c                             a1 >= '0'                                    *NON trovata lettera
064000011128     c                   MOVEL     dsanna        dsann
064100011128     c                   MOVEL     dsmesa        dsmes
064200011128     c                   MOVEL     dsgioa        dsgio
064300011128     c                   Z-ADD     dsdat         g08inv
064400011128     c                   Z-ADD     *zeros        g08dat
064500011128     C                   MOVEL     '3'           g08err
064600011128     c                   CALL      'XSRDA8'
064700011128     c                   PARM                    wlbda8
064800011128if  2c                   IF        g08err <> '1'                                *no errore
064900011128     c                   Z-ADD     g08inv        NInpDin
065000011128e   2c                   ENDIF
065100011128e   1c                   ENDIF
065200050420
065300050420     **?Data apertura iniziale.
065400050420     C     *EUR          TEST(DE)                InpDtApe1
065500050420     C                   IF        %ERROR
065600050420     C                   EVAL      InpDtApe1 = '01.01.0001'
065700050420     C                   ENDIF
065800050420     C     *EUR          MOVE      InpDtApe1     WrkDtIso
065900050420     C                   MOVE      WrkDtIso      nInpDtApe1
066000050420
066100050420     **?Data apertura finale.
066200050420     C     *EUR          TEST(DE)                InpDtApe2
066300050420     C                   IF        %ERROR
066400050420     C                   EVAL      InpDtApe2 = '31.12.9999'
066500050420     C                   ENDIF
066600050420     C     *EUR          MOVE      InpDtApe2     WrkDtIso
066700050420     C                   MOVE      WrkDtIso      nInpDtApe2
066800050420
066900050420     **?Data chiusura iniziale.
067000050420     C     *EUR          TEST(DE)                InpDtChi1
067100050420     C                   IF        %ERROR
067200050420     C                   EVAL      InpDtChi1 = '01.01.0001'
067300050420     C                   ENDIF
067400050420     C     *EUR          MOVE      InpDtChi1     WrkDtIso
067500050420     C                   MOVE      WrkDtIso      nInpDtChi1
067600050420
067700050420     **?Data chiusura finale.
067800050420     C     *EUR          TEST(DE)                InpDtChi2
067900050420     C                   IF        %ERROR
068000050420     C                   EVAL      InpDtChi2 = '31.12.9999'
068100050420     C                   ENDIF
068200050420     C     *EUR          MOVE      InpDtChi2     WrkDtIso
068300050420     C                   MOVE      WrkDtIso      nInpDtChi2
068400050420
068500030213     c*
068600030213     c* riferimenti
068700030213     c     minu:maiu     XLATE     InpRif        NInpRif                        *Minus -> Maiuscolo
068800001005     C*
068900001005     C* provincia di arrivo
069000001005     C     minu:maiu     XLATE     InpPrd        NInpPrd                        *Minus -> Maiuscolo
069100001005     c*
069200001005     c* località di arrivo
069300001005     c     minu:maiu     XLATE     InpLod        NInpLod                        *Minus -> Maiuscolo
069400001005     c*
069500001005     c* mittente
069600001005     c     minu:maiu     XLATE     InpMit        NInpMit                        *Minus -> Maiuscolo
069700001005     c*
069800001005     c* destinatario
069900001005     c     minu:maiu     XLATE     InpDst        NInpDst                        *Minus -> Maiuscolo
070000001005     c*
070100001005     c                   ENDSR
070200001005     c*--------------------------------------------------------------------------------------------*
070300001005     c* imposta i depositi della chiave di lettura per la prima lettura
070400001005     c*--------------------------------------------------------------------------------------------*
070500001005     c     firstlet      BEGSR
070600001005     c*
070700001005     c* variabili di lavoro
070800001005     c                   EVAL      $fvta = 'N'                                  *no fine file
070900001005     c* chiave
071000001005     c                   MOVEL     InpKsc        depksu                         *deposito unificante
071100001005     c                   Z-ADD     NInpDin       depdsp                         *data inizio
071200001005     c                   Z-ADD     *zeros        deplnp
071300001005     c                   Z-ADD     *zeros        depnrs
071400001005     c                   Z-ADD     *zeros        depnsp
071500001005     c                   MOVEL     *blanks       deptbl
071600001005     c*
071700001005     c                   ENDSR
071800001005     c*--------------------------------------------------------------------------------------------*
071900001005     c* operazioni iniziali -da fare sempre-
072000001005     c*--------------------------------------------------------------------------------------------*
072100001005     c     rutinz        BEGSR
072200001005     c*
072300001005     c* reperimento data corrente
072400001005     C                   TIME                    n14                            *ora (6)+ data (8)
072500001005     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
072600001005     C                   MOVE      n14           n8                             *data (8) in g/m/a
072700001005     C                   Z-ADD     n8            g08dat
072800001005     C                   Z-ADD     *zeros        g08inv
072900001005     C                   MOVEL     '0'           g08err
073000001005     c                   CALL      'XSRDA8'
073100001005     c                   PARM                    wlbda8
073200001005     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
073300001005     c*
073400001005     c* imposta variabli di lavoro
073500001005     c                   EVAL      RigheOut =  *zeros
073600001006     c*
073700001006     c* azzera schiere di memorizzazione
073800001006     c                   CLEAR                   SmRif
073900001006     c                   CLEAR                   SmDat
074000001006     c                   CLEAR                   SmDes
074100001006     c                   CLEAR                   SmDst
074200001006     c                   CLEAR                   SmSpe
074300001006     c                   CLEAR                   SmSta
074400001005     c*
074500001005     c                   ENDSR
074600001005     c*--------------------------------------------------------------------------------------------*
074700001005     c* impostazioni iniziali -la prima volta-
074800001005     c*--------------------------------------------------------------------------------------------*
074900001005     c     *inzsr        BEGSR
075000001005     c*
075100001005     c* ricevimento parametri
075200001005     c     *ENTRY        PLIST
075300030213     c                   PARM                    TIS1G2IDS
075400001005     c                   PARM                    Operazione
075500001005     c                   PARM                    RigheRic
075600030213     c                   PARM                    TIS1G2PDS
075700001005     c                   PARM                    RigheOut
075800001005     c                   PARM                    Esito
075900001005     c*
076000001005     c* chiavi di lettura
076100090902     c     keyvta        KLIST                                                  tivta12l
076200001005     c                   KFLD                    ktaksu                         -cliente unificante
076300001005     c                   KFLD                    ktadsp                         -data spedizione
076400001005     c                   KFLD                    ktalnp                         -linea partenza
076500001005     c                   KFLD                    ktanrs                         -serie
076600001005     c                   KFLD                    ktansp                         -spedizione
076700001005     c                   KFLD                    ktatbl                         -tipo bolla
076800001005     c     keytas        KLIST                                                  titas30c
076900001005     c                   KFLD                    kasaas                         -anno spedizione
077000001005     c                   KFLD                    kaslnp                         -linea partenza
077100001005     c                   KFLD                    kasnrs                         -serie
077200001005     c                   KFLD                    kasnsp                         -spedizione
077300001005     c                   KFLD                    kastbl                         -tipo bolla
077400021108     c     keygcp        KLIST                                                  *figcp01l
077500021108     c                   KFLD                    kcpaas                         -anno spedizione
077600021108     c                   KFLD                    kcplnp                         -linea partenza
077700021108     c                   KFLD                    kcpnrs                         -serie
077800021108     c                   KFLD                    kcpnsp                         -spedizione
077900021108     c                   KFLD                    kcpfrg                         -prg aperture
078000001005     c*
078100001005     c* agiunge le librerie in lista
078200060613     c*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
078300060613     c*                  CALL      wpgm
078400001005     c*
078500001005     c* apre i files
078600090902     c                   OPEN      tivta12l
078700001005     c                   OPEN      titas30c
078800050309     c                   OPEN      tigcp51l
078900030213     c                   OPEN      azdst01l
079000001005     c*
079100001005     c                   ENDSR
079200001005     o*--------------------------------------------------------------------------------------------*
079300050419** sdsta (5) - Fase giacenza
079400050419Evasa          1
079500050419Esecuzione     2
079600050419Attesa         3
079700050419               4
079800050419               5
