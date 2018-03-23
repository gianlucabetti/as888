000100001005     h*--------------------------------------------------------------------------------------------*
000200011105     h* HERBALIFE - Visualizza bolle da selezionare
000300001005     h*--------------------------------------------------------------------------------------------*
000400001005      /TITLE Carica righe spedizione trovate
000500001005     Hdatedit(*DMY)
000600001005     f*--------------------------------------------------------------------------------------------*
000700001005     f* Data base
000800001005     f*--------------------------------------------------------------------------------------------*
000900001005     f*---
001000001005     f* Estratto bolle di sede
001100001005     f*---
001200090903     ftivta16l  IF   E           K DISK    USROPN
001300001005     d*--------------------------------------------------------------------------------------------*
001400001005     d* Data structure
001500001005     d*--------------------------------------------------------------------------------------------*
001600001006     d*---
001700001006     d* Schiere di memorizzazione
001800001006     d*---
001900001006     d* 20 righe per pagina
002000011105     d SmA08           S              8    DIM(20)                              *alfabetico 8
002100011105     d SmRif           S             30    DIM(20)                              *riferimento
002200001006     d SmDat           S             10    DIM(20)                              *data spedizione
002300001006     d SmDes           S             30    DIM(20)                              *destinazione
002400001006     d SmDst           S             30    DIM(20)                              *Destinatario
002500001006     d SmSpe           S             12    DIM(20)                              *n° spedizione
002600001005     d*---
002700001005     d* Variabili riferite al data base
002800001005     d*---
002900011106     d ktaksu          S                   LIKE(vtaksu)                         *lettura tivta05l
003000011106     d ktaa08          S                   LIKE(vtaa08)
003100011106     d ktadsp          S                   LIKE(vtadsp)
003200011106     d ktalnp          S                   LIKE(vtalnp)
003300011106     d ktanrs          S                   LIKE(vtanrs)
003400011106     d ktansp          S                   LIKE(vtansp)
003500011106     d ktatbl          S                   LIKE(vtatbl)
003600011106     d depksu          S                   LIKE(vtaksu)                         *depositi
003700011106     d depa08          S                   LIKE(vtaa08)
003800011106     d depdsp          S                   LIKE(vtadsp)
003900011106     d deplnp          S                   LIKE(vtalnp)
004000011106     d depnrs          S                   LIKE(vtanrs)
004100011106     d depnsp          S                   LIKE(vtansp)
004200011106     d deptbl          S                   LIKE(vtatbl)
004300001005     d*---
004400001005     d* Variabili
004500001005     d*---
004600001005     d $fvta           S              1    INZ('N')                             *fine lettura
004700001005     d $recok          S              1    INZ('N')                             *validità record
004800001005     d i               S              5  0                                      *indici
004900001006     d r               S              5  0
005000001006     d z               S              5  0
005100001005     d a30             S             30                                         *Alfabetico 30
005200001005     d a14             S             14                                         *alfabetico 14
005300001005     d n14             S             14  0                                      *Numerico 14
005400001005     d n8              S              8  0                                      *Numerico 8
005500001005     d n4              S              4  0                                      *Numerico 4
005600001005     d pos             S              5  0                                      *posizione SCAN
005700001005     d datcor          S              8  0                                      *data corrente
005800001005     d oracor          S              6  0                                      *ora corrente
005900001005     d wpgm            S             20                                         *programma
006000001005     d Operazione      S              1
006100001005     D wrkore          s             20  0 inz(*zeros)
006200001005     D wrkdata         s               d   DATFMT(*ISO) INZ(D'1999-01-01')
006300001005     d*---
006400001005     d* Ds
006500001005     d*---
006600001005      * DS controlla data
006700001005     d wlbda8          DS                  INZ                                  *controlla data
006800001005     d  g08dat                        8  0                                       -data dritta
006900001005     d  g08inv                        8  0                                       -data invertita
007000001005     d  g08err                        1                                          -errore
007100001005     d  g08tgi                        5  0                                       -giorni fra date
007200001005      * composizione spedizione
007300001005     d                 DS                  INZ
007400001005     d  vtalnp                 1      3  0                                       -linea partenza
007500001005     d  vtanrs                 4      5  0                                       -serie
007600001005     d  vtansp                 6     12  0                                       -spedizione
007700001005     d vtaspe                  1     12                                         *codice spedizione
007800001005      * DS contenente i dati dI INPUT
007900011106     d tis1h2IDS     E DS                  prefix(Inp)
008000001005     d* Input
008100001005     d RigheOut        S              5s 0
008200001005     d RigheRic        S              5s 0
008300001005     d Esito           S              1
008400011106      * DS ritornata dal programma di accesso ai dati -> contiene una riga di dati
008500011106     d tis1h2PDS     E DS                  PREFIX(Pag)
008600001005     c*--------------------------------------------------------------------------------------------*
008700001005     c* Main lines
008800001005     c*--------------------------------------------------------------------------------------------*
008900001005     c*--- --- --- --- --- --- ---
009000001005     c* Operazione
009100001005     c* '1' = Inizio nuova richiesta
009200001005     c* '2' = Dati trovati, richiesta successiva
009300001005     c* '9' = Non trovati dati
009400001005     c*--- --- --- --- --- --- ---
009500001005     c*
009600001005     c* operazioni iniziali -da fare sempre-
009700001005     c                   EXSR      rutinz
009800001005     c*
009900001005     c* Se operazione = '1' esegue prime letture
010000001005if  1c                   IF        Operazione = '1'
010100001005     c*
010200001005     c* operazioni per nuova richiesta
010300001005     c                   EXSR      newric
010400001005     c*
010500001005     c* imposta i depositi della chiave di lettura per la prima lettura
010600001005     c                   EXSR      firstlet
010700001005     c*
010800001005     c* elabora il file TIVTA -estratto bolle-
010900001005     c                   EXSR      elavta
011000001005e   1c                   ENDIF
011100001005     c*
011200001005     c* Se operazione = '2' esegue letture successive
011300001005if  1c                   IF        Operazione = '2'
011400001005     c*
011500001005     c* a fine file -giro precedente- imposta Operazione = '9'
011600001005if  2c                   IF        $fvta = 'S'                                  *fine file
011700001005     c                   EVAL      Operazione = '9'
011800001005     c                   EVAL      RigheOut =  *zeros
011900001005x   2c                   ELSE                                                   *no fine file
012000001005     c*
012100001005     c* elabora il file TIVTA -estratto bolle-
012200001005     c                   EXSR      elavta
012300001005e   2c                   ENDIF
012400001005e   1c                   ENDIF
012500001005     c*
012600001005     c                   RETURN
012700011106     c*--------------------------------------------------------------------------------------------*
012800011106     c* operazioni per nuova richiesta
012900011106     c*--------------------------------------------------------------------------------------------*
013000011106     c     newric        BEGSR
013100011106     c*
013200011106     c* normalizza i campi in Input
013300011106     c                   EXSR      NormInput
013400011106     c*
013500011106     c                   ENDSR
013600011106     c*--------------------------------------------------------------------------------------------*
013700011106     c* normalizza i campi in Input
013800011106     c*--------------------------------------------------------------------------------------------*
013900011106     c     NormInput     BEGSR
014000050211     c*
014100050211if  1c                   IF        InpDocMit = *BLANK
014200050211     c                   EVAL      Operazione = '9'
014300050211     C                   EVAL      Esito = *ON
014400050211     C                   RETURN
014500050211     C                   ENDIF
014600011106     c*
014700011106     c                   ENDSR
014800011106     c*--------------------------------------------------------------------------------------------*
014900011106     c* imposta i depositi della chiave di lettura per la prima lettura
015000011106     c*--------------------------------------------------------------------------------------------*
015100011106     c     firstlet      BEGSR
015200011106     c*
015300011106     c* variabili di lavoro
015400011106     c                   EVAL      $fvta = 'N'                                  *no fine file
015500011106     c* chiave
015600011106     c                   MOVEL     InpKsc        depksu                         *deposito unificante
015700091202     c                   EVAL      depa08 = InpDocMit
015800011106     c                   Z-ADD     *zeros        depdsp
015900011106     c                   Z-ADD     *zeros        deplnp
016000011106     c                   Z-ADD     *zeros        depnrs
016100011106     c                   Z-ADD     *zeros        depnsp
016200011106     c                   MOVEL     *blanks       deptbl
016300011106     c*
016400011106     c                   ENDSR
016500001005     c*--------------------------------------------------------------------------------------------*
016600001005     c* elabora il file TIVTA -estratto bolle-
016700001005     c*--------------------------------------------------------------------------------------------*
016800001005     c     elavta        BEGSR
016900001005     c*
017000001005     c* posizionamento su TIVTA -estratto bolle-
017100001005     c                   EXSR      setvta
017200001005     c*
017300001005     c* carica la prima pagina di 20 righe
017400001005do  1c                   DOW       RigheOut < RigheRic AND                      *riempimento pagina
017500001005     c                             $fvta = 'N'                                  *fine file
017600001005     c*
017700001005     c* lettura di TIVTA -estratto bolle-
017800001005     c                   EXSR      letvta
017900001005     c*
018000001005     c* memorizza le spedizioni da paginare, se corrette
018100001005if  2c                   IF        $recok = 'S'  AND
018200001005     c                             $fvta = 'N'
018300001005     c                   EXSR      MemVta
018400001005e   2c                   ENDIF
018500001005e   1c                   ENDDO
018600001005     c*
018700001006     c* porta le schiere di memorizzazione nella DS di Output
018800011106     c                   MOVEA     SmA08         PagA08
018900011106     c                   MOVEA     SmRif         PagRif
019000001006     c                   MOVEA     SmDat         PagDat
019100001006     c                   MOVEA     SmDes         PagDes
019200001006     c                   MOVEA     SmDst         PagDst
019300001006     c                   MOVEA     SmSpe         PagSpe
019400001006     c*
019500001005     c                   ENDSR
019600001005     c*--------------------------------------------------------------------------------------------*
019700001005     c* posizionamento su TIVTA -estratto bolle-
019800001005     c*--------------------------------------------------------------------------------------------*
019900001005     c     setvta        BEGSR
020000001005     c*
020100001005     c* posizionamento dall'ultimo record letto
020200011106     c* . se 1^ volta la chiave è impostata con cliente unificante + riferimento + data inizio + 0
020300001005     c* . se n^ volte la chiave è impostata con dati ultima bolla letta
020400001005     c                   MOVEL     depksu        ktaksu                         *deposito unificante
020500091202     c                   EVAL      ktaa08 = depa08                              *riferimento
020600001005     c                   Z-ADD     depdsp        ktadsp                         *depositi chiave
020700001005     c                   Z-ADD     deplnp        ktalnp
020800001005     c                   Z-ADD     depnrs        ktanrs
020900001005     c                   Z-ADD     depnsp        ktansp
021000001005     c                   MOVEL     deptbl        ktatbl
021100090903     c     keyvta        SETGT     tivta16l                           99
021200001005if  1c                   IF        *in99                                        *fine file
021300001005     c                   MOVEL     'S'           $fvta                          *fine lettura
021400001005     c                   EVAL      Operazione = '9'
021500001005e   1c                   ENDIF
021600001005     c*
021700001005     c                   ENDSR
021800001005     c*--------------------------------------------------------------------------------------------*
021900001005     c* lettura TIVTA -estratto bolle-
022000001005     c*--------------------------------------------------------------------------------------------*
022100001005     c     letvta        BEGSR
022200001005     c*
022300090903     c     ktaksu        READE     tivta16l                               99
022400001005if  1c                   IF        NOT *in99                                    *record trovato
022500001005     c                   EXSR      chkvta                                       *controlla record
022600001005x   1c                   ELSE                                                   *record non trovato
022700001005     c                   MOVEL     'S'           $fvta                          *fine lettura
022800001005     c                   EVAL      Operazione = '9'
022900001005e   1c                   ENDIF
023000001005     c*
023100001005     c                   ENDSR
023200001005     c*--------------------------------------------------------------------------------------------*
023300001005     c* controlla record di TIVTA -estratto bolle-
023400001005     c*--------------------------------------------------------------------------------------------*
023500001005     c     chkvta        BEGSR
023600001005     c*
023700001005     c                   MOVEL     'S'           $recok                         *record valido
023800001005     c*
023900011106     c* se il riferimento alfabetico8 <> da quello richiesto, esclude
024000001005if  1c                   IF        $recok = 'S'                                 *ancora valido
024100011106     c                   EXSR      ChkVtaA08
024200001005e   1c                   ENDIF
024300001005     c*
024400001005     c* memorizza in un deposito l'ultimo record letto
024500001005     c                   MOVEL     vtaksu        depksu                         *depositi
024600090903     c                   EVAL      depa08 = vtaa08
024700001005     c                   Z-ADD     vtadsp        depdsp
024800011106     c                   Z-ADD     vtalnp        deplnp
024900001005     c                   Z-ADD     vtanrs        depnrs
025000001005     c                   Z-ADD     vtansp        depnsp
025100001005     c                   MOVEL     vtatbl        deptbl
025200001005     c*
025300001005     c                   ENDSR
025400001005     c*--------------------------------------------------------------------------------------------*
025500011106     c* controlla il riferimento alfabetico 8
025600001005     c*--------------------------------------------------------------------------------------------*
025700011106     c     ChkVtaA08     BEGSR
025800001005     c*
025900011106if  1c                   IF        vtaa08 <> InpDocMit
026000001005     c                   MOVEL     'N'           $recok                         *record non valido
026100011106     c                   MOVEL     'S'           $fvta                          *fine lettura
026200001005e   1c                   ENDIF
026300001005     c*
026400001005     c                   ENDSR
026500001006     c*--------------------------------------------------------------------------------------------*
026600001006     c* memorizza le spedizioni da paginare nelle schiere
026700001006     c*--------------------------------------------------------------------------------------------*
026800001006     c     MemVta        BEGSR
026900001006     c*
027000001006     c* incrementa il n° di righe memorizzate
027100001006     c                   EVAL      RigheOut = RigheOut + 1
027200001006     c                   EVAL      r = Righeout                                 *solo + corto
027300001006     c*
027400001006     c* memorizza la riga nella DS di Output
027500011106     c                   EVAL      SmA08(r)  = vtaa08
027600001006     c                   MOVEL     vtadsp        a14
027700001006     c                   EVAL      SmDat(r) = %SUBST(a14:7:2) +
027800001006     c                                        '/' +
027900001006     c                                        %SUBST(a14:5:2) +
028000001006     c                                        '/' +
028100001006     c                                        %SUBST(a14:1:4)
028101110211    >***************************************************************************<
028200110211    >c*                  EVAL      SmRif(r) = %TRIM(%EDITC(vtarmn:'Z')) +       <
028300110211    >C*                                       ' ' +                             <
028400110211    >C*                                       %TRIM(vtarma) +                   <
028500110211    >C*                                       ' ' +                             <
028600110211    >C*                                       %TRIM(vtarpt)                     <
028601110211    >C                   EVAL      smRif(r) = %CHAR(vtaRmn)                     <
028602110211    >C                   IF        vtaRma <> *BLANK AND                         <
028603110211    >C                             %CHAR(vtaRmn) <> %TRIM(vtaRma)               <
028604110211    >C                   EVAL      %SUBST(smRif(r):16) = vtaRma                 <
028605110211    >C                   ELSE                                                   <
028606110211    >C                   EVAL      %SUBST(smRif(r):16) = vtaRpt                 <
028607110211    >C                   ENDIF                                                  <
028608110211    >***************************************************************************<
028700090903     c                   EVAL      SmDes(r)  = vtalna + ' ' + vtaprd
028800001006     c                   EVAL      SmDst(r)  = vtarsd
028900001006     c                   EVAL      SmSpe(r)  = vtaspe
029000001006     c*
029100001006     c                   ENDSR
029200001005     c*--------------------------------------------------------------------------------------------*
029300001005     c* operazioni iniziali -da fare sempre-
029400001005     c*--------------------------------------------------------------------------------------------*
029500001005     c     rutinz        BEGSR
029600001005     c*
029700001005     c* reperimento data corrente
029800001005     C                   TIME                    n14                            *ora (6)+ data (8)
029900001005     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
030000001005     C                   MOVE      n14           n8                             *data (8) in g/m/a
030100001005     C                   Z-ADD     n8            g08dat
030200001005     C                   Z-ADD     *zeros        g08inv
030300001005     C                   MOVEL     '0'           g08err
030400001005     c                   CALL      'XSRDA8'
030500001005     c                   PARM                    wlbda8
030600001005     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
030700001005     c*
030800001005     c* imposta variabli di lavoro
030900001005     c                   EVAL      RigheOut =  *zeros
031000050211     c                   CLEAR                   tis1h2PDS
031100011120     c*
031200011120     c* imposta esito
031300011120     c                   EVAL      Esito = '0'
031400001006     c*
031500011106     c* azzera schiere e variabili di memorizzazione
031600011106     c                   CLEAR                   SmA08
031700011106     c                   CLEAR                   SmRif
031800011106     c                   CLEAR                   SmDat
031900011106     c                   CLEAR                   SmDes
032000011106     c                   CLEAR                   SmDst
032100011106     c                   CLEAR                   SmSpe
032200001005     c*
032300001005     c                   ENDSR
032400001005     c*--------------------------------------------------------------------------------------------*
032500001005     c* impostazioni iniziali -la prima volta-
032600001005     c*--------------------------------------------------------------------------------------------*
032700001005     c     *inzsr        BEGSR
032800001005     c*
032900001005     c* ricevimento parametri
033000001005     c     *ENTRY        PLIST
033100011106     c                   PARM                    tis1h2IDS
033200001005     c                   PARM                    Operazione
033300001005     c                   PARM                    RigheRic
033400011106     c                   PARM                    tis1h2PDS
033500001005     c                   PARM                    RigheOut
033600001005     c                   PARM                    Esito
033700001005     c*
033800001005     c* chiavi di lettura
033900090903     c     keyvta        KLIST                                                  tivta16l
034000001005     c                   KFLD                    ktaksu                         -cliente unificante
034100011106     c                   KFLD                    ktaa08                         -alfabetico 8
034200011106     c                   KFLD                    ktadsp                         -data spedizione
034300011106     c                   KFLD                    ktalnp                         -linea partenza
034400011106     c                   KFLD                    ktanrs                         -serie spedizione
034500011106     c                   KFLD                    ktansp                         -n° spedizione
034600011106     c                   KFLD                    ktatbl                         -tipo bolla
034700001005     c*
034800001005     c* agiunge le librerie in lista
034900060626     c*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
035000060626     c*                  CALL      wpgm
035100001005     c*
035200001005     c* apre i files
035300090903     c                   OPEN      tivta16l
035400001005     c*
035500001005     c                   ENDSR
