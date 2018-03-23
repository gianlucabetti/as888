000100020219     H DECEDIT('0,') DATEDIT(*DMY.)
000200020219
000300020219      ****************************************************************
000400020221      ** TRUL28R ** Calcolo del CHECK DIGIT                         **
000500020219      ****************************************************************
000600020220
000700020220     FTRUL28D   CF   E             WORKSTN
000800020220     F                                     USROPN
000900020219
001000020219      *
001100020219      * SCHIERE
001200020219      *
001300020219     D MSG             s             78    dim(05) ctdata perrcd(1)
001400020221
001500020219      *
001600020219      * DS definite ESTERNAMENTE
001700020219      *
001800020219     D KPJBA         e ds
001900090916     D TRUL28DS      e ds                  inz
002000090916     D TRUL28DS4     e ds                  inz
002100090916     D TRUL28DSX     e ds                  inz  prefix(X)
002200020221      *
002300060428      * - Parametri x Controllo profilo utenti
002400060428     d TIBS34ds      e ds
002500060428      * - Ds di riferimento al file esterno AZUTE00F
002600060428     d AZUTEds       e ds                  extname(AZUTE00F)
002700060428      * - Ds per dati organigramma
002800060428     d DDatiUte      e ds
002900020221
003000020219      *
003100020219      * DS definite INTERNAMENTE
003200020219      *
003300020221
003400020220      *
003500020221      * Status DS
003600020220      *
003700060428     D Status         sds           333
003800060428     D  SDSpgm           *proc
003900060428     D  SDSprm           *parms
004000060428     D  SDSusr                81     90
004100020221
004200020219      *
004300020219      * VARIABILI
004400020219      *
004500020220     D WKPJBA          s                   like(KPJBA)
004600030404     D WPGM            s             10    inz
004700060428     D $Video          s              1    inz('1')
004800020221     D $Fine           s              1    inz(*off)
004900020221     D $Err            s              1    inz(*off)
005000020221
005100020219      *
005200020219      * COSTANTI
005300020219      *
005400020219     D Digits          c                   const('0123456789')
005500020219
005600020221      ****************************************************************
005700020221
005800020219      * Operazioni iniziali
005900020219     C                   exsr      OPEINI
006000020220      *
006100020221do  1C                   dow       $Fine = *off
006200020221      *
006300020221      * Eventuale emissione videata per richiesta parametri di input
006400060428cas 2C     $Video        caseq     '1'           GesVid1
006500060428cas 2C     $Video        caseq     '2'           GesVid2
006600060428cas 2C     $Video        caseq     '0'           GesVid0
006700020221e   2C                   endcs
006800020221      *
006900020221e   1C                   enddo
007000020219      *
007100020221      * Fine
007200020219     C                   exsr      ENDPGM
007300020219
007400020219      ****************************************************************
007500020219      ** OPEINI ** Operazioni Iniziali                              **
007600020219      ****************************************************************
007700020219     C     OPEINI        BEGSR
007800020221      *
007900020221     C     *ENTRY        plist
008000020221     C                   parm                    wKPJBA
008100020221      *
008200060428     C                   if        SDSprm = *zeros
008300020221     C                   clear                   KPJBA
008400060428     C                   eval      KNMUS = SDSusr
008500020221     C***                eval      KNSIF = ???
008600020221     C                   clear                   TRUL28DS
008700040709     C                   clear                   TRUL28DS4
008800020221     C                   else
008900020221     C                   movel     wKPJBA        KPJBA
009000020221     C                   movel     KPJBU         TRUL28DS
009100040709     C                   movel     KPJBU         TRUL28DS4
009200090916     C                   movel     KPJBU         TRUL28DSX
009300020221     C                   endif
009400040709      *
009500040709      * Inizializzazione campi di output a video e DS
009600060428     C                   clear                   O28err
009700060428     C                   clear                   O28msg
009800060428     C                   clear                   O28ckd
009900060428     C                   clear                   O28cod
010000060428     C                   clear                   O284cod
010100060428     C                   clear                   O284ckd
010200060428     C                   clear                   O284err
010300040709     C                   clear                   VO28cod
010400020221      *
010500020221     C                   reset                   $Err
010600020221     C                   reset                   $Fine
010700020221     C                   reset                   $Video
010800020221      *
010900020221      * Richiesta la sola chiusura del file:
011000020221cas 1C     I28tla        caseq     'C'           ENDPGM
011100020221e   1C                   endcs
011200020221      *
011300040709     C                   if            I28mod = *blanks
011400040709     C                             and VI28cod = *blanks
011500060428     C                   eval      $Video = '1'
011600020221     C                   endif
011700020220      *
011800020220     C                   eval      D2SEP  = *all'-'
011900060428     C                   eval      VTCPGM = SDSpgm
012000020221      *
012100060428      * Reperisco dati job
012200060428     c                   exsr      DatiJob
012300020219      *
012400020219     C                   ENDSR
012500060428      *
012600060428      *---------------------------------------------------------------*
012700060428      * Reperimento Dati del job (Utente/Operativi)                   *
012800060428      *---------------------------------------------------------------*
012900060428     c     DatiJob       BEGSR
013000060428      *
013100060428     c     *dtaara       define    §azute        azuteds
013200060428     c     *dtaara       define    §datiute      ddatiute
013300060428      *
013400060428     c                   in(E)     *dtaara
013500060428     c                   IF        %ERROR or RSUT = *blanks
013600060428     c                   clear                   Tibs34Ds
013700060428     c                   call      'TIBS34R'
013800060428     c                   parm                    Tibs34Ds
013900060428     c                   in        *dtaara
014000060428     c                   ENDIF
014100060428      *
014200060428     c                   ENDSR
014300060428
014400060428      ****************************************************************
014500060428      ** GESVID0    ** Gestione videata per interrogazione          **
014600060428      ****************************************************************
014700060428     C     GESVID0       BEGSR
014800060428      *
014900060428     C                   exfmt     TRUL28W1
015000060428      *
015100060428      * F12 = Ritorno
015200060428if  1C                   if        *inKL
015300060428     C                   eval      $Video = '1'
015400060428e   1C                   endIF
015500060428      *
015600060428     C                   ENDSR
015700020220
015800020220      ****************************************************************
015900020221      ** GESVID1    ** Gestione videata per richiesta parametri     **
016000020220      ****************************************************************
016100020221     C     GESVID1       BEGSR
016200020220      *
016300020221if  1C                   if        not %open(TRUL28D)
016400020220     C                   open      TRUL28D
016500020221e   1C                   endif
016600020220      *
016700060502     C                   write     TRUL28T1
016800020220     C                   exfmt     TRUL28D1
016900020220      *
017000060428sel 1C                   select
017100020221      * F3/F12 = Fine
017200060428w   1C                   when      *inKC   or
017300020221     C                             *inKL
017400020220     C                   exsr      ENDPGM
017500060502      * F4 = Interrogazione "Tipi Modulo"
017600060502w   1C                   when      *inKD
017700060428     C                   eval      $Video = '0'
017800060428      * Invio
017900060428x   1C                   other
018000060428     C                   eval      $Video = '2'
018100060428e   1C                   endsl
018200020220      *
018300020220     C                   ENDSR
018400020220
018500020220      ****************************************************************
018600020221      ** GESVID2    ** Gestione videata per visualizzazione output  **
018700020220      ****************************************************************
018800020221     C     GESVID2       BEGSR
018900060428      *
019000060428      * Richiamo programma specifico
019100060428     C                   clear                   O28err
019200060428     C                   clear                   O28msg
019300060428     C                   clear                   O28ckd
019400060428     C                   clear                   VO28cod
019500060428     C                   exsr      CALLPGM
019600020220      *
019700060502     C                   write     TRUL28T1
019800020220     C                   exfmt     TRUL28D2
019900020220      *
020000060428sel 1C                   select
020100020221      * F3 = Fine
020200060428w   1C                   when      *inKC
020300020221     C                   eval      $Fine = *on
020400060428      * F12 = Ritorno
020500060428w   1C                   when      *inKL
020600060428     C                   eval      $Video = '1'
020700060428      * Invio
020800060428x   1C                   other
020900060428     C                   eval      $Video = '1'
021000060428      * - Inizializzazione campi di output a video e DS
021100060428     C                   clear                   VO28cod
021200060428e   1C                   endsl
021300020220      *
021400020220     C                   ENDSR
021500020219
021600020219      ****************************************************************
021700030404      ** CALLPGM ** Richiamo pgm per il calcolo del Check Digit     **
021800020219      ****************************************************************
021900030404     C     CALLPGM       BEGSR
022000020219      *
022100020221     C                   eval      $Err = *off
022200040709      *
022300060421sel 0C                   SELECT
022400060421      *
022500040709      * Effettuo test particolare x calcolo modulo 103 standard in quanto usa una DS diversa
022600090916w   0C***                WHEN      I28mod = '103'
022700090916w   0C                   WHEN      I28mod = 'E10'   or
022800090916     C                             I28mod = 'E13'
022900090916     C                   eval      xI284STD = I28mod
023000090916     C                   eval      xI284COD = VI28cod
023100040709     C                   call      'TRUL28R4'
023200090916     C                   parm                    TRUL28DSx
023300040709      * Testo se ok o errore
023400090916     C                   if        xO284err <> *blank
023500040709     C                   movel     *on           $Err
023600090916     C                   movel     xO284err      O28err
023700040709     C                   movel     MSG(1)        O28msg
023800040709     C                   else
023900090916     C                   movel     xO284COD      VO28cod
024000090916     C                   movel     xO284CKD      O28ckd
024100040709     C                   endif
024200060421      *
024300060421      * Effettuo test particolare x calcolo modulo 37/36 ISO 7064
024400060421w   0C                   WHEN      I28mod = 'M36'
024500060421     C                   eval      I284COD = VI28cod
024600060421     C                   call      'TRUL28R5'
024700060421     C                   parm                    TRUL28DS4
024800060421      * Testo se ok o errore
024900060421     C                   if        O284err <> *off
025000060421     C                   movel     *on           $Err
025100060421     C                   movel     O284err       O28err
025200060421     C                   movel     MSG(1)        O28msg
025300060421     C                   else
025400060421     C                   movel     O284COD       VO28cod
025500060421     C                   movel     O284CKD       O28ckd
025600060421     C                   endif
025700040709      *
025800040709      * ...altrimenti altri lanci
025900060421x   0C                   OTHER
026000060428      *
026100040709     C                   eval      I28COD  = VI28cod
026200030404      *
026300030404sel 1C                   select
026400020219      *
026500020221      * M10   - Segnacollo BARTOLINI: FLS LNA NRS NSC ZNC
026600030404w   1C                   when      I28mod = 'BAR'
026700030404     C                   eval      Wpgm   = 'TRUL28R1  '
026800020221      *
026900020221      * M1031 - Segnacollo DPD ("Chi Sono" BARTOLINI): AA/CC + 12 NUMERI
027000030404w   1C                   when      I28mod = 'DPD'
027100030404     C                   eval      Wpgm   = 'TRUL28R2  '
027200030407      *
027300030407      * FEDEX - Segnacollo FEDEX: 12 NUMERI
027400030407      *         (11 o '0'+10 o '00'+9 + Check Digit)
027500090916w   1C                   when      I28mod = 'FED'   or
027600090916     C                             I28mod = 'FE7'
027700030407     C                   eval      Wpgm   = 'TRUL28R3  '
027800030407      *
027900030407      * Il pgm. TRUL28R3 funge da "filtro" per i 2 pgm. TRUL28R3A e
028000030407      *   TRUL28R3B, per cui le due successive selezioni sono superflue.
028100030407      * Ma i 2 pgm. esistono e sono richiamabili singolarmente, per cui
028200030407      *   volendo...
028300030407      *   (se si sa già che tipo di calcolo fare: MOD 11 o MOD 7)
028400020221      *
028500030407      * FedEx MOD 11 - Segnacollo FEDEX: 12 NUMERI   (11 + Check Digit)
028600030407w   1C***                when      I28mod = 'FED'
028700030407     C***                eval      Wpgm   = 'TRUL28R3A '
028800030404      *
028900030407      * FedEx MOD 7  - Tracking Number lungo 10 o 11 (9 o 10 + Check Digit)
029000030407w   1C***                when      I28mod = 'FE7'
029100030407     C***                eval      Wpgm   = 'TRUL28R3B '
029200020219      *
029300030404e   1C                   endsl
029400020221      *
029500030404      * Richiamo pgm per controllo "specifico"
029600030404     C                   call      Wpgm
029700040709     C                   parm                    TRUL28DS
029800060113     C                   movel     O28cod        VO28cod
029900030404      *
030000020221if  1C                   if        O28err <> *blank
030100020221     C                   movel     *on           $Err
030200030404if  2C                   if        O28msg =  *blanks
030300030404     C                   movel     MSG(1)        O28msg
030400030404e   2C                   endif
030500020221e   1C                   endif
030600060421      *
030700060421e   0C                   ENDSL
030800020219      *
030900030404     C                   ENDSR
031000020219
031100020219      ****************************************************************
031200020219      ** ENDPGM ** Fine Programma
031300020219      ****************************************************************
031400020219     C     ENDPGM        BEGSR
031500020219      *
031600020219     C                   movel(p)  TRUL28ds      KPJBU
031700020220      *
031800020221     C                   if        %open(TRUL28D)
031900020220     C                   close     TRUL28D
032000020220     C                   endif
032100020221     C                   eval      $Video = *off
032200020219      *
032300060428if  1C                   if        I28tla = *blank
032400060428     C                   movel     *on           *inRT
032500020219x   1C                   else
032600060428     C                   movel     *on           *inLR
032700020219e   1C                   endif
032800020219     C                   return
032900020219      *
033000020219     C                   ENDSR
033100020219
033200020219** MSG
033300020219TRUL28R - Codice errato                                                        01
033400020221TRUL28R - Codice errato: contiene caratteri non ammessi (o blank)              02
033500020221TRUL28R - Codice errato: contiene caratteri oltre la lunghezza massima         03
033600020221TRUL28R - Caratteri iniziali del barcode errati                                04
