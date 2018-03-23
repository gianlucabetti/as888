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
002000140402     D TRUL28DS0     e ds                  inz
002100090916     D TRUL28DS4     e ds                  inz
002200090916     D TRUL28DSX     e ds                  inz  prefix(X)
002300020221      *
002400060428      * - Parametri x Controllo profilo utenti
002500060428     d TIBS34ds      e ds
002600060428      * - Ds di riferimento al file esterno AZUTE00F
002700060428     d AZUTEds       e ds                  extname(AZUTE00F)
002800060428      * - Ds per dati organigramma
002900060428     d DDatiUte      e ds
003000020221
003100020219      *
003200020219      * DS definite INTERNAMENTE
003300020219      *
003400020221
003500020220      *
003600020221      * Status DS
003700020220      *
003800060428     D Status         sds           333
003900060428     D  SDSpgm           *proc
004000060428     D  SDSprm           *parms
004100060428     D  SDSusr                81     90
004200020221
004300020219      *
004400020219      * VARIABILI
004500020219      *
004600020220     D WKPJBA          s                   like(KPJBA)
004700030404     D WPGM            s             10    inz
004800060428     D $Video          s              1    inz('1')
004900020221     D $Fine           s              1    inz(*off)
005000020221     D $Err            s              1    inz(*off)
005100020221
005200020219      *
005300020219      * COSTANTI
005400020219      *
005500020219     D Digits          c                   const('0123456789')
005600020219
005700020221      ****************************************************************
005800020221
005900020219      * Operazioni iniziali
006000020219     C                   exsr      OPEINI
006100020220      *
006200020221do  1C                   dow       $Fine = *off
006300020221      *
006400020221      * Eventuale emissione videata per richiesta parametri di input
006500060428cas 2C     $Video        caseq     '1'           GesVid1
006600060428cas 2C     $Video        caseq     '2'           GesVid2
006700060428cas 2C     $Video        caseq     '0'           GesVid0
006800020221e   2C                   endcs
006900020221      *
007000020221e   1C                   enddo
007100020219      *
007200020221      * Fine
007300020219     C                   exsr      ENDPGM
007400020219
007500020219      ****************************************************************
007600020219      ** OPEINI ** Operazioni Iniziali                              **
007700020219      ****************************************************************
007800020219     C     OPEINI        BEGSR
007900020221      *
008000020221     C     *ENTRY        plist
008100020221     C                   parm                    wKPJBA
008200020221      *
008300060428     C                   if        SDSprm = *zeros
008400020221     C                   clear                   KPJBA
008500060428     C                   eval      KNMUS = SDSusr
008600020221     C***                eval      KNSIF = ???
008700020221     C                   clear                   TRUL28DS
008800040709     C                   clear                   TRUL28DS4
008900020221     C                   else
009000020221     C                   movel     wKPJBA        KPJBA
009100020221     C                   movel     KPJBU         TRUL28DS
009200140402     C                   movel     KPJBU         TRUL28DS0
009300040709     C                   movel     KPJBU         TRUL28DS4
009400090916     C                   movel     KPJBU         TRUL28DSX
009500020221     C                   endif
009600040709      *
009700040709      * Inizializzazione campi di output a video e DS
009800060428     C                   clear                   O28err
009900060428     C                   clear                   O28msg
010000060428     C                   clear                   O28ckd
010100060428     C                   clear                   O28cod
010200060428     C                   clear                   O284cod
010300060428     C                   clear                   O284ckd
010400060428     C                   clear                   O284err
010500040709     C                   clear                   VO28cod
010600020221      *
010700020221     C                   reset                   $Err
010800020221     C                   reset                   $Fine
010900020221     C                   reset                   $Video
011000020221      *
011100020221      * Richiesta la sola chiusura del file:
011200020221cas 1C     I28tla        caseq     'C'           ENDPGM
011300020221e   1C                   endcs
011400020221      *
011500040709     C                   if            I28mod = *blanks
011600040709     C                             and VI28cod = *blanks
011700060428     C                   eval      $Video = '1'
011800020221     C                   endif
011900020220      *
012000020220     C                   eval      D2SEP  = *all'-'
012100060428     C                   eval      VTCPGM = SDSpgm
012200020221      *
012300060428      * Reperisco dati job
012400060428     c                   exsr      DatiJob
012500020219      *
012600020219     C                   ENDSR
012700060428      *
012800060428      *---------------------------------------------------------------*
012900060428      * Reperimento Dati del job (Utente/Operativi)                   *
013000060428      *---------------------------------------------------------------*
013100060428     c     DatiJob       BEGSR
013200060428      *
013300060428     c     *dtaara       define    §azute        azuteds
013400060428     c     *dtaara       define    §datiute      ddatiute
013500060428      *
013600060428     c                   in(E)     *dtaara
013700060428     c                   IF        %ERROR or RSUT = *blanks
013800060428     c                   clear                   Tibs34Ds
013900060428     c                   call      'TIBS34R'
014000060428     c                   parm                    Tibs34Ds
014100060428     c                   in        *dtaara
014200060428     c                   ENDIF
014300060428      *
014400060428     c                   ENDSR
014500060428
014600060428      ****************************************************************
014700060428      ** GESVID0    ** Gestione videata per interrogazione          **
014800060428      ****************************************************************
014900060428     C     GESVID0       BEGSR
015000060428      *
015100060428     C                   exfmt     TRUL28W1
015200060428      *
015300060428      * F12 = Ritorno
015400060428if  1C                   if        *inKL
015500060428     C                   eval      $Video = '1'
015600060428e   1C                   endIF
015700060428      *
015800060428     C                   ENDSR
015900020220
016000020220      ****************************************************************
016100020221      ** GESVID1    ** Gestione videata per richiesta parametri     **
016200020220      ****************************************************************
016300020221     C     GESVID1       BEGSR
016400020220      *
016500020221if  1C                   if        not %open(TRUL28D)
016600020220     C                   open      TRUL28D
016700020221e   1C                   endif
016800020220      *
016900060502     C                   write     TRUL28T1
017000020220     C                   exfmt     TRUL28D1
017100020220      *
017200060428sel 1C                   select
017300020221      * F3/F12 = Fine
017400060428w   1C                   when      *inKC   or
017500020221     C                             *inKL
017600020220     C                   exsr      ENDPGM
017700060502      * F4 = Interrogazione "Tipi Modulo"
017800060502w   1C                   when      *inKD
017900060428     C                   eval      $Video = '0'
018000060428      * Invio
018100060428x   1C                   other
018200060428     C                   eval      $Video = '2'
018300060428e   1C                   endsl
018400020220      *
018500020220     C                   ENDSR
018600020220
018700020220      ****************************************************************
018800020221      ** GESVID2    ** Gestione videata per visualizzazione output  **
018900020220      ****************************************************************
019000020221     C     GESVID2       BEGSR
019100060428      *
019200060428      * Richiamo programma specifico
019300060428     C                   clear                   O28err
019400060428     C                   clear                   O28msg
019500060428     C                   clear                   O28ckd
019600060428     C                   clear                   VO28cod
019700060428     C                   exsr      CALLPGM
019800020220      *
019900060502     C                   write     TRUL28T1
020000020220     C                   exfmt     TRUL28D2
020100020220      *
020200060428sel 1C                   select
020300020221      * F3 = Fine
020400060428w   1C                   when      *inKC
020500020221     C                   eval      $Fine = *on
020600060428      * F12 = Ritorno
020700060428w   1C                   when      *inKL
020800060428     C                   eval      $Video = '1'
020900060428      * Invio
021000060428x   1C                   other
021100060428     C                   eval      $Video = '1'
021200060428      * - Inizializzazione campi di output a video e DS
021300060428     C                   clear                   VO28cod
021400060428e   1C                   endsl
021500020220      *
021600020220     C                   ENDSR
021700020219
021800020219      ****************************************************************
021900030404      ** CALLPGM ** Richiamo pgm per il calcolo del Check Digit     **
022000020219      ****************************************************************
022100030404     C     CALLPGM       BEGSR
022200020219      *
022300020221     C                   eval      $Err = *off
022400040709      *
022500060421sel 0C                   SELECT
022600060421      *
022700040709      * Effettuo test particolare x calcolo modulo 103 standard in quanto usa una DS diversa
022800090916w   0C***                WHEN      I28mod = '103'
022900090916w   0C                   WHEN      I28mod = 'E10'   or
023000090916     C                             I28mod = 'E13'
023100090916     C                   eval      xI284STD = I28mod
023200090916     C                   eval      xI284COD = VI28cod
023300040709     C                   call      'TRUL28R4'
023400090916     C                   parm                    TRUL28DSx
023500040709      * Testo se ok o errore
023600090916     C                   if        xO284err <> *blank
023700040709     C                   movel     *on           $Err
023800090916     C                   movel     xO284err      O28err
023900040709     C                   movel     MSG(1)        O28msg
024000040709     C                   else
024100090916     C                   movel     xO284COD      VO28cod
024200090916     C                   movel     xO284CKD      O28ckd
024300040709     C                   endif
024400060421      *
024500060421      * Effettuo test particolare x calcolo modulo 37/36 ISO 7064
024600060421w   0C                   WHEN      I28mod = 'M36'
024700060421     C                   eval      I284COD = VI28cod
024800060421     C                   call      'TRUL28R5'
024900060421     C                   parm                    TRUL28DS4
025000060421      * Testo se ok o errore
025100060421     C                   if        O284err <> *off
025200060421     C                   movel     *on           $Err
025300060421     C                   movel     O284err       O28err
025400060421     C                   movel     MSG(1)        O28msg
025500060421     C                   else
025600060421     C                   movel     O284COD       VO28cod
025700060421     C                   movel     O284CKD       O28ckd
025800060421     C                   endif
025900140402      *
026000140402      * Effettuo test particolare x calcolo modulo BRT
026100140402w   0C                   WHEN      I28mod = 'BRT'
026200140402     C                   eval      I280COD = VI28cod
026300140402     C                   call      'TRUL28R0'
026400140402     C                   parm                    TRUL28DS0
026500140402      * Testo se ok o errore
026600140403     C                   movel     O280COD       VO28cod
026700140403     C                   clear                   O28ckd
026800140402     C                   if        O280err <> *off
026900140402     C                   movel     *on           $Err
027000140402     C                   movel     O280err       O28err
027100140402     C                   movel     MSG(1)        O28msg
027200140402     C                   endif
027300040709      *
027400040709      * ...altrimenti altri lanci
027500060421x   0C                   OTHER
027600060428      *
027700040709     C                   eval      I28COD  = VI28cod
027800030404      *
027900030404sel 1C                   select
028000020219      *
028100020221      * M10   - Segnacollo BARTOLINI: FLS LNA NRS NSC ZNC
028200030404w   1C                   when      I28mod = 'BAR'
028300030404     C                   eval      Wpgm   = 'TRUL28R1  '
028400020221      *
028500020221      * M1031 - Segnacollo DPD ("Chi Sono" BARTOLINI): AA/CC + 12 NUMERI
028600030404w   1C                   when      I28mod = 'DPD'
028700030404     C                   eval      Wpgm   = 'TRUL28R2  '
028800030407      *
028900030407      * FEDEX - Segnacollo FEDEX: 12 NUMERI
029000030407      *         (11 o '0'+10 o '00'+9 + Check Digit)
029100090916w   1C                   when      I28mod = 'FED'   or
029200090916     C                             I28mod = 'FE7'
029300030407     C                   eval      Wpgm   = 'TRUL28R3  '
029400030407      *
029500030407      * Il pgm. TRUL28R3 funge da "filtro" per i 2 pgm. TRUL28R3A e
029600030407      *   TRUL28R3B, per cui le due successive selezioni sono superflue.
029700030407      * Ma i 2 pgm. esistono e sono richiamabili singolarmente, per cui
029800030407      *   volendo...
029900030407      *   (se si sa già che tipo di calcolo fare: MOD 11 o MOD 7)
030000020221      *
030100030407      * FedEx MOD 11 - Segnacollo FEDEX: 12 NUMERI   (11 + Check Digit)
030200030407w   1C***                when      I28mod = 'FED'
030300030407     C***                eval      Wpgm   = 'TRUL28R3A '
030400030404      *
030500030407      * FedEx MOD 7  - Tracking Number lungo 10 o 11 (9 o 10 + Check Digit)
030600030407w   1C***                when      I28mod = 'FE7'
030700030407     C***                eval      Wpgm   = 'TRUL28R3B '
030800020219      *
030900030404e   1C                   endsl
031000020221      *
031100030404      * Richiamo pgm per controllo "specifico"
031200030404     C                   call      Wpgm
031300040709     C                   parm                    TRUL28DS
031400060113     C                   movel     O28cod        VO28cod
031500030404      *
031600020221if  1C                   if        O28err <> *blank
031700020221     C                   movel     *on           $Err
031800030404if  2C                   if        O28msg =  *blanks
031900030404     C                   movel     MSG(1)        O28msg
032000030404e   2C                   endif
032100020221e   1C                   endif
032200060421      *
032300060421e   0C                   ENDSL
032400020219      *
032500030404     C                   ENDSR
032600020219
032700020219      ****************************************************************
032800020219      ** ENDPGM ** Fine Programma
032900020219      ****************************************************************
033000020219     C     ENDPGM        BEGSR
033100020219      *
033200020219     C                   movel(p)  TRUL28ds      KPJBU
033300020220      *
033400020221     C                   if        %open(TRUL28D)
033500020220     C                   close     TRUL28D
033600020220     C                   endif
033700020221     C                   eval      $Video = *off
033800020219      *
033900060428if  1C                   if        I28tla = *blank
034000060428     C                   movel     *on           *inRT
034100020219x   1C                   else
034200060428     C                   movel     *on           *inLR
034300020219e   1C                   endif
034400020219     C                   return
034500020219      *
034600020219     C                   ENDSR
034700020219
034800020219** MSG
034900020219TRUL28R - Codice errato                                                        01
035000020221TRUL28R - Codice errato: contiene caratteri non ammessi (o blank)              02
035100020221TRUL28R - Codice errato: contiene caratteri oltre la lunghezza massima         03
035200020221TRUL28R - Caratteri iniziali del barcode errati                                04
