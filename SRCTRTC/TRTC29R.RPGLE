000100030127      *---------------------------------------------------------------*
000200050826      *    - PREPARAZIONE DEL FILE FIVAI PER INVIO A CLIENTI -        *
000300030123      *---------------------------------------------------------------*
000400030123
000500030123      *****************************************************************
000600030123      *  RIEPILOGO INDICATORI                                         *
000700030123      *****************************************************************
000800030211      * 21    - Comodo                                                *
000900030127      * 50-51 - Tipo record letto dal file FIAR532C                   *
001000030123      * 97    - Membro non disponibile                                *
001100030123      *****************************************************************
001200030123
001300030123     h decedit('0,') datedit(*dmy.)
001400030123
001500030123      *---------------------------------------------------------------*
001600030123
001700030123     fTNTBE01L  If   e           k disk
001800030123     fTABEL00F  If   e           k disk
001900030123     fTITAS30C  If   e           k disk
002000030123     fTITA430C  If   e           k disk
002100030123      *
002200030123     fFIAR532C  Uf   e           k disk
002300030123      *
002400050826     FTIVGD00F  O    E             DISK
002500030123
002600030123      *---------------------------------------------------------------*
002700030123
002800030123      * Costanti :
002900030123
003000030123      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003100030123
003200030123      * Schiere :
003300030123      *
003400030127      * - Schiera tipi record da trasmettere AL CLIENTE
003500030127     d AR5s            s                   dim(50)  like(AR5trd)
003600030127      * - Schiera tipi record da NON trasmettere AL CLIENTE
003700030127     d AR5n            s                   dim(50)  like(AR5trd)
003800030123
003900030123      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004000030123
004100030123      * Strutture Dati :
004200030123      *
004300030123     d KPJBA         e ds
004400030123      * - Ds x definizione tipi record FIAR5
004500030123     d dAR5          e ds                  inz
004600030123      * - Ds x estensione  tipi record di FIAR5
004700030123     d dAR5scr       e ds                  inz
004800030123     d dAR5ban       e ds                  inz
004900030123      * - Ds x dati variabili (VAIdti) di FIVAI
005000030123     d dVAImac       e ds                  inz  prefix(M)
005100030123     d dVAIban       e ds                  inz  prefix(B)
005200030123      * - Ds x riferimenti da file TITA400F tipo rec. "A"
005300030123     d dTA4A         e ds                  inz
005400030123      * - Ds x dati da/per clienti in filiale
005500050826     d ds3K          e ds                  inz
005600030123      *
005700030123     d WLBDA8          ds                  inz
005800030123     d  G08DAT                 1      8  0 inz
005900030123     d  G08INV                 9     16  0 inz
006000030123     d  G08ERR                17     17    inz
006100030123     d  G08TGI                18     22  0 inz
006200050826      *
006300050826      * - Ds ridefinizione file FIVAI x scrittura flat su TIVGD00F
006400050826     D fivaids       e ds                  extname(fivai00t)
006500050907      *
006600050907      * - Ds d procedura x TRUL47R
006700050907     D trul47ds      e ds
006800030123
006900030123      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
007000030123
007100030123      * Variabili
007200030123      *
007300030123      * - Flag
007400030123     d $Err            s              1    inz(*off)
007500030123     d $Inv            s              1    inz(*off)
007600030123      * - Indici di schiera
007700030123     d XX              s              3  0 inz
007800030127     d YY              s              3  0 inz
007900030123      * - Campi per test "rotture"
008000030123     d SAVccm          s                   like(TASccm) inz(*loval)
008100030123     d NEWccm          s                   like(TASccm) inz
008200030123      * - Campi di comodo
008300030123     d Wn14            s             14  0
008400030123     d Wn08            s              8  0
008500030123     d Woggi           s              8  0
008600030123
008700030123      *---------------------------------------------------------------*
008800030123
008900030123     iFIAR5000      50
009000030123     iFIAR5P00      51
009100030123
009200030123      *---------------------------------------------------------------*
009300030123
009400050907     C*
009500050907     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VI'
009600050907     C                   clear                   trul47ds
009700050907     C                   eval      d47opz  = 'I'
009800050907     C                   eval      d47tip  = 'VI'
009900050909     C                   eval      d47lck  = 'N'
010000050909     C                   eval      d47chkj = 'N'
010100050907     C                   eval      d47pgm  = 'TRTC29R'
010200050907     C                   call      'TRUL47R'
010300050907     C                   parm                    trul47ds
010400050907     C*
010500050907     C* Se elaborazione consentita => proseguo
010600050909     C                   if        d47sts <> 'A'
010700050907     C*
010800030123      * Ciclo di lettura del file FIAR5 (Estensione testata bolla)
010900030123      * (la vista logica esclude i record già estratti - AR5FT3 <> ' ')
011000030123     c     *loval        setll     FIAR532C
011100030123     c                   read      FIAR532C
011200030123      *
011300030123do  1c                   dow       Not %Eof(FIAR532C)
011400030123      *
011500030123     c                   exsr      Ctr_Rec
011600030123      *
011700030127s   2C                   SELECT
011800030127      * Se ho un errore, manca tipo record in tabella AR5 o manca TITAS,
011900030127      * => non flaggo il record e leggo il successivo
012000030127    2c                   WHEN      $Err = *on
012100030127      * Se il tipo record di FIAR5 deve essere trasmesso al mittente
012200050826      * => registro un record nel file TIVGD00F (file VAS Generico Download)
012300030127      *     e aggiorno il record di FIAR5 come "elaborato"
012400030127      *     a meno di errori di allocazione
012500030127    2c                   WHEN      $Inv = *on
012600050826     c                   exsr      Wrt_FIVAI
012700030127     C                   If        $ERR = *off
012800030127     c                   exsr      Upd_FIAR5
012900030127     c                   Endif
013000030127      * Se il tipo record di FIAR5 NON deve essere trasmesso al mittente
013100030127      * => aggiorno il record di FIAR5 come "elaborato"
013200030127x   2c                   OTHER
013300030123     c                   exsr      Upd_FIAR5
013400030127e   2c                   ENDSL
013500030123      *
013600030211     c                   movea     '00'          *in(50)
013700030123     c                   read      FIAR532C
013800030123      *
013900030123e   1c                   enddo
014000050907     C*
014100050907     C                   endif
014200050907     C*
014300050907     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VI'
014400050907     C                   clear                   trul47ds
014500050907     C                   eval      d47opz  = 'F'
014600050907     C                   eval      d47tip  = 'VI'
014700050907     C                   call      'TRUL47R'
014800050907     C                   parm                    trul47ds
014900050907     C*
015000030123      *
015100030123      * Fine
015200030123      *
015300030123     c                   eval      *inLR = *on
015400030123
015500030123      **-------------------------------------------------------------**
015600030123      ** Operazioni iniziali                                         **
015700030123      **-------------------------------------------------------------**
015800030123     c     *InzSR        BEGSR
015900030123      *
016000030123     c     *Entry        Plist
016100030123     c                   parm                    KPJBA
016200030123      *
016300030123      * Chiavi di lettura:
016400030123      * - TITAS30C > parziale (4 campi chiave su 5)
016500030123     c     K04TAS30      klist
016600030123     c                   kfld                    AR5aas
016700030123     c                   kfld                    AR5lnp
016800030123     c                   kfld                    AR5nrs
016900030123     c                   kfld                    AR5nsp
017000030123      * - TITA430C > completa (5 campi chiave)
017100030123     c     K05TA430      klist
017200030123     c                   kfld                    AR5aas
017300030123     c                   kfld                    AR5lnp
017400030123     c                   kfld                    AR5nrs
017500030123     c                   kfld                    AR5nsp
017600030123     c                   kfld                    ta4trc
017700030123     c                   eval      TA4trc = 'A'
017800030123      * - TABEL00F > completa (3 campi chiave)
017900030123     c     KTAB          klist
018000030123     c                   kfld                    TBLkut
018100030123     c                   kfld                    TBLcod
018200030123     c                   kfld                    TBLkey
018300030123     c                   eval      TBLkut = 1
018400030123     c                   eval      TBLcod = '3K'
018500030123      *
018600030123      * calcola la data corrente
018700030123     c                   time                    Wn14                           *ora(6) + data(8)
018800030123     c                   move      Wn14          Wn08                           *data(8) in g/m/aa
018900030123     c                   z-add     Wn08          G08dat
019000030123     c                   clear                   G08inv
019100030123     c                   movel     *off          G08err
019200030123     c                   call      'XSRDA8'
019300030123     c                   parm                    WLBDA8
019400030123     c                   z-add     G08inv        Woggi                          *d.corrente aa/m/g
019500030123      *
019600030123      *  Effettuo caricamento dei tipi record da trasmettere
019700030123     c                   clear                   xx
019800030127     c                   clear                   yy
019900030127     c                   clear                   AR5s
020000030127     c                   clear                   AR5n
020100030123     c                   eval      TBEcod   = 'AR5'
020200030123     c     TBEcod        setll     TNTBE000
020300030123     c     TBEcod        reade     TNTBE000
020400030127do  1c                   DOW       Not %Eof(TNTBE01L)
020500030123if  2c                   if        TBEatb   = *blanks
020600030127     c                   eval      dAR5 = TBEuni
020700030127if  3c                   IF        §AR5trmi = 'S'
020800030123     c                   add       1             xx
020900030127     c                   movel     TBEke1        AR5s(xx)
021000030127x   3C                   ELSE
021100030127     c                   add       1             yy
021200030127     c                   movel     TBEke1        AR5n(yy)
021300030127e   3c                   ENDIF
021400030127e   2c                   endif
021500030123     c     TBEcod        reade     TNTBE000
021600030127e   1c                   ENDDO
021700030123      *
021800030123     c                   ENDSR
021900030123
022000030123      **-------------------------------------------------------------**
022100030123      ** Controlli per ogni record letto dal file FIAR532C           **
022200030123      **-------------------------------------------------------------**
022300030123     c     Ctr_Rec       BEGSR
022400030123      *
022500030123     c                   reset                   $Err
022600030123     c                   reset                   $Inv
022700030204      *
022800030204      * Per i record di tipo "BAN" - se DAR5RB1 e DAR5RB2 = 0 :
022900030204      * non trasmetto (ma fleggo come trasmesso al cliente)
023000030204if  1c                   if        AR5trd = 'BAN'
023100030204     c                   movel     AR5uni        dAR5ban
023200030204if  2c                   if             §AR5rb1 = 0
023300030204     c                             and  §AR5rb2 = 0
023400030204     c*** (lo è già:)    eval      $Inv = *off
023500030204     c                   goto      F_CtrRec
023600030204e   2c                   endif
023700030204e   1c                   endif
023800030123      *
023900030123      * Controllo se il tipo record deve essere trasmesso al mittente
024000030127      * (se non reperisco la tab. "AR5" non elaboro il record e non flaggo)
024100030123     c                   eval      *in30  =  *off
024200030123     c                   eval      xx     =  1
024300030127     c     AR5trd        lookup    AR5s(xx)                               30
024400030127if  1c                   IF        *in30
024500030127     c                   eval      $Inv   =  *on
024600030127x   1c                   ELSE
024700030127     c     AR5trd        lookup    AR5n(xx)                               30
024800030127if  2c                   If        *in30 = *off
024900030123     c                   eval      $Err   =  *on
025000030127e   2c                   Endif
025100030127e   1c                   ENDIF
025200030123     c     $Inv          cabeq     *off          F_CtrRec
025300030123      *
025400030123      * Aggancio file bolle per reperire il codice del mittente
025500030127      * (se non reperisco la bolla non elaboro il record e non flaggo)
025600030123     c     K04TAS30      chain     TITAS30C
025700030123if  1c                   if        %Found(TITAS30C)
025800030123if  2c                   if        SAVccm <> TASccm
025900030123     c                   eval      SAVccm =  TASccm
026000030123     c                   clear                   ds3K
026100030123     c                   movel(p)  TASccm        TBLkey
026200030123     c     KTAB          chain     TABEL
026300030123if  3c                   if            %Found(TABEL00F)
026400030123     c                             and TBLflg = *blanks
026500030123     c                   movel     TBLuni        ds3K
026600030123e   3c                   endif
026700030123e   2c                   endif
026800030123x   1c                   else
026900030123     c                   eval      $Err   =  *on
027000030123     c                   eval      $Inv   =  *off
027100030123e   1c                   endif
027200030123     c     $Inv          cabeq     *off          F_CtrRec
027300030123      *
027400030123      * Controllo se i dati cons. in esame devono essere trasmessi
027500030123s   1c                   select
027600030123    1c                   when      AR5trd = 'SCR'  and  §3Kcma = 'S'
027700030123     c                   eval      $Inv   =  *on
027800030123    1c                   when      AR5trd = 'BAN'  and  §3Kcba = 'S'
027900030123     c                   eval      $Inv   =  *on
028000030123x   1c                   other
028100030123     c                   eval      $Inv   =  *off
028200030123e   1c                   endsl
028300030123     c     $Inv          cabeq     *off          F_CtrRec
028400030123      *
028500030123     c     F_CtrRec      ENDSR
028600030123
028700030123      **-------------------------------------------------------------**
028800050826      ** Scrittura record nel file FIVAI                             **
028900030123      **-------------------------------------------------------------**
029000050826     c     Wrt_FIVAI     BEGSR
029100030123      *
029200050826     c                   reset                   $Err
029300030123      *
029400030123      * Imposto i campi del nuovo record
029500050826     c                   exsr      Rie_FIVAI
029600050826      *
029700050826      * Scrivo il file TIVGD00F (file VAS Generico Download)
029800050826     c                   exsr      WRIVGD
029900030123      *
030000030123     c                   ENDSR
030100030123
030200030123      **-------------------------------------------------------------**
030300050826      ** Riempimento campi del nuovo record di FIVAI                 **
030400030123      **-------------------------------------------------------------**
030500030123     c     Rie_FIVAI     BEGSR
030600030123      *
030700030123      * Aggancio TITA430C per recuperare i riferimenti (tipo rec. "A")
030800030123     c     K05TA430      chain     TITA430C
030900030123if  1c                   if        %found(TITA430C)
031000030123     c                   movel     TA4not        dTA4A
031100030123x   1c                   else
031200030123     c                   clear                   dTA4A
031300030123e   1c                   endif
031400030123      *
031500030123      * Imposto i campi del record in output
031600050826     c                   clear                   FIVAIDS
031700030127      *
031800030127      *  CAMPI "fissi"
031900030123     c                   eval      VAIccm = TASccm
032000030123     c                   eval      VAIaas = AR5aas
032100030123     c                   eval      VAIlnp = AR5lnp
032200030123     c                   eval      VAInrs = AR5nrs
032300030123     c                   eval      VAInsp = AR5nsp
032400030123     c                   eval      VAIrmn = TASrmn
032500030123     c                   eval      VAIrma = §TA4Arma
032600030127     c                   eval      VAIxxx = 'X'
032700030127      *
032800030127      *  CAMPI "variabili"
032900030127s   1c                   SELECT
033000030127      *
033100030127      * "MAC" - modifica accordi consegna
033200030127    1c                   WHEN      AR5trd = 'SCR'
033300030123     c                   eval      VAIdac = AR5dac
033400030123     c                   eval      VAIorc = AR5orc
033500030123     c                   eval      VAItdt = 'MAC'
033600030127      * - imposto i campi della ds dVAImac con quelli della ds dAR5scr
033700030127     c                   clear                   dVAImac
033800030127     c                   movel     AR5uni        dAR5scr
033900030127     c                   eval      m§VAIcvb = §AR5cvb
034000030127     c                   eval      m§VAIpou = §AR5pou
034100030127     c                   eval      m§VAItcr = §AR5tcr
034200030127     c                   eval      m§VAIdcr = §AR5dcr
034300030127     c                   eval      m§VAIhcr = §AR5hcr
034400030127     c                   eval      m§VAIgc1 = §AR5gc1
034500030127     c                   eval      m§VAIgc2 = §AR5gc2
034600030127     c                   eval      m§VAItc1 = §AR5tc1
034700030127     c                   eval      m§VAItc2 = §AR5tc2
034800030127     c                   eval      m§VAInom = §AR5nom
034900030127     c                   eval      m§VAItel = §AR5tel
035000030127     c                   eval      m§VAImot = §AR5mot
035100030127     c                   movel     dVAImac       VAIdti
035200030127      *
035300030127      * "BAN" - gestione bancali consegna
035400030127    1c                   WHEN      AR5trd = 'BAN'
035500030123     c                   eval      VAIdac = §AR5dav
035600030123     c                   eval      VAIorc = §AR5orv
035700030123     c                   eval      VAItdt = 'BAN'
035800030127      * - imposto i campi della ds dVAIban con quelli della ds dAR5ban
035900030127     c                   clear                   dVAIban
036000030127     c                   movel     AR5uni        dAR5ban
036100030127     c                   eval      b§VAItba = §AR5tba
036200030127     c                   eval      b§VAInba = §AR5nba
036300030127     c                   eval      b§VAItb2 = §AR5tb2
036400030127     c                   eval      b§VAInb2 = §AR5nb2
036500030127     c                   eval      b§VAIrb1 = §AR5rb1
036600030127     c                   eval      b§VAIrb2 = §AR5rb2
036700030127     c                   eval      b§VAInom = §AR5nomb
036800030127     c                   eval      b§VAImot = §AR5motb
036900030127     c                   movel     dVAIban       VAIdti
037000030127e   1c                   ENDSL
037100030123      *
037200030123     c                   ENDSR
037300030123
037400030123      **-------------------------------------------------------------**
037500030123      ** Aggiornamento record del file FIAR532C                      **
037600030123      **-------------------------------------------------------------**
037700030123     c     Upd_FIAR5     BEGSR
037800030123      *
037900030123     c                   eval      AR5ft3 = 'T'
038000030123     c                   eval      AR5dt3 = Woggi
038100030123      *
038200030123     c   50              Update    FIAR5000
038300030123     c   51              Update    FIAR5P00
038400030123      *
038500030123     c                   ENDSR
038600050826
038700050826
038800050826
038900050826      /TITLE Scrittura record FIVAI in file TIVGD00F (file VAS generico download)
039000050826     C     wriVGD        BEGSR
039100050826      *
039200050826      * imposto il codice del cliente per il nome del membro
039300050826     c                   if        §3Kcks <> *zeros
039400050826     c                   eval      NEWccm =  §3Kcks
039500050826     c                   else
039600050826     c                   eval      NEWccm =  TASccm
039700050826     c                   endif
039800050826     C*
039900050826     C                   clear                   tivgd000
040000080404     C                   eval      vgdDTA = %subst(FIVAIDS:1:%size(FIVAIDS))
040100050826     C                   eval      vgdTIP = 'VI'
040200050826     C                   eval      vgdKSU = *all'0'
040300050826     C                   move      NEWccm        vgdKSU
040400050826     C                   eval      vgdTSC = 'WW'
040500050826     C                   eval      vgdDAT = Woggi
040600050826     C                   eval      vgdPGM = 'TRTC29R'
040700050826     C                   write     tivgd000
040800050826     C*
040900050826     C                   ENDSR
041000050826     C*------------------------------------------------------------------------*
