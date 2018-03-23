000100001102      *--------------------------------------------------------------------------------------------*
000200001102      * Creazione FNVAG da fasi C.A.
000300001102      *--------------------------------------------------------------------------------------------*
000400020912      * Elaboro fasi flaggate che siano create da un P.O. presente nel
000500020912      *   Sistema Informativo
000600001102      * La C.A. non deve essere chiusa
000700001102      *--------------------------------------------------------------------------------------------*
000800001102
000900001102      ****************************************************************
001000001102      *  FASI C.A. FISSATE A PROGRAMMA  (ricercare con 0 davanti)
001100001102      ****************************************************************
001200001102      *    900 = Chiusura C.A.                                (WFcch)
001300001102      ****************************************************************
001400001102
001500001102     H DECEDIT('0,') DATEDIT(*DMY.)
001600001102
001700001102      ****************************************************************
001800980929      *  RIEPILOGO INDICATORI
001900980929      ***************************************************************
002000020912      * 10   - comodo
002100980929      * 30   - CHAIN FNARB/FNBLP
002200001102      * 46   - presenza in 3K
002300001102      * 90   - Trovate tabelle decodifica
002400001102      * 91   - Mancata allocazione file
002500001102      ****************************************************************
002600001102
002700001102     fFNDCF02L  UF   E           K DISK
002800001102     fFNBLP01L  IF   E           K DISK
002900001106     fFNDCT01L  IF   E           K DISK
003000050610     ftivgd00f  O  A E             DISK
003100001102     fTABEL00F  IF   E           K DISK
003200001102     fTNTBE01L  IF   E           K DISK
003300010528     fFNEVB04L  IF   E           K DISK
003400001102
003500020912      *----------------
003600020912      * Schiere
003700020912      *----------------
003800020912     d SC_Ktad         S                   like(TbeKe1)   DIM(30)
003900020912     d SC_Ragtad       S                   like(§TADRAGR) DIM(30)
004000021004     d SC_K3k          S                   like(TblKey)   DIM(990)
004100050610     d sc_uni3K        S                   like(Tbluni)   DIM(990)
004200021004     d SC_cks3K        S              7                   DIM(990)
004300020912     d SC_K2a          S                   like(TblKey)   DIM(200)
004400020912     d SC_Des2a        S                   like(§2ADES)   DIM(200)
004500020912      * schiera dei P.O. presenti sull'AS/400
004600020912     d sc_POA          s              3  0 dim(250) inz
004700020912      * schiera dei P.O. presenti nel Sistema Informativo
004800020912     d sc_POS          s              3  0 dim(250) inz
004900001102      *----------------
005000020912      * Strutture dati definite esternamente
005100020912      *----------------
005200020912     D KPJBA         E DS                                                       *architettura
005300020912     D UT§DSE0F      E DS
005400020912     D cncr80        E DS
005500020912     D DTAD          E DS
005600020912     D DS2A          E DS
005700020912     D DS3K          E DS
005800050610     d fnvagds       e ds
005900020912     d TIBS56DS      e ds
006000020912     d  ski                    7    756  0 dim(250)
006100020912      *----------------
006200020912      * Strutture dati definite internamente
006300020912      *----------------
006400020912     D DSmembro        ds             8    inz
006500020912     D  MbrM                   1      1    inz('M')
006600020912     D  MbrCKS                 2      8    inz(*zeros)
006700001102      *----------------
006800020912      * Variabili
006900020912      *----------------
007000001102     D WFcch           S                   LIKE(DCTfca)   INZ(900)
007100001102      *----------------
007200020912     d WtbeCod         S                   like(TbeCod)  inz('TAD')
007300020912     d Wtbeke1         S                   like(TbeKe1)
007400020912     d Wtblkey         S                   like(TblKey)
007500001102     D KUTE            S                   like(TBLKUt)  inz(1)
007600001102     D KCOD            S                   like(TBLCOD)
007700001102     d XX              s              3  0
007800001102     d YY              s              3  0
007900020912     d $POA            s              1    inz(*off)
008000020912     d $POS            s              1    inz(*off)
008100050610     d dataiso         s               d   datfmt(*iso)
008200020912      *----------------
008300020912      * Costanti
008400001102      *--------------------------------------------------------------------------------------------*
008500001102
008600001102     c     *loval        setll     Fndcf000
008700001102     c                   read      Fndcf000
008800001102      *
008900001102     c                   DOW       not %EOF(fndcf02l)
009000001102      *
009100020912      * Controllo che il P.O. che ha creato la fase sia nello stesso
009200020912      *   AS/400
009300020912     c                   eval      *in10 = *off
009400020912     c     DcfFev        lookup    sc_POA                                 10
009500020912     c                   movel     *in10         $POA
009600020912      *
009700020912      * Controllo che il P.O. che ha creato la fase sia nello stesso
009800020912      *   Sistema Informativo
009900020912     c                   eval      *in10 = *off
010000020912     c     DcfFev        lookup    sc_POS                                 10
010100020912     c                   movel     *in10         $POS
010200001102      *
010300001102      * Aggancio la C.A.
010400001102     c     Keynca        chain     FNDCT000
010500001102      *
010600021004     C                   clear                   BLPccm
010700001102      * Aggancio la spedizione in partenza
010800001102     c     Kbol01        chain     FNBLP000
010900001102      *
011000001102      * Cerco il codice nella 3K
011100001102     c                   If        BLPccm > *zeros
011200001102     c                   movel(p)  BLPCCM        WTblKey
011300001102     c                   Else
011400001102     c                   movel(p)  BLPKSC        WTblKey
011500001102     c                   EndIf
011600001102      *
011700001102     c                   eval      xx = 1
011800001102     c     WTBLKEY       lookup    SC_K3K(xx)                             46
011900050610     c   46              movel     sc_uni3k(xx)  ds3k
012000050610
012100021004     C                   SELECT
012200021004      *  - FLEGGO x fase creata da P.O. non presente nell'AS400
012300021004     C                   WHEN      $POA = *off
012400021004     c                   exsr      SBR_flag
012500021004      *  - NON ELABORO x fase creata da P.O. non presente nel S.I.
012600021004     c                   WHEN      $POS = *Off
012700021004      *  - FLEGGO x C.A. non trovata
012800021004     C                   WHEN      NOT %found(FNDCT01L)
012900021004     c                   exsr      SBR_flag
013000021004      *  - FLEGGO x bolla non trovata
013100021004     C                   WHEN      NOT %found(FNBLP01L)
013200021004     c                   exsr      SBR_flag
013300021004      *  - FLEGGO x C.A. chiusa
013400021004     C                   WHEN      DCTFCA = WFCCH
013500021004     c                   exsr      SBR_flag
013600021004      *  - FLEGGO x Cliente non presente in 3K
013700021004     C                   WHEN      *IN46 = *off
013800021004     c                   exsr      SBR_flag
013900021004      *  - ELABORO
014000021004     C                   OTHER
014100050610      *  scrivo rcd di FNVAG
014200050610     c                   exsr      SBR_WRTVAG
014300021004     c                   ENDSL
014400021004      *
014500001102     c                   read      Fndcf000
014600001102     c                   ENDDO
014700001106      *
014800001102     C                   eval       *inLR = *on
014900001102      *--------------------------------------------------------------------------------------------*
015000021004      * Aggiorno FNDCF
015100001102      *--------------------------------------------------------------------------------------------*
015200021004     C     SBR_flag      BEGSR
015300001102      *
015400021004     c                   eval      %subst(dcflet:1:1) = 'T'
015500021004     c                   update    FNDCF000
015600021004      *
015700021004     c                   ENDSR
015800021004      *--------------------------------------------------------------------------------------------*
015900021004      * Scrivo rcd di FNVAG
016000021004      *--------------------------------------------------------------------------------------------*
016100021004     C     SBR_WRTVAG    BEGSR
016200021004      *
016300050610     c                   clear                   FNVAGds
016400050610     c                   clear                   tivgd000
016500001102      *
016600001102     c                   eval      YY = 1
016700001102     c                   movel     DCTTAD        Wtbeke1
016800001102     c     Wtbeke1       lookup    SC_KTAD(yy)                            99
016900001102      *
017000001102     c                   IF        *IN99 = *on
017100001102     c                   eval      wTblKey = 'W' + SC_ragtad(yy)
017200001102     c                   If        dctnev > *zeros
017300001102     c                   eval      %subst(wTblKey:3:1) = 'E'
017400001102     c                   Endif
017500001102     c                   eval      YY = 1
017600001102     c     Wtblkey       lookup    SC_K2A(yy)                             99
017700001102      *
017800001102     c                   IF        *IN99 = *on
017900001102     c                   eval      VagAas = BlpAas
018000001102     c                   eval      VagLnp = BlpLnp
018100001102     c                   eval      VagNrs = BlpNrs
018200001102     c                   eval      VagNsp = BlpNsp
018300001102     c                   eval      VagLna = BlpLna
018400001102     c                   eval      VagCmc = WtblKey
018500001102     c                   eval      VagRmn = BlpRmn
018600001102     c                   eval      VagRma = BlpRma
018700001102     c                   eval      VagAgc = DctAac
018800001102     c                   eval      VagMgc = DctMgc
018900001106     c                   eval      VagDmc = SC_Des2a(yy)
019000001106     c                   eval      VagNot = 'nr. colli coinvolti ' +
019100001106     c                                       %editc(DctNcn:'2')
019200001102     c                   If        BLPccm > *zeros
019300001102     c                   z-add     BLPCCM        VagScm
019400001102     c                   Else
019500001102     c                   z-add     BLPKSC        VagScm
019600001102     c                   EndIf
019700001102      *
019800010528     c                   movel     'R  '         evbcev
019900010528     c     Kfnevb        chain     fnevb000                           31
020000001102      *
020100010528     C     *in31         Ifeq      *off
020200001102     c                   eval      VagDmm = EvbDev
020300001102     c                   Endif
020400001102      *
020500080404     c                   eval      vgddta = %subst(fnvagds:1:%size(fnvagds))
020600050610     c                   move      'VG'          vgdtip
020700050719     c                   movel     *all'0'       vgdksu
020800050610     c                   move      §3kcks        vgdksu
020900050610     c                   move      §3kcsd        vgdtsc
021000050610     c                   eval      vgdpgm = 'FIDN98R'
021100050610     c                   move      *date         dataiso
021200050610     c                   move      dataiso       vgddat
021300050610     C                   WRITE     tivgd000
021400001106      * Aggiorno fndcf00f
021500021004     c                   EXSR      SBR_flag
021600001102      *
021700001102     c                   ENDIF
021800001102     c                   ENDIF
021900001102      *
022000001102     c                   ENDSR
022100001102      *--------------------------------------------------------------------------------------------*
022200001102      * *inzsr - operazioni iniziali
022300001102      *--------------------------------------------------------------------------------------------*
022400980828     C     *inzsr        BEGSR
022500001102      *
022600001102      * ricevimento parametri
022700980828     C     *entry        PLIST
022800980828     C                   PARM                    kpjba
022900001102      *
023000980915     C                   CALL      'X§PARUT'
023100001102     C                   PARM                    UT§DSE0F
023200980915     C                   MOVEL     REC80         CNCR80
023300020912      ***
023400020912      * Carico la schiera dei P.O. presenti sull'AS/400
023500020912      ***
023600020912     c                   clear                   TIBS56DS
023700020912     c                   eval      I56ppo = SIMfel
023800020912     c                   eval      I56mod = 'POA'
023900020912     c                   call      'TIBS56R'
024000020912     c                   parm                    TIBS56DS
024100020912     c                   if        O56err <> *blanks
024200020912     c                   clear                   SKI
024300020912     c                   endif
024400020912     c                   z-add     SKI           sc_POA
024500020912      ***
024600020912      * Carico la schiera dei P.O. presenti nel S.I.
024700020912      ***
024800020912     c                   clear                   TIBS56DS
024900020912     c                   eval      I56ppo = SIMfel
025000020912     c                   eval      I56mod = 'POS'
025100020912     c                   call      'TIBS56R'
025200020912     c                   parm                    TIBS56DS
025300020912     c                   if        O56err <> *blanks
025400020912     c                   clear                   SKI
025500020912     c                   endif
025600020912     c                   z-add     SKI           sc_POS
025700001102      ***
025800001102      * CARICO TABELLA 2A
025900001102      ***
026000001102     C                   clear                   xx
026100001102     C                   eval      KCOD = '2A'
026200001102     C     KTAB          SETLL     TABEL
026300001102     C     KTAB          READE     TABEL
026400001102      *
026500001102     C                   dow       NOT %eof(tabel00f)
026600001102     C                   if        TBLFLG = *blanks
026700001102     C                   ADD       1             xx
026800001102     C                   MOVEL     TBLUNI        DS2A
026900001102     C                   MOVEA     TBLKEY        sc_K2a(xx)                     SECONDARIA
027000001102     C                   MOVEA     §2ADES        sc_Des2a(xx)
027100001102     C                   endif
027200001102     C     KTAB          READE     TABEL
027300001102     C                   enddo
027400001102      ***
027500001102      * CARICO TABELLA 3K solo abilitati alla ricezione C.A.
027600001102      ***
027700001102     C                   clear                   xx
027800001102     C                   eval      KCOD = '3K'
027900001102     C     KTAB          SETLL     TABEL
028000001102     C     KTAB          READE     TABEL
028100001102      *
028200001102     C                   dow       NOT %eof(tabel00f)
028300001102     C                   MOVEL     TBLUNI        DS3K
028400001102     C                   if        TBLFLG = *blanks  and  §3kcq7 = 'S'
028500001102     C                   ADD       1             xx
028600001102     C                   MOVEA     TBLKEY        sc_K3k(xx)                     SECONDARIA
028700001106     C                   MOVEL     §3kcks        sc_Cks3K(xx)
028800050610     C                   MOVEL     tbluni        sc_uni3K(xx)
028900001102     C                   endif
029000001102     C     KTAB          READE     TABEL
029100001102     C                   enddo
029200001102      ***
029300001102      * CARICO TABELLA TAD
029400001102      ***
029500001102     c                   clear                   xx
029600001102     c     Wtbecod       setll     tntbe000
029700001102     c     Wtbecod       reade     tntbe000
029800001102      *
029900001102     c                   DOW       NOT %EOF(tntbe01l)
030000001102      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
030100001102     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
030200001102      *                            and TBEatb = *BLANKS
030300001102     c                   add       1             xx
030400001102     c                   movel     TBEuni        DTAD
030500001102     c                   movea     TBEke1        SC_Ktad(XX)
030600001102     c                   movea     §TadRagr      SC_RagTad(XX)
030700001102     c                   endif
030800001102     c     Wtbecod       reade     tntbe000                               31
030900001102     c                   enddo
031000001102
031100001102      *
031200001102     C     KTAB          KLIST
031300001102     C                   KFLD                    KUTE
031400001102     C                   KFLD                    KCOD
031500001102      *
031600981021     C     keynca        KLIST                                                  *fndcd00f
031700001102     C                   KFLD                    dcfaac                          -anno C.A.
031800001102     C                   KFLD                    dcffil                          -P.O. C.A.
031900001102     C                   KFLD                    dcfnca                          -numero C.A.
032000001102      *
032100990203     c     kbol01        klist
032200990203     c                   kfld                    dctaas
032300990203     c                   kfld                    dctlnp
032400990203     c                   kfld                    dctnrs
032500990203     c                   kfld                    dctnsp
032600010528      *
032700010528     c     kfnevb        klist
032800010528     c                   kfld                    dctaas
032900010528     c                   kfld                    dctlnp
033000010528     c                   kfld                    dctnrs
033100010528     c                   kfld                    dctnsp
033200010528     c                   kfld                    evbcev
033300010528      *
033400980828     C                   ENDSR
