000100970612     H DECEDIT('0,') DATEDIT(*DMY.)
000200970616     H* FNLSI1R4 *---------------------------------------------------*
000300970612     H*                                                              *
000400970612     H*       - CHIUSURA BOLLA IN PARTENZA CON PRATICA IDD      -    *
000500970616     H*       - - - - - - -   SCRITTURA EDVAC    - - - - - - - - -   *
000600970612     H*--------------------------------------------------------------*
000700090219     Ftabel00f  IF   E           K DISK
000800090219     F*---------
000900051114     FFIAR901L  IF   E           K DISK
001000970616     F*---------
001100970617     FFNBLP01L  IF   E           K DISK
001200970616     F*---------
001300080421     F*****EDVAC01L  UF A E           K DISK    USROPN
001400080421     ftivgd00f  o    e             disk
001500980204     F*---------
001600980204     FEDRDE01L  O  A E           K DISK
001700970617     F*---------
001800990810     FFNEVB00F  O  A E           K DISK
001900091030      **
002000091030      *
002100091030     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002200970612     D*--------------------------------------------------------------*
002300970612     D* DS
002400970612     D*--------------------------------------------------------------*
002500970616     D WLBDA8          DS
002600970616     D  G02DAT                 1      8  0
002700970616     D  G02INV                 9     16  0
002800970616     D  G02ERR                17     17
002900970616     D  G02TGI                18     22  0
003000970612     D KPJBA         E DS
003100970612     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
003200021114     D Tibs36ds      E DS
003300080421      *
003400080421     D DVGDFLO       e ds
003500080421     D FNVACDS       e ds
003600090219     D Ds3K          e ds
003700091030     D OG143         E DS
003800090615      *----------------------------------------------------------------
003900090615      * Controllo e semaforo x scrittura del VAC: TRUL47R
004000090615      *----------------------------------------------------------------
004100090615     D trul47ds      e ds
004200080421      *
004300091030     I*--------------------------------------------------------------*
004400091030     I*  Definizione campi Azorg
004500091030     I*--------------------------------------------------------------*
004600091030     IAZORG01L  AA
004700091030     I                                  1    3  ORGDIT
004800091030     I                             P    4    5 0ORGFIL
004900091030     I                               3776 3800  ORGDE3
005000970612     C*--------------------------------------------------------------*
005100970612     C*  C I C L O     P R I N C I P A L E
005200970612     C*--------------------------------------------------------------*
005300970613     C     *ENTRY        PLIST
005400970613     C                   PARM                    KPJBA
005500970613     C                   MOVEL     KPJBU         FNLSI1
005600080421      *
005700970616     C* Eseguo scrittura EDVAC
005800970616     C                   EXSR      WRTVAC
005900970616     C*
006000970616     C                   SETON                                        RT
006100970616     C*--------------------------------------------------------------*
006200970616     C* WRTVAC: Scrivo EDVAC
006300970616     C*--------------------------------------------------------------*
006400970616     C     WRTVAC        BEGSR
006500970616     C*
006600970616     C* Apro EDVAC
006700080421     C********           OPEN      EDVAC01L
006800090615     C*
006900090615      *? Alloca il VGD subito prima di scrivere.                  ?
007000090615     c                   exsr      Apre_VGD
007100090615     C*
007200970616     C* Aggiorno campi bolla partenze
007300051114     C                   CLEAR                   FIAR9000
007400970616     C                   Z-ADD     DI1AAS        KAAS
007500970616     C                   Z-ADD     DI1LNP        KLNP
007600970616     C                   Z-ADD     DI1NRS        KNRS
007700970616     C                   Z-ADD     DI1NSP        KNSP
007800970616     C     KBLP          CHAIN     FNBLP01L                           31
007900051114     C     KAR9          CHAIN     FIAR901L                           31
008000080421      *
008100080421      * Il VAC viene sostituito con il TIVGD ma sempre in scrittura
008200080421     C**** KVAC          CHAIN     EDVAC01L                           32
008300080421      *
008400080421     c                   clear                   fnVACds
008500970616     C                   MOVEL     BLPAAS        VACAAS
008600970616     C                   MOVEL     BLPLNP        VACLNP
008700970616     C                   MOVEL     BLPMGS        VACMGS
008800970616     C                   MOVEL     BLPNRS        VACNRS
008900970616     C                   MOVEL     BLPNSP        VACNSP
009000970616     C                   MOVEL     BLPCBO        VACCBO
009100970616     C                   MOVEL     BLPLNA        VACLNA
009200970616     C                   MOVEL     BLPRSD        VACRSD
009300970616     C                   MOVEL     BLPPRD        VACPRD
009400970616     C                   MOVEL     BLPGC1        VACGC1
009500970616     C                   MOVEL     BLPGC2        VACGC2
009600970616     C                   MOVEL     BLPCTR        VACCTR
009700970616     C                   MOVEL     BLPCTS        VACCTS
009800970616     C                   MOVEL     BLPFTM        VACFTM
009900970616     C                   MOVEL     BLPFIN        VACFIN
010000970616     C                   MOVEL     BLPFAP        VACFAP
010100970616     C                   MOVEL     BLPTSP        VACTSP
010200970616     C                   MOVEL     BLPIAS        VACIAS
010300970616     C                   MOVEL     BLPVAS        VACVAS
010400970616     C                   MOVEL     BLPNAS        VACNAS
010500970616     C                   MOVEL     BLPNCL        VACNCL
010600970616     C                   MOVEL     BLPPKF        VACPKB
010700970616     C                   MOVEL     BLPVLF        VACVLB
010800970616     C                   MOVEL     BLPQFT        VACQFT
010900051114     C                   MOVEL     AR9CAS        VACCAS
011000051114     C                   MOVEL     AR9VCA        VACVCA
011100051114     C                   MOVEL     AR9TIC        VACTIC
011200970616     C     BLPCCM        IFNE      0
011300970616     C                   MOVEL     BLPCCM        VACCCM
011400970616     C                   ELSE
011500970616     C                   MOVEL     BLPKSC        VACCCM
011600970616     C                   END
011700970616     C                   MOVEL     BLPRMN        VACRMN
011800970616     C                   MOVEL     BLPRMA        VACRMA
011900970616     C                   MOVEL     BLPRMO        VACRMO
012000970616     C                   MOVEL     BLPFFD        VACFFD
012100970616     C                   MOVEL     BLPDCR        VACDCR
012200970616     C                   MOVEL     BLPTCR        VACTCR
012300970616     C                   MOVEL     BLPHCR        VACHCR
012400970616     C                   MOVEL     BLPTC1        VACTC1
012500970616     C                   MOVEL     BLPTC2        VACTC2
012600970617     C                   MOVEL     OGGI          VACDCM
012700970616     C     BLPHMC        IFEQ      0
012800970616     C                   TIME                    WTIME             6 0
012900970616     C                   MOVEL     WTIME         VACHMC
013000970626     C                   ELSE
013100970626     C                   Z-ADD     BLPHMC        VACHMC
013200970616     C                   END
013300970617     C                   MOVEL     BLPCCA        VACCCA
013400980204     C     DI1CCA        IFEQ      'S'
013500980203     C     DI1NCL        IFEQ      BLPNCL
013600970617     C                   MOVEL     '7'           VACCCA
013700980203     C                   ELSE
013800970617     C                   MOVEL     'P'           VACCCA
013900970617     C                   END
014000980203     C                   END
014100970616     C                   MOVEL     0             VACDLA
014200970616     C                   Z-ADD     0             VACDAG
014300970616     C*
014400080421     C**********
014500080421     C* sostitutita la scrittura del VAC co il TIVGD
014600080421     C**N32*****         UPDATE    FNVAC000
014700080421     C***32*****         WRITE     FNVAC000
014800080421      **********
014900080421      * --> scrittura del TIVGD
015000080421     C                   clear                   tivgd000
015100080421     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
015200080421     C                   eval      vgdTIP = 'VC'
015300080421     C                   movel     *all'0'       vgdKSU
015400080421     c                   move      DI1KSC        vgdksu
015500090219      *
015600090219      *  imposta come Default
015700090219     C                   eval      vgdTSC = 'EW'
015800091030      *  se DPD invece il Default deve essere "WW"
015900091030     C                   Z-ADD     blpLNP        ORGFIL
016000091030     C     ORGFIL        CHAIN     AZORG01L
016100091030      **? e se  si tratta di DPD (eccezione .......)
016200091030     c                   if        %Found(AZORG01L)
016300091030     c                   movel     orgde3        og143
016400091030     c                   if        §OGNTW ='DPD'
016500091030     C                   eval      vgdTSC = 'WW'
016600091030     c                   end
016700091030     c                   end
016800090219      * però il tipo invio deve essere preso dalla "3K"
016900090219      *
017000090219     c                   z-add     1             TBLKUT
017100090219     c                   movel     '3K'          TBLCOD
017200090219     c                   movel(p)  di1ksc        TBLKEY
017300090219      *
017400090219     c     kTabel        chain     Tabel00f
017500090219     c                   if        %Found(tabel00f)
017600090219     c                   eval      ds3K = tblUNI
017700090219     C                   eval      vgdTSC = §3KCSD
017800090311     C                   movel     *all'0'       vgdKSU
017900090311     c                   move      §3KCKS        vgdksu
018000090219     c                   end
018100090219      *
018200080421     c                   eval      vgdpgm = 'FNLSI1R4'
018300080421     c                   z-add     oggi          vgddat
018400080421      *  x generare l'EDI:
018500080421      *    occorre farlo in modo controllato (personalizzato)
018600080421     C                   clear                   DVGDFLO
018700080421     C                   movel     'P'           §VGDFELA
018800080421     C                   movel     DVGDFLO       vgdflo
018900080421      *
019000080421     C                   write     tivgd000
019100090615     C*
019200090615      *? Rilascia il VGD subito dopo.                  ?
019300090615     c                   exsr      Rilascia_VGD
019400080421      **********
019500970616     C* Chiudo file
019600080421     C**********         CLOSE     EDVAC01L
019700080421     C**********
019800980204     C* Scrivo Note aggiuntive in EDRDE
019900980204     C     DI1FTX        IFNE      *BLANKS
020000980204     C                   Z-ADD     BLPLNP        RDELNP
020100980204     C                   Z-ADD     BLPAAS        RDEAAS
020200980204     C                   Z-ADD     BLPNRS        RDENRS
020300980204     C                   Z-ADD     BLPNSP        RDENSP
020400980204     C                   MOVEL     'DD4440'      RDENMC
020500980204     C                   Z-ADD     1             RDENRP
020600980204     C                   MOVEL     DI1FTX        RDEVAL
020700980204     C                   WRITE     EDRDE000
020800980204     C* Controllo se + più lungo di 35 chr
020900980204     C                   MOVE      DI1FTX        WCOM15           15
021000980204     C     WCOM15        IFNE      *BLANKS
021100980204     C                   Z-ADD     BLPLNP        RDELNP
021200980204     C                   Z-ADD     BLPAAS        RDEAAS
021300980204     C                   Z-ADD     BLPNRS        RDENRS
021400980204     C                   Z-ADD     BLPNSP        RDENSP
021500980204     C                   MOVEL     'DD4440'      RDENMC
021600980204     C                   Z-ADD     2             RDENRP
021700980204     C                   MOVEL     *BLANKS       RDEVAL
021800980204     C                   MOVEL     WCOM15        RDEVAL
021900980204     C                   WRITE     EDRDE000
022000980204     C                   END
022100980204     C*
022200980204     C                   END
022300980204     C*
022400980204     C* Scrivo evento mancanza
022500980204     C     DI1C2A        IFNE      *BLANKS
022600970617     C                   MOVEL     DI1C2A        EVBCEV
022700970617     C                   MOVEL     *BLANKS       EVBATB
022800970617     C                   Z-ADD     OGGI          EVBDTV
022900970617     C                   TIME                    EVBORV
023000020913      *
023100020913     C*****              Z-ADD     BLPLNP        EVBFLE
023200020913     C*****              Z-ADD     DSFCDU        EVBCDU
023300020913      * P.O.Utente e Codice Utente e non più la LNP
023400021114     C                   Z-ADD     O36pou        EVBFLE
023500021114     C                   Z-ADD     O36cou        EVBCDU
023600020913      *
023700970617     C                   Z-ADD     BLPLNP        EVBLNP
023800970617     C                   Z-ADD     BLPAAS        EVBAAS
023900970617     C                   Z-ADD     BLPNRS        EVBNRS
024000970617     C                   Z-ADD     BLPNSP        EVBNSP
024100970617     C                   Z-ADD     OGGI          EVBDEV
024200970617     C                   MOVEL     EVBORV        EVBHEV
024300971007     C                   MOVEL     'T'           EVBFTR
024400971007     C                   Z-ADD     OGGI          EVBDTR
024500970617     C*
024600990810     C                   WRITE     FNEVB000
024700980204     C                   END
024800980204     C*
024900970616     C                   ENDSR
025000970613     C*--------------------------------------------------------------*
025100970613     C* *INZSR: Operazioni iniziali
025200970613     C*--------------------------------------------------------------*
025300970613     C     *INZSR        BEGSR
025400970613     C*
025500970617     C* Decodifica Elaboratore
025600021114     C                   CLEAR                   Tibs36Ds
025700021114     c                   EVAL      I36ute = KNMUS
025800021114     c                   EVAL      I36Tla = 'L'
025900021114     C                   CALL      'TIBS36R'
026000021114     C                   PARM                    Tibs36Ds
026100020913      *
026200970613      *  Definizione chiavi
026300051114     C     KAR9          KLIST
026400970613     C                   KFLD                    KAAS
026500970613     C                   KFLD                    KLNP
026600970613     C                   KFLD                    KNRS
026700970613     C                   KFLD                    KNSP
026800970616     C     KBLP          KLIST
026900970616     C                   KFLD                    KAAS
027000970616     C                   KFLD                    KLNP
027100970616     C                   KFLD                    KNRS
027200970616     C                   KFLD                    KNSP
027300970616     C     KVAC          KLIST
027400970616     C                   KFLD                    KAAS
027500970616     C                   KFLD                    KLNP
027600970616     C                   KFLD                    KNRS
027700970616     C                   KFLD                    KNSP
027800090219     c     kTabel        klist
027900090219     C                   KFLD                    TBLkut
028000090219     C                   KFLD                    TBLcod
028100090219     C                   KFLD                    TBLkey
028200970613      *
028300970613      *  Definizione variabili
028400970616     C     *LIKE         DEFINE    BLPAAS        KAAS
028500970616     C     *LIKE         DEFINE    BLPLNP        KLNP
028600970616     C     *LIKE         DEFINE    BLPNRS        KNRS
028700970616     C     *LIKE         DEFINE    BLPNSP        KNSP
028800970616      *
028900970616      *  Reperisco la data del giorno
029000970616     C                   TIME                    WHHDAT           14 0
029100970616     C                   MOVE      WHHDAT        G02DAT
029200970616     C                   MOVEL     *BLANK        G02ERR
029300970616     C                   CALL      'XSRDA8'
029400970616     C                   PARM                    WLBDA8
029500970616     C                   Z-ADD     G02INV        OGGI              8 0
029600970616     C*
029700970613     C                   ENDSR
029800090615     C*--------------------------------------------------------------*
029900090615      *?Prima di iniziare, controlla se è possibile Procedere mediante  ?
030000090615      *? un controllo semaforico fatto con il TRUL47R.                  ?
030100090615      *  Se la strada è libera si può procedere altrimenti msg di stop.
030200090615     C*---------------------------------------------------------------*
030300090615      *? Come prima cosa avvio il blocco elaborazione TIVGD x tipo file ?
030400090615     C*---------------------------------------------------------------*
030500090615     C     Apre_VGD      BEGSR
030600090615      *
030700090615     C                   clear                   trul47ds
030800090615     C                   eval      d47opz  = 'I'
030900090615     C                   eval      d47tip  = 'VC'
031000090615     C                   eval      d47lck  = 'N'
031100090615     C                   eval      d47chkj = 'N'
031200090615     C                   eval      d47pgm  = 'FNLSI1R4'
031300090615     C                   call      'TRUL47R'
031400090615     C                   parm                    trul47ds
031500090615      *
031600090615      *? se strada bloccata invio msg a video.                  ?
031700090615     C                   if        d47sts = 'A'
031800090615     c                   end
031900090615     C*
032000090615     C     FINCT1        ENDSR
032100090615     C*--------------------------------------------------------------*
032200090615     C* Rilascia il TIVGD
032300090615     C*--------------------------------------------------------------*
032400090615     C     Rilascia_VGD  BEGSR
032500090615     C*
032600090615      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
032700090615     C                   clear                   trul47ds
032800090615     C                   eval      d47opz  = 'F'
032900090615     C                   eval      d47tip  = 'VC'
033000090615     C                   call      'TRUL47R'
033100090615     C                   parm                    trul47ds
033200090615     C*
033300090615     C                   ENDSR
033400090615     C*--------------------------------------------------------------*
033500090615      * Routine di controllo x eventuali Errori
033600090615      *------------------------------------------------------------------------*
033700090615     C     *PSSR         BEGSR
033800090615      *
033900090615      * Comunque eseguo x rilasciare il TIVGD.
034000090615      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
034100090615     C                   clear                   trul47ds
034200090615     C                   eval      d47opz  = 'F'
034300090615     C                   eval      d47tip  = 'VC'
034400090615     C                   call      'TRUL47R'
034500090615     C                   parm                    trul47ds
034600090615      *
034700090615     C                   ENDSR     '*CANCL'
034800090615     C*------------------------------------------------------------------------*
