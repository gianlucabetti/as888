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
019000091116      *  Se DPD non deve scrivere il TIVGD poichè ci sono i FIEU
019100091116      *   con gli SCAN
019200091116     c                   if        §OGNTW <>'DPD' and
019300091116     c                             §3kcsd <> *blank
019400080421     C                   write     tivgd000
019500091116     c                   end
019600090615     C*
019700090615      *? Rilascia il VGD subito dopo.                  ?
019800090615     c                   exsr      Rilascia_VGD
019900080421      **********
020000970616     C* Chiudo file
020100080421     C**********         CLOSE     EDVAC01L
020200080421     C**********
020300980204     C* Scrivo Note aggiuntive in EDRDE
020400980204     C     DI1FTX        IFNE      *BLANKS
020500980204     C                   Z-ADD     BLPLNP        RDELNP
020600980204     C                   Z-ADD     BLPAAS        RDEAAS
020700980204     C                   Z-ADD     BLPNRS        RDENRS
020800980204     C                   Z-ADD     BLPNSP        RDENSP
020900980204     C                   MOVEL     'DD4440'      RDENMC
021000980204     C                   Z-ADD     1             RDENRP
021100980204     C                   MOVEL     DI1FTX        RDEVAL
021200980204     C                   WRITE     EDRDE000
021300980204     C* Controllo se + più lungo di 35 chr
021400980204     C                   MOVE      DI1FTX        WCOM15           15
021500980204     C     WCOM15        IFNE      *BLANKS
021600980204     C                   Z-ADD     BLPLNP        RDELNP
021700980204     C                   Z-ADD     BLPAAS        RDEAAS
021800980204     C                   Z-ADD     BLPNRS        RDENRS
021900980204     C                   Z-ADD     BLPNSP        RDENSP
022000980204     C                   MOVEL     'DD4440'      RDENMC
022100980204     C                   Z-ADD     2             RDENRP
022200980204     C                   MOVEL     *BLANKS       RDEVAL
022300980204     C                   MOVEL     WCOM15        RDEVAL
022400980204     C                   WRITE     EDRDE000
022500980204     C                   END
022600980204     C*
022700980204     C                   END
022800980204     C*
022900980204     C* Scrivo evento mancanza
023000980204     C     DI1C2A        IFNE      *BLANKS
023100970617     C                   MOVEL     DI1C2A        EVBCEV
023200970617     C                   MOVEL     *BLANKS       EVBATB
023300970617     C                   Z-ADD     OGGI          EVBDTV
023400970617     C                   TIME                    EVBORV
023500020913      *
023600020913     C*****              Z-ADD     BLPLNP        EVBFLE
023700020913     C*****              Z-ADD     DSFCDU        EVBCDU
023800020913      * P.O.Utente e Codice Utente e non più la LNP
023900021114     C                   Z-ADD     O36pou        EVBFLE
024000021114     C                   Z-ADD     O36cou        EVBCDU
024100020913      *
024200970617     C                   Z-ADD     BLPLNP        EVBLNP
024300970617     C                   Z-ADD     BLPAAS        EVBAAS
024400970617     C                   Z-ADD     BLPNRS        EVBNRS
024500970617     C                   Z-ADD     BLPNSP        EVBNSP
024600970617     C                   Z-ADD     OGGI          EVBDEV
024700970617     C                   MOVEL     EVBORV        EVBHEV
024800971007     C                   MOVEL     'T'           EVBFTR
024900971007     C                   Z-ADD     OGGI          EVBDTR
025000970617     C*
025100990810     C                   WRITE     FNEVB000
025200980204     C                   END
025300980204     C*
025400970616     C                   ENDSR
025500970613     C*--------------------------------------------------------------*
025600970613     C* *INZSR: Operazioni iniziali
025700970613     C*--------------------------------------------------------------*
025800970613     C     *INZSR        BEGSR
025900970613     C*
026000970617     C* Decodifica Elaboratore
026100021114     C                   CLEAR                   Tibs36Ds
026200021114     c                   EVAL      I36ute = KNMUS
026300021114     c                   EVAL      I36Tla = 'L'
026400021114     C                   CALL      'TIBS36R'
026500021114     C                   PARM                    Tibs36Ds
026600020913      *
026700970613      *  Definizione chiavi
026800051114     C     KAR9          KLIST
026900970613     C                   KFLD                    KAAS
027000970613     C                   KFLD                    KLNP
027100970613     C                   KFLD                    KNRS
027200970613     C                   KFLD                    KNSP
027300970616     C     KBLP          KLIST
027400970616     C                   KFLD                    KAAS
027500970616     C                   KFLD                    KLNP
027600970616     C                   KFLD                    KNRS
027700970616     C                   KFLD                    KNSP
027800970616     C     KVAC          KLIST
027900970616     C                   KFLD                    KAAS
028000970616     C                   KFLD                    KLNP
028100970616     C                   KFLD                    KNRS
028200970616     C                   KFLD                    KNSP
028300090219     c     kTabel        klist
028400090219     C                   KFLD                    TBLkut
028500090219     C                   KFLD                    TBLcod
028600090219     C                   KFLD                    TBLkey
028700970613      *
028800970613      *  Definizione variabili
028900970616     C     *LIKE         DEFINE    BLPAAS        KAAS
029000970616     C     *LIKE         DEFINE    BLPLNP        KLNP
029100970616     C     *LIKE         DEFINE    BLPNRS        KNRS
029200970616     C     *LIKE         DEFINE    BLPNSP        KNSP
029300970616      *
029400970616      *  Reperisco la data del giorno
029500970616     C                   TIME                    WHHDAT           14 0
029600970616     C                   MOVE      WHHDAT        G02DAT
029700970616     C                   MOVEL     *BLANK        G02ERR
029800970616     C                   CALL      'XSRDA8'
029900970616     C                   PARM                    WLBDA8
030000970616     C                   Z-ADD     G02INV        OGGI              8 0
030100970616     C*
030200970613     C                   ENDSR
030300090615     C*--------------------------------------------------------------*
030400090615      *?Prima di iniziare, controlla se è possibile Procedere mediante  ?
030500090615      *? un controllo semaforico fatto con il TRUL47R.                  ?
030600090615      *  Se la strada è libera si può procedere altrimenti msg di stop.
030700090615     C*---------------------------------------------------------------*
030800090615      *? Come prima cosa avvio il blocco elaborazione TIVGD x tipo file ?
030900090615     C*---------------------------------------------------------------*
031000090615     C     Apre_VGD      BEGSR
031100090615      *
031200090615     C                   clear                   trul47ds
031300090615     C                   eval      d47opz  = 'I'
031400090615     C                   eval      d47tip  = 'VC'
031500090615     C                   eval      d47lck  = 'N'
031600090615     C                   eval      d47chkj = 'N'
031700090615     C                   eval      d47pgm  = 'FNLSI1R4'
031800090615     C                   call      'TRUL47R'
031900090615     C                   parm                    trul47ds
032000090615      *
032100090615      *? se strada bloccata invio msg a video.                  ?
032200090615     C                   if        d47sts = 'A'
032300090615     c                   end
032400090615     C*
032500090615     C     FINCT1        ENDSR
032600090615     C*--------------------------------------------------------------*
032700090615     C* Rilascia il TIVGD
032800090615     C*--------------------------------------------------------------*
032900090615     C     Rilascia_VGD  BEGSR
033000090615     C*
033100090615      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
033200090615     C                   clear                   trul47ds
033300090615     C                   eval      d47opz  = 'F'
033400090615     C                   eval      d47tip  = 'VC'
033500090615     C                   call      'TRUL47R'
033600090615     C                   parm                    trul47ds
033700090615     C*
033800090615     C                   ENDSR
033900090615     C*--------------------------------------------------------------*
034000090615      * Routine di controllo x eventuali Errori
034100090615      *------------------------------------------------------------------------*
034200090615     C     *PSSR         BEGSR
034300090615      *
034400090615      * Comunque eseguo x rilasciare il TIVGD.
034500090615      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
034600090615     C                   clear                   trul47ds
034700090615     C                   eval      d47opz  = 'F'
034800090615     C                   eval      d47tip  = 'VC'
034900090615     C                   call      'TRUL47R'
035000090615     C                   parm                    trul47ds
035100090615      *
035200090615     C                   ENDSR     '*CANCL'
035300090615     C*------------------------------------------------------------------------*
