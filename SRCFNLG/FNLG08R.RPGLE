000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200051013     H* fNLG08R *----------------------------------------------------*
000300900418     H*                                                              *
000400050606     H* Scrittura del tivgd00F                                       *
000500900418     H*--------------------------------------------------------------*
000600050512     Ftigcp02L  iF   E           K DISK
000700050512     Ftignp01L  iF   E           K DISK
000800050512     Ffnblp01L  iF   E           K DISK
000900050512     Ftabel00f  iF   E           K DISK
001000050606     Ftivgd00f  if a E             DISK    commit
001100900418     D KPJBA         E DS
001200050512     D FNLG08ds      E DS
001300050606     D FNvagds       E DS
001400050512     D ds2a          E DS
001500050606     D WLBDA8          DS
001600050606     D  G02DAT                 1      8  0
001700050606     D  G02INV                 9     16  0
001800050606     D  G02ERR                17     17
001900050606     D  G02TGI                18     22  0
002000050512     c* aggancio la giacenza
002100050512     c                   do
002200050512     c     kgia          chain     tigcp02l
002300050512     c                   if        not %found(tigcp02l)
002400050512     c                   leave
002500050512     c                   end
002600050512     C*  aggancio la bolla in partenza per reperire dati mancanti
002700050512     C     Kspe          CHAIN     FNBLP01L                           22
002800050512     c                   if        not %found(fnblp01l)
002900050512     c                   leave
003000050512     c                   end
003100050606     c*
003200050606     c                   clear                   fnvagds
003300050512     C* PRELEVO EVENTUALE ULTERIORE DESCRIZIONE
003400050512     C                   Z-ADD     10            fase
003500050512     C                   Z-ADD     1             NPR
003600050512     C     KNOT          CHAIN     tignp01L
003700050512     c                   if        %found(tignp01l)
003800050512     C                   MOVEL     GNPDMC        ULTDMC
003900050512     c                   else
004000050512     C                   MOVEL     *BLANKS       ULTDMC           50
004100050512     c                   end
004200960124     C** RICERCO COD.MOTIVO GIACENZA SULLA TABELLA
004300050512     c                   if        d08est = 'E'
004400050512     C                   MOVEL     '2X'          tblcod
004500050512     c                   else
004600050512     C                   MOVEL     '2A'          tblcod
004700050512     c                   end
004800050512     C                   eval      tblkut = 1
004900050512     C                   MOVEL(p)  GCPCMC        tblkey
005000050512     c                   clear                   ds2a
005100050512     c                   clear                   tbluni
005200050512     C     KTAB          CHAIN     TABEL00f
005300050512     c                   if        %found(tabel00f)
005400050512     c                   if        d08est = 'E'
005500050512     C                   MOVEL     TBLUNI        WMOTEV           75
005600050512     c                   else
005700050512     C                   MOVEL     TBLUNI        DS2A
005800050512     C     §2ADES        IFEQ      *BLANKS
005900050512     C                   MOVEL     ULTDMC        §2ADES
006000050512     C                   MOVEL     *BLANKS       ULTDMC
006100050512     C                   END
006200050512     c                   end
006300050512     c                   end
006400921013     C* SCRIVO IL RECORD
006500940927     C                   Z-ADD     GCPLNP        VAGLNP
006600940927     C                   Z-ADD     GCPAAS        VAGAAS
006700940927     C                   Z-ADD     GCPNRS        VAGNRS
006800940927     C                   Z-ADD     GCPNSP        VAGNSP
006900940927     C                   Z-ADD     GCPLNA        VAGLNA
007000940927     C                   Z-ADD     GCPAGC        VAGAGC
007100940927     C                   Z-ADD     GCPMGC        VAGMGC
007200940927     C                   Z-ADD     GCPFGC        VAGFGC
007300940927     C                   Z-ADD     GCPNGC        VAGNGC
007400940927     C                   MOVEL     GCPCMC        VAGCMC
007500060116      * ulteriore informazione APPUNTAMENTO
007600060116     c                   if        d08app = 'S' and d08tpr = 'A'
007700060116     C                   Z-ADD     GCPded        VAGded
007800060116     C                   Z-ADD     GCPddm        VAGddm
007900060116     c                   endif
008000051013      *
008100051013     c                   if        gcpded > woggi
008200051013     C                   Z-ADD     GCPDED        VAGDED
008300051013     c                   else
008400051013     C                   move      *zero         VAGDED
008500051013     c                   endif
008600050609      * blocco di campi da scrivere solo se rekord con dati di chiusura
008700050609     c                   if        d08tpr = 'C'
008800050609     C                   move      GCPVCS        VAGVCS
008900050609     C                   Z-ADD     GCPCAS        VAGCAS
009000050609     C                   MOVE      GCPVCA        VAGVCA
009100050609     C                   Z-ADD     GCPDDM        VAGDDM
009200050609     C                   Z-ADD     GCPDED        VAGDED
009300050609     C                   Z-ADD     GCPDCG        VAGDCG
009400050609     C                   MOVE      GCPCFG        VAGCFG
009500050609     C                   Z-ADD     GCPDSD        VAGDSD
009600050609     C                   MOVEL     GCPDSA        VAGDSA
009700050609     C                   Z-ADD     GCPGGS        VAGGGS
009800050609     C                   Z-ADD     GCPSGF        VAGSGF
009900050609     C                   Z-ADD     GCPSGA        VAGSGA
010000050609     C                   Z-ADD     GCPTFT        VAGTFT
010100050609     C                   Z-ADD     GCPDBG        VAGDBG
010200050609     C                   Z-ADD     GCPLPW        VAGLPW
010300050609     C                   Z-ADD     GCPASW        VAGASW
010400050609     C                   Z-ADD     GCPNSW        VAGNSW
010500050609     C                   Z-ADD     GCPNBW        VAGNBW
010600050609     C                   Z-ADD     GCPKSC        VAGKSC
010700050609     c                   endif
010800921013     C*
010900050512     C     d08est        IFEQ      'E'
011000961203     C                   MOVEL     WMOTEV        VAGDMC
011100961203     C                   ELSE
011200961203     C                   MOVEL     §2ADES        VAGDMC
011300961203     C                   END
011400940927     C                   MOVEL     ULTDMC        VAGNOT
011500940927     C                   Z-ADD     BLPRMN        VAGRMN
011600940927     C                   MOVEL     BLPRMA        VAGRMA
011700040907     C                   Z-ADD     GCPSCM        VAGSCM
011800940927     C                   Z-ADD     GCPDMM        VAGDMM
011900940927     C                   MOVEL     BLPGGA        VAGGGA
012000050214      * imposto vagdur solo se riapertura vera testando il flag
012100050214     c                   if        gcpriap <> *blank
012200940927     C                   Z-ADD     GCPDUR        VAGDUR
012300050214     c                   endif
012400050513     C* PRELEVO NUMERO PROGRESSIVO APERTURA GIACENZA
012500050512     C                   Z-ADD     9             prg
012600050512     C     Kgia          SETGT     tigcp02L
012700050512     C                   READP     tigcp02L
012800050630      * per le chiusure passo sempre il progressivo = 0
012900050630     c                   if        d08tpr <> 'C'
013000940927     C                   Z-ADD     GCPFRG        VAGFRG
013100050630     c                   else
013200050630     C                   Z-ADD     0             VAGFRG
013300050630     c                   endif
013400050606     c* scivo file tivgd00f
013500050606     c                   clear                   tivgd000
013600080403     c*****              eval      vgddta = fnvagds
013700080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
013800050606     c                   eval      vgdtip = 'VG'
013900050606     c                   eval      vgdksu = *all'0'
014000050606     c                   move      d08uni        vgdksu
014100050606     c                   eval      vgdtsc = d08gsd
014200050608     c                   eval      vgdpgm = d08pgm
014300050606     c                   eval      vgddat = woggi
014400050606     C                   WRITE     tivgd000
014500050516     c                   if        d08commit = 'S'
014600050516     c                   commit
014700050516     c                   end
014800050512     c                   eval      d08ok = 'S'
014900050512     c                   enddo
015000050512     c                   movel     fnlg08ds      kpjbu
015100050512     c                   seton                                        lr
015200050512     c**********************************************************************
015300050512     c     *inzsr        begsr
015400050512     c**********************************************************************
015500050512     c     *entry        plist
015600050512     C                   PARM                    KPJBA
015700050512     c                   movel     kpjbu         fnlg08ds
015800050512     c                   eval      d08ok='N'
015900050606     C* Creo data del giorno
016000050606     C                   TIME                    WHHDAT           14 0
016100050606     C                   MOVE      WHHDAT        G02DAT
016200050606     C                   MOVE      *ZEROS        G02INV
016300050606     C                   MOVEL     *BLANK        G02ERR
016400050606     C                   CALL      'XSRDA8'
016500050606     C                   PARM                    WLBDA8
016600050606     C                   Z-ADD     G02INV        WOGGI             8 0
016700050512     C** TABEL
016800050512     C     KTAB          KLIST
016900050512     C                   KFLD                    tblkut
017000050512     C                   KFLD                    tblcod
017100050512     C                   KFLD                    tblkey
017200050512     C* FLBLP
017300050512     C     Kspe          KLIST
017400050512     C                   KFLD                    GCPAAS
017500050512     C                   KFLD                    GCPLNP
017600050512     C                   KFLD                    GCPNRS
017700050512     C                   KFLD                    GCPNSP
017800050512     C*
017900050512     C     KGIA          KLIST
018000050512     C                   KFLD                    D08AGC
018100050512     C                   KFLD                    D08FGC
018200050512     C                   KFLD                    D08NGC
018300050512     C                   KFLD                    PRG               2 0
018400050512     C                   Z-ADD     0             PRG
018500050512     c*
018600050512     C     KNOT          KLIST
018700050512     C                   KFLD                    GCPAGC
018800050512     C                   KFLD                    GCPFGC
018900050512     C                   KFLD                    GCPNGC
019000050512     C                   KFLD                    GCPFRG
019100050512     C                   KFLD                    FASE              3 0
019200050512     C                   KFLD                    NPR               3 0
019300050512     c*
019400050512     c                   endsr
