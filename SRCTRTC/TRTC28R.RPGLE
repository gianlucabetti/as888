000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300000124     H*   CREA  ARCHIVIO BOLLE FATTURATE  AL CLIENTE       *
000400030516     H*                                                    *
000500030516     H* per compilare fare OVRDBF del file TABELXXF        *
000600030516     H* relese *CURRENT  (TGTRLS)                          *
000700050127     H*----------------------------------------------------*
000800111018     H* ------> 18/10/2011 eliminata tabella 4R <--------  *
000900050127     H* ATTENZIONE!!!! la gestione del nuovo mbr del file  *
001000050127     H* TIVAF00T x accorpamento bolle non è prevista nel   *
001100050127     H* caso di cliente con multimembro tab.4R             *
001200890921     H*----------------------------------------------------*
001300890921     F*
001400990817     FTITAS31C  IF   E           K DISK
001500990823     FTITA730C  IF   E           K DISK
001600050905     FFNORM01L  IF   E           K DISK
001700051219     fAZORG01L  if   e           k disk
001800910830     FTABEL00F  IF   E           K DISK
001900170712     FTABELxxF  IF   E           K DISK    usropn
002000170712     F                                     extdesc('EDPFG/TABELXXF')
002100170712     F                                     rename(tabel:tabelx) prefix(x)
002200040531     FFIAR531C  IF   E           K DISK
002300060125     FTIVAF00T  UF A E             DISK    usropn
002400050906     FTIVGD00F  UF A E             DISK
002500170227     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002600170227     F                                     PREFIX(f_)
002700140108
002800101025     D CMD1            S             50    DIM(1) CTDATA PERRCD(1)
002900101025     D CMD2            S             50    DIM(1) CTDATA PERRCD(1)
003000101025     D CMD3            S             50    DIM(1) CTDATA PERRCD(1)
003100101025     D CMD5            S             50    DIM(1) CTDATA PERRCD(1)
003200101025     D CMD             S             50    DIM(1) CTDATA PERRCD(1)
003300030515     D VALNUM          S             10    DIM(1) CTDATA PERRCD(10)
003400050906     D VSV             S              1    DIM(30)                              SIGLA VARIE
003500990910     D VVA             S             11  3 DIM(30)                              IMPOR VARIE
003600140109     D
003700050905     D curKSU          s                   like(vgdKSU)
003800050905     D curTSC          s                   like(vgdTSC)
003900170227     D skKSCU          s                   dim(80000) like(DS_PRG)
004000170227     D skPRG           s                   dim(80000) like(vgdPRG)
004100140109     D jPRG            s              5  0 inz
004200140109     D tPRG            s                   inz like(jPRG)
004300140109     D DS_PRG          ds
004400140109     D  ds_KSU                             like(vgdKSU)
004500140109     D  ds_DFT                             like(tasDFT)
004600140109     D  ds_NFT                             like(tasNFT)
004700140109     D  ds_FIV                             like(tasFIV)
004800170227     D skVGDKSU        s                   like(DS_VGDKSU) dim(80000) inz
004900170227     D jKSU            s              5  0 inz
005000170227     D DS_VGDKSU       ds                  qualified inz
005100170227     D  ds_KSU                             like(vgdKSU)
005200170227     D  ds_PRG                             like(vgdPRG)
005300170406     D  ds_TSC                             like(vgdTSC)
005400170227
005500890921     D KPJBA         E DS
005600990817     D DS4C          E DS
005700060203     D DS4C_W        E DS                  EXTNAME(DS4C) PREFIX(W_)
005800050906     D DSQT1         E DS
005900111018     D**!!!DS4r          E DS
006000050906     D DS$2          E DS
006100030516     D DTA4A         E DS
006200160209     D DTA4M         E DS
006300050905     D tivafds       e ds                  extname(tivaf00t)
006400170227     D TIS7VASDS     E DS
006500170227
006600050906     D PARAM           DS
006700941122     D  DAL                    1      8  0
006800941122     D  AL                     9     16  0
006900050906     D  CLIU                  17     23  0
007000101025     D  REVISORI              24     24
007100140108     D  NFT                   25     30  0
007200140113     D  FILESEP               31     31
007300140319     D  FLGUNI                32     32
007400140319     D  CLIVAS                33     40
007500010402     D*------------------
007600030917      * DS FIAR5
007700030917     d dAr5Bnb       e ds
007800151016     D DAR5FAT       E DS
007900050126
008000050126     d Tibs02ds      e ds
008100111026     d*******dvus          e ds
008200051219
008300051219     d Og143         e ds                  inz
008400060127
008500030917      *
008600021212     D Wtara           s                   like(§QTTPC)
008700021212     D Wntara          s                   like(TASPKF)
008800030917
008900170308     D savPRG          s                   like(vgdPRG)
009000170308
009100030917     d kAr5Trd         s                   Like(Ar5Trd)
009200030515
009300030515     D Rma15           s             15
009400030515     D WRma15          s             15
009500030515     D II              s              2  0
009600030515     D Pos             s              2  0
009700111026     D Var             s              9
009800050126     d wxco            s              1    inz(*Off)
009900101025     d $Fine           s              1    inz('0')
010000160208
010100160208     D*--------------------
010200160208     D* DEFINIZIONI A PROCEDURE ESTERNE
010300160208     D*--------------------
010400160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
010500160208
010600010402
010700101025     C*------------------------------------------------------------------
010800101025     C     *ENTRY        PLIST
010900101025     C                   PARM                    KPJBA
011000140319     C                   MOVEL     KPJBU         PARAM
011100140319     C*
011200140319     C* Verifico subito se richiesto un particolare cliente VAS (per output)
011300140319     C                   setoff                                       45
011400140319     C                   if        CLIVAS = *all'0' and CLIU <> *zeros
011500140319     C                   move      CLIU          CLIVAS
011600140319     C                   endif
011700140319     C                   if        CLIVAS <> *all'0'
011800140319     C                   seton                                        45
011900140319     C                   endif
012000170308     C*
012100170308     C* Verifico subito se richiesti file separati per ogni fattura
012200170308     C                   if        FILESEP = 'N'
012300170308     C                   exsr      calprog
012400170308     C                   eval      savPRG = f_tis7prgf
012500170308     C                   else
012600170308     C                   eval      savPRG = *blanks
012700170308     C                   endif
012800140319     c*
012900101025     c* Verifico se elaborazione per REVISORI o per scarico a clienti
013000101025     c                   if        revisori='S'
013100101025     c                   exsr      ElaREV
013200101025     c                   else
013300101025     c                   exsr      ElaCLI
013400101025     c                   endif
013500160208     C*
013600160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
013700160208     C                   CALL      'UBTA400R'
013800160208     C                   PARM                    UBTA4IOPZ
013900160208     C                   PARM      'C'           UBTA4ITLA
014000160208     C                   PARM                    UBTA4IAAS
014100160208     C                   PARM                    UBTA4ILNP
014200160208     C                   PARM                    UBTA4INRS
014300160208     C                   PARM                    UBTA4INSP
014400160208     C                   PARM                    UBTA4ITRC
014500160208     C                   PARM                    UBTA4OERR
014600160208     C                   PARM                    UBTA4ODS
014700160208     C                   PARM                    UBTA4OLEN
014800160208     C                   PARM                    UBTA4ODATI
014900160208     C*
015000921023     C                   SETON                                        LR
015100910830     C*----------------------------------------------------*
015200101025     c* Scrittuta TIVGD per i clienti che lo hanno richiesto
015300101025     C*----------------------------------------------------*
015400101025     c     ElaCLI        BEGSR
015500101025     c
015600101025     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VF'
015700170227     C                   CLEAR                   TIS7VASDS
015800170227     C                   EVAL      i§7VASOPZ = 'SEM'
015900170227     C                   EVAL      i§7VASKSU = *blanks
016000170227     C                   EVAL      i§7VASTIP = 'VF'
016100170227     C                   CALL(e)   'TIS7VASR1'
016200170227     C                   PARM                    TIS7VASDS
016300170227     C*
016400170227     C* Verifico esito chiamata
016500170227     C                   if        not %error AND
016600170227     C                             o§7VASOK = *on
016700101025     C*
016800101025     C                   EXSR      DEFCAM
016900101025      *
017000101025     C     KTB           SETLL     TABEL
017100140326     C                   SETOFF                                       58
017200140326    1C                   IF        %equal(TABEL00F)
017300140326     C     KTB           READE     TABEL
017400140326    2C                   DOW       not %eof(TABEL00F)
017500101025    3C     TBLFLG        IFNE      '*'
017600101025     C                   MOVEL     TBLUNI        DS4C
017700101025    4C     §4CFSD        IFNE      *BLANK                                        CLI DA SCAR
017800101025     C                   MOVE      §4CFKS        KSCU              7 0
017900101025     C                   MOVEL     TBLKEY        KSC               7 0
018000101025     C*
018100140326    5C                   if        (CLIU>*zeros AND FLGUNI='S' AND CLIU=KSCU) OR
018200140326     C                             (CLIU>*zeros AND FLGUNI='N')               OR
018300140319     C                              CLIU=*zeros
018400140326     C                   if         CLIU>*zeros AND FLGUNI='N'
018500140326     C                   eval      KSC = CLIU
018600140326     C                   seton                                        58
018700140326     C                   endif
018800140326     C*
018900101025     c                   Eval      wxco = *Off
019000101025     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
019100140326   e5C                   endif
019200140326     C*
019300140326     C                   if        *in58
019400140326     C                   leave
019500140326     C                   endif
019600140326     C*
019700140326   e4C                   ENDIF
019800140326   e3C                   ENDIF
019900140326     C     KTB           READE     TABEL
020000140326   e2C                   ENDDO
020100140326   e1C                   ENDIF
020200101025     C*
020300170227     C* CANCELLO LE OVR ESEGUITE
020400101025     C                   MOVEL     *BLANKS       COMMAN           80
020500101025     C                   Z-ADD     17            LUNG
020600101025     C                   MOVEA     CMD(1)        COMMAN
020700101025     C                   CALL      'QCMDEXC'                            21
020800101025     C                   PARM                    COMMAN
020900101025     C                   PARM                    LUNG
021000101025     C*
021100170227     C                   else
021200170227     C                   exsr      EXEERR
021300101025     C                   endif
021400170227     C*
021500170227     C* Sancisco le transazioni logiche per il download del VF
021600170227     C                   z-add     1             jKSU
021700170227     C                   dow       jKSU < %elem(skVGDKSU)
021800170227     C*
021900170227     C* Finchè elementi valorizzati
022000170227     C                   if        skVGDKSU(jKSU) = *blanks
022100170227     C                   leave
022200170227     C                   else
022300170227     C                   eval      DS_VGDKSU = skVGDKSU(jKSU)
022400170227     C*
022500170227     C* Finalizzo la transazione
022600170227     C                   EVAL      i§7VASOPZ  = 'RLS'
022700170227     C                   EVAL      i§7VASTIP  = 'VF'
022800170227     C                   EVAL      i§7VASKSU  = DS_VGDKSU.ds_KSU
022900170227     C                   EVAL      i§7VASPRG  = DS_VGDKSU.ds_PRG
023000170406     C                   EVAL      i§7VASTSC  = DS_VGDKSU.ds_TSC
023100170227     C                   EVAL      i§7VASSTO  = '?'
023200170227     C                   EVAL      i§7VASSTTO = 'L'
023300170227     C                   CALL(e)   'TIS7VASR1'
023400170227     C                   PARM                    TIS7VASDS
023500170227     C                   endif
023600170227     C*
023700170227     C                   add       1             jKSU
023800170227     C                   enddo
023900101025     C*
024000170227     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
024100170227     C                   CLEAR                   TIS7VASDS
024200170227     C                   EVAL      i§7VASOPZ = 'DEL'
024300170227     C                   EVAL      i§7VASKSU = *blanks
024400170227     C                   EVAL      i§7VASTIP = 'VF'
024500170227     C                   CALL(e)   'TIS7VASR1'
024600170227     C                   PARM                    TIS7VASDS
024700101025     C*
024800101025     c                   ENDSR
024900101025     C*----------------------------------------------------*
025000101025     c* operazioni  iniziali
025100101025     C*----------------------------------------------------*
025200910830     C     DEFCAM        BEGSR
025300050905      *
025400050905      * calcola la data corrente
025500160209     c                   z-add     *zeros        datcor            8 0          *d.corrente aa/m/g
025600160209     C                   eval      datcor = %dec(%date() : *ISO)
025700000721      *
025800050905      * Overrida tabel in tabelxx per gestione multimembro (stesso dato a 2 clienti)
025900000721     c                   movel     *blanks       comman
026000000721     c                   movea     cmd5          comman
026100000721     c                   z-add     44            lung
026200050906     c                   exsr      esegue
026300000721     c                   open      tabelxxf
026400941122     C* TABEL
026500921023     C     KTB           KLIST
026600910830     C                   KFLD                    TBLKUT
026700910902     C                   KFLD                    TBLCOD
026800030917      * FIAR
026900030917     c     kFiar5        Klist
027000030917     c                   Kfld                    TasAas
027100030917     c                   Kfld                    TasLnp
027200030917     c                   Kfld                    TasNrs
027300030917     c                   Kfld                    TasNsp
027400030917     c                   Kfld                    kAr5Trd
027500990817     C* TITAS
027600050905     C     KTAS          KLIST
027700050905     C                   KFLD                    KSC
027800050905     C                   KFLD                    DAL
027900990817     C* TITA7
028000050905     C     KTA7          KLIST
028100941122     C                   KFLD                    TASAAS
028200941122     C                   KFLD                    TASLNP
028300941122     C                   KFLD                    TASNRS
028400941122     C                   KFLD                    TASNSP
028500050905     C                   KFLD                    TASTBL
028600990910     C     *LIKE         DEFINE    TASVA1        WVA
028700990910     C     *LIKE         DEFINE    TASSV1        WSV
028800010402     C*
028900050905     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
029000050905     C     KORM          KLIST
029100160209     C                   KFLD                    §TA4MPOE
029200160209     C                   KFLD                    §TA4MNSR
029300160209     C                   KFLD                    §TA4MNOR
029400160209     C                   KFLD                    §TA4MNRV
029500990817     C*
029600990817     C* LEGGE IL RECORD SIGLE VARIE
029700990817     C     *LIKE         DEFINE    TBLKUT        KTBKUT
029800990817     C     *LIKE         DEFINE    TBLCOD        KTBCOD
029900990817     C     *LIKE         DEFINE    TBLKEY        KTBKEY
030000000721     C     *LIKE         DEFINE    TBLCOD        KTBCODx
030100000721     C     *LIKE         DEFINE    TBLKEY        KTBKEYx
030200000721     C     KEYTABx       KLIST
030300000721     C                   KFLD                    KTBKUT
030400000721     C                   KFLD                    KTBCODx
030500000721     C                   KFLD                    KTBKEYx
030600990817     C     KEYTAB        KLIST
030700990817     C                   KFLD                    KTBKUT
030800990817     C                   KFLD                    KTBCOD
030900990817     C                   KFLD                    KTBKEY
031000021212      *
031100021212      ** Aggancio tabella $2 - Varie forzate a programma
031200990817     C                   Z-ADD     1             KTBKUT
031300990817     C                   MOVEL     '$2'          KTBCOD
031400990817     C                   MOVEL     1             KTBKEY
031500990817     C     KEYTAB        CHAIN     TABEL00F                           99
031600990817IF  1C     *IN99         IFEQ      *OFF
031700990817     C     TBLFLG        ANDEQ     *BLANKS
031800990817     C                   MOVEL     TBLUNI        DS$2
031900990817E   1C                   ENDIF
032000021212      *
032100050905      ** Aggancio tabella QT - campi standard tassazione
032200021212     C                   MOVEL     'QT'          KTBCOD
032300021212     C                   MOVEL     1             KTBKEY
032400021212     C     KEYTAB        CHAIN     TABEL00F                           99
032500021212IF  1C     *IN99         IFEQ      *OFF
032600021212     C     TBLFLG        ANDEQ     *BLANKS
032700021212     C                   MOVEL     TBLUNI        DSQT1
032800021212E   1C                   ENDIF
032900990817     C*
033000021212      ** Imposto key per tabella clienti con ritorno info fattura
033100990817     C                   MOVE      1             TBLKUT
033200990817     C                   MOVEL     '4C'          TBLCOD
033300990817     C                   MOVEL     *BLANKS       TBLKEY
033400990817     C*
033500910830     C                   ENDSR
033600050906     C*--------------------------------------------------------
033700050905     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
033800000124     C*--------------------------------------------------------
033900921023     C     WRTFIL        BEGSR
034000990817     C     KTAS          SETLL     TITAS31C
034100921023     C                   DO        *HIVAL
034200990817     C     KSC           READE     TITAS31C                               70
034300000124     C  N70              DO
034400000124     C     TASNFT        IFGT      *ZERO                                        N.FATT.
034500921023     C     TASDFT        ANDLE     AL
034600140108
034700140108     C*
034800140108     C* Se richiesto filtro per numero fattura => verifico
034900140108     C                   IF        NFT <> *zeros AND
035000140108     C                             NFT <> tasNFT
035100140108     C* Skippo spedizione corrente
035200140108     C                   ELSE
035300050126
035400050126      * per prima cosa controllo se devo scrivere un membro apposta x l'accorpamento bolle
035500111026     c****               If        Tasxco <> *Blanks
035600050906      * se in tabella è prevista la creazione di un nuovo membro
035700111026     c****               If        §4cRfo = 'X'    and
035800050906      * e non era già stato aperto il membro x questo cliente
035900111026     c****                         wxco = *Off
036000050906      * chiudo il vecchio membro e apro il nuovo
036100111026     c****               Move      ') '          var               9
036200111026     c****               Movel     §4cMbx        var
036300111026     c****               ExSr      opnfil
036400111026     c****               Eval      wxco = *On
036500111026     c****               EndIf
036600111026     c****               EndIf
036700101025     c*
036800101025     c                   exsr      SCriviDati
036900101025     c
037000050906     C*
037100050906     C* Verifico se richiesta scrittura membro "particolare" x gestone XCO
037200050906     C                   if        wxco = *on
037300050906     C                   write     tivaf000
037400050906     C                   else
037500060203     C*
037600060203     C* Scrivo il file TIVGD00F (file VAS Generico Download)
037700140319     C                   movel     *all'0'       curKSU
037800140319     C                   if        not *in45
037900060203     C                   move      KSCU          curKSU
038000140319     C                   else
038100140319     C                   move      CLIVAS        curKSU
038200140319     C                   endif
038300060203     C                   eval      curTSC = §4CFSD
038400060203     C*
038500060203     C* Verifica il tipo scarico attribuito al cliente su cui unificare i dati del cliente corrente
038600060203     C                   clear                   DS4C_W
038700060203     C                   move      '4C'          ktbcodx
038800060203     C                   movel     KSCU          ktbkeyx
038900060203     C     keytabx       chain     tabelxxf                           89
039000060203     C                   if        %found(tabelxxf)
039100060203     C                   movel     xtbluni       DS4C_W
039200060203     C                   eval      curTSC = W_§4CFSD
039300060203     C                   endif
039400151016     C*
039500151016     C* Eseguo eventuali "forzature"
039600151016     C                   EXSR      RTVAR5FAT
039700060203     C*
039800060203     C* Quindi scarico il buffer nel file d out generico download
039900050905     C                   exsr      WRIVGD
040000050906     C                   endif
040100050905     C*
040200050905     C* Verifica se richiesta gestione multimembro (stesso dato a 2 clienti)
040300111018     c**!!!              move      '4R'          ktbcodx
040400111018     c**!!!              movel     tblkey        ktbkeyx
040500111018     c**!!!keytabx       chain     tabelxxf                           89
040600111018     c**!!!              if        %found(tabelxxf)
040700111018     c**!!!              movel     xtbluni       ds4r
040800111018     C**!!!              movel     *all'0'       curKSU
040900111018     C**!!!              move      §4RCKS        curKSU
041000111018     C**!!!              eval      curTSC = §4RCSD
041100111018     C**!!!              exsr      WRIVGD
041200111018     c**!!!              endif
041300050905     C*
041400140108     C                   ENDIF
041500140108     C*
041600940910     C                   END
041700921023     C                   END
041800000124     C  N70              END
041900000124     C                   ENDSR
042000101025     C*-----------------------------------------------------------*
042100101025     C* Scrivo dati nel file TIVAF
042200101025     C*-----------------------------------------------------------*
042300101025     c     Scrividati    BEGSR
042400101025     C*
042500101025     C* Inizializzo il buffer d output
042600101025     C                   clear                   TIVAFDS
042700160208     C*
042800160209     C* Reperisco TITA4 tipo record 'A'
042900160209     C                   MOVEL     'A'           UBTA4ITRC
043000160209     C                   EXSR      RTVTA4
043100101025     C*
043200101025     C                   MOVEL     TASTBL        VAFTBL
043300101025     C                   Z-ADD     TASLNP        VAFLNP
043400101025     C                   Z-ADD     TASMGS        VAFMGS
043500101025     C                   Z-ADD     TASAAS        VAFAAS
043600101025     C                   Z-ADD     TASNRS        VAFNRS
043700101025     C                   Z-ADD     TASNSP        VAFNSP
043800101025     C                   Z-ADD     TASKSC        VAFKSC
043900101025     C                   MOVEL     TASFIN        VAFFIN
044000101025     C                   Z-ADD     TASLNA        VAFLNA
044100101025      * se particolità varia = 'O' recupero da FIAR5 il numero colli originali
044200101025      *
044300101025     c                   If        %Subst(TasGva:1:1) = 'O'
044400101025
044500101025     c                   eval      kAr5Trd  = 'BNB'
044600101025     c                   clear                   dAr5Bnb
044700101025     c     kFiar5        Chain     Fiar531c
044800101025     c                   If        %Found(Fiar531c)
044900101025     c                   Movel     Ar5Uni        dAr5Bnb
045000101025     c                   EndIf
045100101025      * Colli originali
045200101025     c                   Eval      VafNcl = §Ar5bcor
045300101025
045400101025     c                   else
045500101025      * Colli TITAS
045600101025     C                   Z-ADD     TASNCL        VAFNCL
045700101025     c                   endif
045800101025
045900101025     C                   Z-ADD     TASVLF        VAFVLF
046000101025     C                   Z-ADD     TASPOR        VAFPOR
046100101025      * PESO
046200101025     C                   Z-ADD     TASPKF        VAFPKB
046300101025     C                   IF        TASFPF = 'V'
046400150716     C*
046500150716     C                   IF        TASDFT <= §QTDST
046600150716     C     §QTTCO        MULT      TASNCP        WTARA
046700150716     C     TASPKC        SUB(h)    WTARA         WNTARA
046800150716     C                   Z-ADD     WNTARA        VAFPKC
046900150716     C                   ELSE
047000101025     C     §QTTPC        MULT      TASNCP        WTARA
047100101025     C     TASPKC        SUB(h)    WTARA         WNTARA
047200150716     C                   Z-ADD     WNTARA        VAFPKC
047300150716     C                   ENDIF
047400150716     C*
047500101025     C                   ELSE
047600151020     C                   IF        TASFPF <> 'D'
047700150716     C                   Z-ADD     *zeros        VAFPKC
047800151020     C                   ENDIF
047900101025     C                   ENDIF
048000150716     C*
048100101025     C* AZZERO CAMPI NON SEMPRE PRESENTI
048200101025     C                   MOVE      *ZERO         WSV
048300101025     C                   MOVE      *ZERO         WVA
048400101025     C                   MOVEA     *BLANKS       VSV
048500101025     C                   MOVEA     *ZERO         VVA
048600101025     C                   CLEAR                   A
048700101025     C                   CLEAR                   VAFSV1
048800101025     C                   CLEAR                   VAFVA1
048900101025     C                   CLEAR                   VAFSV2
049000101025     C                   CLEAR                   VAFVA2
049100101025     C                   CLEAR                   VAFSV3
049200101025     C                   CLEAR                   VAFVA3
049300101025     C                   CLEAR                   VAFSV4
049400101025     C                   CLEAR                   VAFVA4
049500101025     C                   CLEAR                   VAFSV5
049600101025     C                   CLEAR                   VAFVA5
049700101025     C                   CLEAR                   VAFSV6
049800101025     C                   CLEAR                   VAFVA6
049900101025     C                   CLEAR                   VAFSV7
050000101025     C                   CLEAR                   VAFVA7
050100101025     C                   CLEAR                   VAFSV8
050200101025     C                   CLEAR                   VAFVA8
050300101025     C                   CLEAR                   VAFVAX
050400101025     C*
050500101025     C                   MOVEL     TASSV1        WSV
050600101025     C                   Z-ADD     TASVA1        WVA
050700101025     C                   EXSR      CONTR
050800101025     C                   MOVEL     TASSV2        WSV
050900101025     C                   Z-ADD     TASVA2        WVA
051000101025     C                   EXSR      CONTR
051100101025     C                   MOVEL     TASSV3        WSV
051200101025     C                   Z-ADD     TASVA3        WVA
051300101025     C                   EXSR      CONTR
051400101025     C*
051500101025     C                   Z-ADD     TASDFT        VAFDFT
051600101025     C                   Z-ADD     TASNFT        VAFNFT
051700101025      *
051800101025     c                   if        tascbo <> 'FO'
051900101025     C                   Z-ADD     TASRMN        VAFRMN
052000101025     c                   endif
052100101025      *
052200101025     C                   MOVEL     TASFTC        VAFFTC
052300101025     C                   MOVEL     TASTSP        VAFTSP
052400101025     C                   MOVEL     TASCTS        VAFCTS
052500101025     C                   MOVEL     TASFAP        VAFFAP
052600101025     C                   Z-ADD     TASCTR        VAFCTR
052700101025     C                   Z-ADD     TASQFT        VAFQFT
052800101025     C                   MOVEL     TASTC2        VAFTC2
052900101025     C                   Z-ADD     TASIMV        VAFIMV
053000101025     C                   Z-ADD     TASFIV        VAFFIV
053100101025     C                   MOVEL     TASDIV        VAFDIV
053200101025     C                   Z-ADD     TASDRT        VAFDRT
053300160208     C                   MOVEL     §TA4ANAS      VAFNAS
053400101025     C                   MOVEL     TASCAD        VAFCAD
053500101025     C                   MOVEL     TASLOD        VAFLOD
053600101025     C                   MOVEL     TASPRD        VAFPRD
053700101025     C                   MOVEL     TASNZD        VAFNZD
053800101025     C                   MOVEL     TASVAS        VAFVAS
053900101025     C                   Z-ADD     TASIAS        VAFIAS
054000101025    *C                   MOVEL     *blanks       VAFFPC
054100101025    *C                   MOVEL     *blanks       VAFFVC
054200101025    *C                   Z-ADD     *zeros        VAFVLC
054300101025     C     TASSV3        IFNE      *BLANK
054400101025     C                   EXSR      SRTA7
054500101025     C                   END
054600101025     C                   MOVEL     *BLANKS       VAFRMA
054700101025     C* se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
054800160222     c                   if        tascbo <> 'FO'
054900160208     C                   MOVEL     DTA4A         VAFRMA
055000101025     C                   ENDIF
055100101025     C* se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
055200160222     c                   if        tascbo = 'FO'
055300101025     c                   if        §ta4arma <> *blanks
055400101025     C                   MOVEL     §TA4ARMA      Rma15
055500101025      * verifico se campo numerico escludendo i blanks
055600101025     c                   eval      II = 1
055700101025     c                   dow       II <= %len(%trim(Rma15))
055800101025      *
055900101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
056000101025     c                   eval      WRma15 = (%trim(Wrma15)
056100101025     c                                      + %subst(%trim(Rma15):II:1))
056200101025     c                   endif
056300101025      *
056400101025     c                   eval      II = II + 1
056500101025      *
056600101025     c                   enddo
056700101025     c                   clear                   rma15
056800101025     c                   evalr     rma15 = %trim(Wrma15)
056900101025      *
057000101025     c                   eval      II = 1
057100101025     c                   setoff                                       29
057200101025     c                   dow       II <= %len(%trim(Rma15))
057300101025      *
057400101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
057500101025     c                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
057600101025     c                             VALNUM(1))
057700101025     c                   if        pos = 0
057800101025     c                   seton                                        29
057900101025     c                   leave
058000101025     c                   endif
058100101025     c                   endif
058200101025      *
058300101025     c                   eval      II = II + 1
058400101025      *
058500101025     c                   enddo
058600101025     c                   if        *in29 = *OFF
058700101025     c                   eval      VAFRMA= %editc(tasrmn:'Z')
058800101025     C                   MOVEL     rma15         VAFRMN
058900101025     c                   else
059000101025     C                   EVAL      VAFRMA = §ta4arma
059100101025     C                   EVAL      VAFRMN = tasrmn
059200101025     c                   endif
059300101025      *
059400101025     C                   endif
059500101025     C                   endif
059600101025     C*
059700101025     C* SCRIVO LE VARIE
059800101025     C                   Z-ADD     1             A
059900101025     C     VSV(A)        DOWNE     ' '
060000101025     C                   SELECT
060100101025     C     VAFSV1        WHENEQ    ' '
060200101025     C                   MOVEL     VSV(A)        VAFSV1
060300101025     C                   Z-ADD     VVA(A)        VAFVA1
060400101025     C*
060500101025     C     VAFSV2        WHENEQ    ' '
060600101025     C                   MOVEL     VSV(A)        VAFSV2
060700101025     C                   Z-ADD     VVA(A)        VAFVA2
060800101025     C*
060900101025     C     VAFSV3        WHENEQ    ' '
061000101025     C                   MOVEL     VSV(A)        VAFSV3
061100101025     C                   Z-ADD     VVA(A)        VAFVA3
061200101025     C*
061300101025     C     VAFSV4        WHENEQ    ' '
061400101025     C                   MOVEL     VSV(A)        VAFSV4
061500101025     C                   Z-ADD     VVA(A)        VAFVA4
061600101025     C*
061700101025     C     VAFSV5        WHENEQ    ' '
061800101025     C                   MOVEL     VSV(A)        VAFSV5
061900101025     C                   Z-ADD     VVA(A)        VAFVA5
062000101025     C*
062100101025     C     VAFSV6        WHENEQ    ' '
062200101025     C                   MOVEL     VSV(A)        VAFSV6
062300101025     C                   Z-ADD     VVA(A)        VAFVA6
062400101025     C*
062500101025     C     VAFSV7        WHENEQ    ' '
062600101025     C                   MOVEL     VSV(A)        VAFSV7
062700101025     C                   Z-ADD     VVA(A)        VAFVA7
062800101025     C*
062900101025     C     VAFSV8        WHENEQ    ' '
063000101025     C                   MOVEL     VSV(A)        VAFSV8
063100101025     C                   Z-ADD     VVA(A)        VAFVA8
063200101025     C                   OTHER
063300101025     C                   ADD       VVA(A)        VAFVAX
063400101025     C                   ENDSL
063500101025     C*
063600101025     C                   ADD       1             A
063700101025     C                   ENDDO
063800101025      * Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
063900101025     c                   Clear                   VafRmx
064000101025     C                   MOVEL     VAFKSC        WLB3              3 0
064100101025     c     WLB3          chain     AZORG
064200101025     c                   if            %found(AZORG01L)
064300101025     c                             and ORGfva = *blanks
064400101025     c                   movel     ORGde3        Og143
064500101025     c                   else
064600101025     c                   clear                   Og143
064700101025     c                   endif
064800101025
064900101025     c                   Select
065000101025
065100160209     C* Recupera Riferimento Alfanumerico da ORM
065200101025     C                   When      §4cRfo = 'S'
065300160209     C*
065400160209     C* Reperisco TITA4 tipo record 'M'
065500160209     C                   MOVEL     'M'           UBTA4ITRC
065600160209     C                   EXSR      RTVTA4
065700160209     C*
065800160209     C                   IF        DTA4M <> *zeros
065900101025     C     KORM          CHAIN     FNORM01L                           61
066000160209     C     *IN61         IFEQ      *OFF
066100101025     C                   MOVEL     ORMRFA        VAFRMX
066200101025     C                   ENDIF
066300160209     C                   ENDIF
066400160209     C*
066500101025      * Ritorna al cliente il campo XCO
066600111026     c**                 When      §4cRfo = 'X' and TasXco <> *Blanks
066700101025      * recupero il valore da tornare dalla tabella VUS
066800111026     c**                 Clear                   Tibs02ds
066900111026     c**                 Clear                   dvus
067000111026     c**                 Eval      T02mod = 'C'
067100111026     c**                 Eval      T02sif = knsif
067200111026     c**                 Eval      T02cod = 'VUS'
067300111026     c**                 Eval      T02ke1 = TasXco
067400111026     c**                 Call      'TIBS02R'
067500111026     c**                 Parm                    kpjba
067600111026     c**                 Parm                    Tibs02ds
067700111026     c**                 If        T02err = *blanks
067800111026     c**                 Eval      dvus = T02uni
067900111026     c**                 EndIf
068000111026     c**                 Eval      vafrmx = §vuvaf
068100101025
068200160209     C*
068300101025     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
068400101025     ***C                   When      wlb3 = 190  or wlb3 = 195 or wlb3 = 191
068500101025     c                   When      §OGntw = 'DPD'
068600101025     C                   movel     TASDCM        VAFRMX
068700101025
068800101025     c                   EndSl
068900101025     c
069000101025     c                   ENDSR
069100000124     C*-----------------------------------------------------------*
069200000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
069300000124     C*-----------------------------------------------------------*
069400941122     C     SRTA7         BEGSR
069500990817     C*
069600990823     C     KTA7          SETLL     TITA730C
069700990823     C     KTA7          READE     TITA730C                               59
069800990817DO  1C     *IN59         DOWEQ     *OFF
069900990910     C                   MOVEL     TA7SVN        WSV
070000990910     C                   Z-ADD     TA7VAN        WVA
070100990910     C                   EXSR      CONTR
070200990910     C*
070300990823     C     KTA7          READE     TITA730C                               59
070400990817E   1C                   ENDDO
070500990817     C*
070600941122     C                   ENDSR
070700990910     C*--------------------------------------------------------
070800000124     C* CONTROLLO SE VARIE GIA' PRESENTI
070900000124     C*--------------------------------------------------------
071000990910     C     CONTR         BEGSR
071100000124IF  1C     WSV           IFNE      ' '
071200990910     C*
071300990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
071400990910     C     WSV           ANDNE     §$2IVA
071500990910     C                   ADD       1             A                 3 0
071600990910     C                   MOVEL     WSV           VSV(A)
071700990910     C                   Z-ADD     WVA           VVA(A)
071800990910E   4C                   ENDIF
071900000124E   1C                   ENDIF
072000000124     C                   ENDSR
072100050905
072200170227
072300170227
072400050905      /TITLE Scrittura record TIVAF in file TIVGD00F (file VAS generico download)
072500170227     C     wriVGD        BEGSR
072600140109     C*
072700050905     C                   clear                   tivgd000
072800080404     C                   eval      vgdDTA = %subst(TIVAFDS:1:%size(TIVAFDS))
072900050905     C                   eval      vgdTIP = 'VF'
073000050905     C                   eval      vgdKSU = curKSU
073100080404     C                   eval      vgdTSC = curTSC
073200050905     C                   eval      vgdDAT = datcor
073300140109     C                   eval      vgdPGM = 'TRTC28R'
073400170227     C                   eval      vgdSTO = '?'
073500140109     C*
073600140109     C*************
073700170406     C* Se specificatamente richiesto nel lancio ...
073800140113     C                   if        FILESEP <> 'N'
073900140113     C*
074000140109     C* Verifico la rottura di codice per KSU / DFT / NFT / FIV
074100140109     C                   eval      ds_KSU = curKSU
074200140109     C                   eval      ds_DFT = tasDFT
074300140109     C                   eval      ds_NFT = tasNFT
074400140109     C                   eval      ds_FIV = tasFIV
074500140109     C*
074600140109     C                   z-add     1             jPRG
074700140109     C     DS_PRG        lookup    skKSCU(jPRG)                           55
074800140109     C                   if        %found
074900140109     C                   eval      vgdPRG = skPRG(jPRG)
075000140109     C                   else
075100140109     C                   exsr      calprog
075200140109     C                   eval      vgdPRG = f_tis7prgf
075300140109     C                   add       1             tPRG
075400140109     C                   eval      skKSCU(tPRG) = DS_PRG
075500140109     C                   eval      skPRG(tPRG)  = vgdPRG
075600140109     C                   endif
075700170308     C*
075800170308     C                   else
075900170308     C                   eval      vgdPRG = savPRG
076000140113     C*
076100140113     C                   endif
076200170227     C*************
076300170227     C*
076400170227     C* Salvo tutti i clienti unificanti scritti
076500170227     C                   eval      DS_VGDKSU.ds_KSU = vgdKSU
076600170227     C                   eval      DS_VGDKSU.ds_PRG = vgdPRG
076700170406     C                   eval      DS_VGDKSU.ds_TSC = vgdTSC
076800170227     C     DS_VGDKSU     lookup    skVGDKSU                               55
076900170227     C                   if        not %found
077000170227     C                   add       1             jKSU
077100170227     C                   eval      skVGDKSU(jKSU) = DS_VGDKSU
077200170227     C                   endif
077300170227     C*
077400170227     C* Scarico ul buffer di output download
077500050905     C                   write     tivgd000
077600050905     C*
077700050905     C                   ENDSR
077800140108
077900140108
078000140108      /TITLE Valorizzazione Progressivo Applicazione
078100140108     C     calprog       begsr
078200140108     C*
078300140108     C                   movel     *blanks       dwlisv            2
078400140108     C                   movel     *all'0'       dwlprg           10
078500140108     C                   z-add     *zeros        wrkprg            8 0
078600140108     C*
078700140108     C                   eval      dwlisv = 'VF'
078800140108     C*
078900170324     C     *start        setll     tis7prgf
079000140108     C                   read(e)   tis7prgf
079100140108     C                   if        not %error
079200140108     C                   eval      dwlprg = f_tis7prgf
079300140108     C*
079400140108     C                   move(p)   dwlprg        wrkprg
079500140108     C                   add       1             wrkprg
079600140108     C                   move(p)   wrkprg        dwlprg
079700140108     C                   movel     dwlisv        dwlprg
079800140108     C*
079900140108     C                   eval      f_tis7prgf = dwlprg
080000140108     C                   update    tis7prg0
080100170324     C                   else
080200170324     C                   eval      f_tis7prgf = 'XX00000000'
080300140108     C                   endif
080400140108     C*
080500140108     C                   endsr
080600140108
080700140108
080800050906     C*--------------------------------------------------------
080900050906     C* OPNFIL    CONTROLLO ESISTENZA DEL MEMBRO (MXXXXXXX    *
081000050906     C*           DOVE XXXXXXX=COD CLI) SE NON C'E AGGIUNGO E *
081100050906     C*           APRO IL FILE                                *
081200050906     C*--------------------------------------------------------
081300050906     C     OPNFIL        BEGSR
081400101025     C                   Z-ADD     50            LUNG             15 5
081500050906     C*
081600050906     C** CONTROLLO SE ESISTE IL MEMBRO
081700050906     C                   SETOFF                                       33        TIVAF00T
081800050906     C                   MOVE      VAR           CMD1
081900050906     C                   MOVEL     *BLANKS       COMMAN           80
082000050906     C                   MOVEA     CMD1(1)       COMMAN
082100050906     C                   CALL      'QCMDEXC'                            33
082200050906     C                   PARM                    COMMAN
082300050906     C                   PARM                    LUNG
082400050906     C** ESEGUE ADDPFM SOLO SE NON ESISTE MEMBRO
082500050906     C   33              DO
082600050906     C                   MOVE      VAR           CMD2
082700050906     C                   MOVEL     *BLANKS       COMMAN
082800050906     C                   MOVEA     CMD2(1)       COMMAN
082900050906     C                   CALL      'QCMDEXC'
083000050906     C                   PARM                    COMMAN
083100050906     C                   PARM                    LUNG
083200050906     C                   END
083300050906     C** FACCIO OVRDBF DEL FILE E LO APRO
083400050906     C                   MOVE      VAR           CMD3
083500050906     C                   MOVEL     *BLANKS       COMMAN
083600050906     C                   MOVEA     CMD3(1)       COMMAN
083700050906     C                   CALL      'QCMDEXC'
083800050906     C                   PARM                    COMMAN
083900050906     C                   PARM                    LUNG
084000050906     C**
084100060203     C                   IF        not %open(TIVAF00T)
084200050906     C                   OPEN      TIVAF00T
084300060203     C                   ENDIF
084400060203     C*
084500050906     C                   ENDSR
084600050905?    C*-------------------------------------------------
084700050906?    C* esegue - esegue comandi "CL"
084800050905?    C*-------------------------------------------------
084900050905     c     esegue        begsr
085000050905     c                   call      'QCMDEXC'                            60
085100050905     c                   parm                    comman
085200050905     c                   parm                    lung
085300050905     c                   endsr
085400101025     C*----------------------------------------------------*
085500101025     c* Scrittuta TIVAF per embro revisori
085600101025     C*----------------------------------------------------*
085700101025     c     ElaREV        BEGSR
085800101026     c
085900101026     C                   EXSR      DEFCAM
086000101026
086100101025     c                   eval      $Fine='0'
086200101025     c                   clear                   ksc
086300101025     c                   eval      VAR='REVISORI)'
086400101025     c                   exsr      opnfil
086500101025     c
086600101025     c* Lettura del primo codice cliente
086700101025    1c                   dow       $Fine='0'
086800101025     c     ksc           setgt     titas31c
086900101025     c                   read      titas31c
087000101025     c
087100101025    2c                   if        %eof(titas31c)
087200101025     c                   eval      $Fine='1'
087300101025   x2c                   else
087400101025     c
087500101025     c                   eval      ksc=tasksc
087600101025     c     ktas          setll     titas31c
087700101025     c     ktas          reade     titas31c
087800101025     c
087900101025    3c                   if        not %eof(titas31c)
088000101025     c* Escludo se non si tratta di cliente codificato
088100101025     c                   move      tasksc        w0040             4 0
088200101025
088300101025    4c                   if        w0040<>8888 and tasksc<>9999
088400101025
088500101025    5c                   dow       not %eof(titas31c)
088600101025     c
088700170712     c                   if        tasfiv>=900
088800101025     c                   exsr      ScriviDati
088900101025     c
089000101025     c                   write     tivaf000
089100101026     c                   endif
089200101025     c
089300101025     c     ktas          reade     titas31c
089400101025    5c                   enddo
089500101025     c
089600101025    4c                   endif
089700101025     c
089800101025    3c                   endif
089900101025    2c                   endif
090000101025    1c                   enddo
090100101025     c*
090200101025     c                   if        %open(tivaf00t)
090300101025     c                   close     tivaf00t
090400101025     c                   endif
090500101025     c
090600101025     c                   ENDSR
090700990817     C*--------------------------------------------------------
090800151016
090900151016
091000151016
091100151016     C     RTVAR5FAT     BEGSR
091200151016     C*
091300151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
091400151028     C                   SETOFF                                       7172
091500151016     C                   EVAL      kAr5Trd = 'FAT'
091600151016     C                   CLEAR                   DAR5FAT
091700151016     C     kFiar5        CHAIN     FIAR531C
091800151016     C                   IF        %found(FIAR531C)
091900151016     C                   MOVEL     AR5UNI        DAR5FAT
092000170130     C                   IF        (§AR5FPTAS = 'T' OR §AR5FPTAS = 'D') AND
092100170130     C                             §AR5PKTAS > *zeros
092200151028     C                   SETON                                        71
092300151016     C                   EVAL      VAFPKC = §AR5PKTAS
092400151016     C                   ENDIF
092500170130     C                   IF        (§AR5FVTAS = 'T' OR §AR5FVTAS = 'D') AND
092600170130     C                             §AR5VLTAS > *zeros
092700151028     C                   SETON                                        72
092800160505     C                   EVAL      VAFVLC = §AR5VLTAS
092900151016     C                   ENDIF
093000151016     C                   ENDIF
093100151016     C*
093200151016     C                   ENDSR
093300160208
093400160208
093500160208
093600160209     C     RTVTA4        BEGSR
093700160208     C*
093800160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
093900160208     C                   CLEAR                   DTA4A
094000160209     C                   CLEAR                   DTA4M
094100160208     C*
094200160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
094300160208     C                   CALL      'UBTA400R'
094400160208     C                   PARM      *blanks       UBTA4IOPZ
094500160208     C                   PARM      *blanks       UBTA4ITLA
094600160208     C                   PARM      tasAAS        UBTA4IAAS
094700160208     C                   PARM      tasLNP        UBTA4ILNP
094800160208     C                   PARM      tasNRS        UBTA4INRS
094900160208     C                   PARM      tasNSP        UBTA4INSP
095000160209     C                   PARM                    UBTA4ITRC
095100160208     C                   PARM                    UBTA4OERR
095200160208     C                   PARM                    UBTA4ODS
095300160208     C                   PARM                    UBTA4OLEN
095400160208     C                   PARM                    UBTA4ODATI
095500160208     C*
095600160208     C                   IF        UBTA4OERR = *zeros
095700160208     C                   SELECT
095800160208     C* Gestione output tipo record 'A'
095900160208     C                   WHEN      UBTA4ODS = 'DTA4A'
096000160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
096100160209     C* Gestione output tipo record 'M'
096200160209     C                   WHEN      UBTA4ODS = 'DTA4M'
096300160209     C                   EVAL      DTA4M = %subst(UBTA4ODATI:1:UBTA4OLEN)
096400160208     C                   ENDSL
096500160208     C*
096600160208     C                   ENDIF
096700160208     C*
096800160208     C                   ENDSR
096900151016     C*--------------------------------------------------------
097000170227
097100170227
097200170227
097300170227     C*------------------------------------------------------------------------*
097400170227     C* EXEERR - Routine di esecuzione azioni su Errore
097500170227     C*------------------------------------------------------------------------*
097600170227     C     EXEERR        BEGSR
097700170227     C*
097800170227     C                   dump(A)
097900170227     C                   rolbk
098000170227     C                   seton                                        lr
098100170227     C                   return
098200170227     C*
098300170227     C                   ENDSR
098400170227     C*------------------------------------------------------------------------*
098500991110**         CMD1
098600000124CHKOBJ OBJ(TIVAF00T) OBJTYPE(*FILE) MBR(M0000000)
098700910830**         CMD2
098800000124ADDPFM FILE(TIVAF00T)               MBR(M0000000)
098900910830**         CMD3
099000000124OVRDBF FILE(TIVAF00T)               MBR(M0000000)
099100000721**         CMD5
099200000724OVRDBF FILE(TABELXXF) TOFILE(*LIBL/TABEL00F)
099300991110**         CMD
099400991110DLTOVR FILE(*ALL)
099500030515**         VALNUM
0996000305150123456789
