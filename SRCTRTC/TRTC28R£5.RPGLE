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
001900000721     FTABELxxF  IF   E           K DISK    rename(tabel:tabelx) prefix(x)
002000010402     F                                     usropn
002100040531     FFIAR531C  IF   E           K DISK
002200060125     FTIVAF00T  UF A E             DISK    usropn
002300050906     FTIVGD00F  UF A E             DISK
002400170227     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002500170227     F                                     PREFIX(f_)
002600140108
002700101025     D CMD1            S             50    DIM(1) CTDATA PERRCD(1)
002800101025     D CMD2            S             50    DIM(1) CTDATA PERRCD(1)
002900101025     D CMD3            S             50    DIM(1) CTDATA PERRCD(1)
003000101025     D CMD5            S             50    DIM(1) CTDATA PERRCD(1)
003100101025     D CMD             S             50    DIM(1) CTDATA PERRCD(1)
003200030515     D VALNUM          S             10    DIM(1) CTDATA PERRCD(10)
003300050906     D VSV             S              1    DIM(30)                              SIGLA VARIE
003400990910     D VVA             S             11  3 DIM(30)                              IMPOR VARIE
003500140109     D
003600050905     D curKSU          s                   like(vgdKSU)
003700050905     D curTSC          s                   like(vgdTSC)
003800170227     D skKSCU          s                   dim(80000) like(DS_PRG)
003900170227     D skPRG           s                   dim(80000) like(vgdPRG)
004000140109     D jPRG            s              5  0 inz
004100140109     D tPRG            s                   inz like(jPRG)
004200140109     D DS_PRG          ds
004300140109     D  ds_KSU                             like(vgdKSU)
004400140109     D  ds_DFT                             like(tasDFT)
004500140109     D  ds_NFT                             like(tasNFT)
004600140109     D  ds_FIV                             like(tasFIV)
004700170227     D skVGDKSU        s                   like(DS_VGDKSU) dim(80000) inz
004800170227     D jKSU            s              5  0 inz
004900170227     D DS_VGDKSU       ds                  qualified inz
005000170227     D  ds_KSU                             like(vgdKSU)
005100170227     D  ds_PRG                             like(vgdPRG)
005200170227
005300890921     D KPJBA         E DS
005400990817     D DS4C          E DS
005500060203     D DS4C_W        E DS                  EXTNAME(DS4C) PREFIX(W_)
005600050906     D DSQT1         E DS
005700111018     D**!!!DS4r          E DS
005800050906     D DS$2          E DS
005900030516     D DTA4A         E DS
006000160209     D DTA4M         E DS
006100050905     D tivafds       e ds                  extname(tivaf00t)
006200170227     D TIS7VASDS     E DS
006300170227
006400050906     D PARAM           DS
006500941122     D  DAL                    1      8  0
006600941122     D  AL                     9     16  0
006700050906     D  CLIU                  17     23  0
006800101025     D  REVISORI              24     24
006900140108     D  NFT                   25     30  0
007000140113     D  FILESEP               31     31
007100140319     D  FLGUNI                32     32
007200140319     D  CLIVAS                33     40
007300010402     D*------------------
007400030917      * DS FIAR5
007500030917     d dAr5Bnb       e ds
007600151016     D DAR5FAT       E DS
007700050126
007800050126     d Tibs02ds      e ds
007900111026     d*******dvus          e ds
008000051219
008100051219     d Og143         e ds                  inz
008200060127
008300030917      *
008400021212     D Wtara           s                   like(§QTTPC)
008500021212     D Wntara          s                   like(TASPKF)
008600030917
008700170308     D savPRG          s                   like(vgdPRG)
008800170308
008900030917     d kAr5Trd         s                   Like(Ar5Trd)
009000030515
009100030515     D Rma15           s             15
009200030515     D WRma15          s             15
009300030515     D II              s              2  0
009400030515     D Pos             s              2  0
009500111026     D Var             s              9
009600050126     d wxco            s              1    inz(*Off)
009700101025     d $Fine           s              1    inz('0')
009800160208
009900160208     D*--------------------
010000160208     D* DEFINIZIONI A PROCEDURE ESTERNE
010100160208     D*--------------------
010200160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
010300160208
010400010402
010500101025     C*------------------------------------------------------------------
010600101025     C     *ENTRY        PLIST
010700101025     C                   PARM                    KPJBA
010800140319     C                   MOVEL     KPJBU         PARAM
010900140319     C*
011000140319     C* Verifico subito se richiesto un particolare cliente VAS (per output)
011100140319     C                   setoff                                       45
011200140319     C                   if        CLIVAS = *all'0' and CLIU <> *zeros
011300140319     C                   move      CLIU          CLIVAS
011400140319     C                   endif
011500140319     C                   if        CLIVAS <> *all'0'
011600140319     C                   seton                                        45
011700140319     C                   endif
011800170308     C*
011900170308     C* Verifico subito se richiesti file separati per ogni fattura
012000170308     C                   if        FILESEP = 'N'
012100170308     C                   exsr      calprog
012200170308     C                   eval      savPRG = f_tis7prgf
012300170308     C                   else
012400170308     C                   eval      savPRG = *blanks
012500170308     C                   endif
012600140319     c*
012700101025     c* Verifico se elaborazione per REVISORI o per scarico a clienti
012800101025     c                   if        revisori='S'
012900101025     c                   exsr      ElaREV
013000101025     c                   else
013100101025     c                   exsr      ElaCLI
013200101025     c                   endif
013300160208     C*
013400160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
013500160208     C                   CALL      'UBTA400R'
013600160208     C                   PARM                    UBTA4IOPZ
013700160208     C                   PARM      'C'           UBTA4ITLA
013800160208     C                   PARM                    UBTA4IAAS
013900160208     C                   PARM                    UBTA4ILNP
014000160208     C                   PARM                    UBTA4INRS
014100160208     C                   PARM                    UBTA4INSP
014200160208     C                   PARM                    UBTA4ITRC
014300160208     C                   PARM                    UBTA4OERR
014400160208     C                   PARM                    UBTA4ODS
014500160208     C                   PARM                    UBTA4OLEN
014600160208     C                   PARM                    UBTA4ODATI
014700160208     C*
014800921023     C                   SETON                                        LR
014900910830     C*----------------------------------------------------*
015000101025     c* Scrittuta TIVGD per i clienti che lo hanno richiesto
015100101025     C*----------------------------------------------------*
015200101025     c     ElaCLI        BEGSR
015300101025     c
015400101025     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VF'
015500170227     C                   CLEAR                   TIS7VASDS
015600170227     C                   EVAL      i§7VASOPZ = 'SEM'
015700170227     C                   EVAL      i§7VASKSU = *blanks
015800170227     C                   EVAL      i§7VASTIP = 'VF'
015900170227     C                   CALL(e)   'TIS7VASR1'
016000170227     C                   PARM                    TIS7VASDS
016100170227     C*
016200170227     C* Verifico esito chiamata
016300170227     C                   if        not %error AND
016400170227     C                             o§7VASOK = *on
016500101025     C*
016600101025     C                   EXSR      DEFCAM
016700101025      *
016800101025     C     KTB           SETLL     TABEL
016900140326     C                   SETOFF                                       58
017000140326    1C                   IF        %equal(TABEL00F)
017100140326     C     KTB           READE     TABEL
017200140326    2C                   DOW       not %eof(TABEL00F)
017300101025    3C     TBLFLG        IFNE      '*'
017400101025     C                   MOVEL     TBLUNI        DS4C
017500101025    4C     §4CFSD        IFNE      *BLANK                                        CLI DA SCAR
017600101025     C                   MOVE      §4CFKS        KSCU              7 0
017700101025     C                   MOVEL     TBLKEY        KSC               7 0
017800101025     C*
017900140326    5C                   if        (CLIU>*zeros AND FLGUNI='S' AND CLIU=KSCU) OR
018000140326     C                             (CLIU>*zeros AND FLGUNI='N')               OR
018100140319     C                              CLIU=*zeros
018200140326     C                   if         CLIU>*zeros AND FLGUNI='N'
018300140326     C                   eval      KSC = CLIU
018400140326     C                   seton                                        58
018500140326     C                   endif
018600140326     C*
018700101025     c                   Eval      wxco = *Off
018800101025     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
018900140326   e5C                   endif
019000140326     C*
019100140326     C                   if        *in58
019200140326     C                   leave
019300140326     C                   endif
019400140326     C*
019500140326   e4C                   ENDIF
019600140326   e3C                   ENDIF
019700140326     C     KTB           READE     TABEL
019800140326   e2C                   ENDDO
019900140326   e1C                   ENDIF
020000101025     C*
020100170227     C* CANCELLO LE OVR ESEGUITE
020200101025     C                   MOVEL     *BLANKS       COMMAN           80
020300101025     C                   Z-ADD     17            LUNG
020400101025     C                   MOVEA     CMD(1)        COMMAN
020500101025     C                   CALL      'QCMDEXC'                            21
020600101025     C                   PARM                    COMMAN
020700101025     C                   PARM                    LUNG
020800101025     C*
020900170227     C                   else
021000170227     C                   exsr      EXEERR
021100101025     C                   endif
021200170227     C*
021300170227     C* Sancisco le transazioni logiche per il download del VF
021400170227     C                   z-add     1             jKSU
021500170227     C                   dow       jKSU < %elem(skVGDKSU)
021600170227     C*
021700170227     C* Finchè elementi valorizzati
021800170227     C                   if        skVGDKSU(jKSU) = *blanks
021900170227     C                   leave
022000170227     C                   else
022100170227     C                   eval      DS_VGDKSU = skVGDKSU(jKSU)
022200170227     C*
022300170227     C* Finalizzo la transazione
022400170227     C                   EVAL      i§7VASOPZ  = 'RLS'
022500170227     C                   EVAL      i§7VASTIP  = 'VF'
022600170227     C                   EVAL      i§7VASKSU  = DS_VGDKSU.ds_KSU
022700170227     C                   EVAL      i§7VASPRG  = DS_VGDKSU.ds_PRG
022800170227     C                   EVAL      i§7VASTSC  = 'WW'
022900170227     C                   EVAL      i§7VASSTO  = '?'
023000170227     C                   EVAL      i§7VASSTTO = 'L'
023100170227     C                   CALL(e)   'TIS7VASR1'
023200170227     C                   PARM                    TIS7VASDS
023300170227     C                   endif
023400170227     C*
023500170227     C                   add       1             jKSU
023600170227     C                   enddo
023700101025     C*
023800170227     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
023900170227     C                   CLEAR                   TIS7VASDS
024000170227     C                   EVAL      i§7VASOPZ = 'DEL'
024100170227     C                   EVAL      i§7VASKSU = *blanks
024200170227     C                   EVAL      i§7VASTIP = 'VF'
024300170227     C                   CALL(e)   'TIS7VASR1'
024400170227     C                   PARM                    TIS7VASDS
024500101025     C*
024600101025     c                   ENDSR
024700101025     C*----------------------------------------------------*
024800101025     c* operazioni  iniziali
024900101025     C*----------------------------------------------------*
025000910830     C     DEFCAM        BEGSR
025100050905      *
025200050905      * calcola la data corrente
025300160209     c                   z-add     *zeros        datcor            8 0          *d.corrente aa/m/g
025400160209     C                   eval      datcor = %dec(%date() : *ISO)
025500000721      *
025600050905      * Overrida tabel in tabelxx per gestione multimembro (stesso dato a 2 clienti)
025700000721     c                   movel     *blanks       comman
025800000721     c                   movea     cmd5          comman
025900000721     c                   z-add     44            lung
026000050906     c                   exsr      esegue
026100000721     c                   open      tabelxxf
026200941122     C* TABEL
026300921023     C     KTB           KLIST
026400910830     C                   KFLD                    TBLKUT
026500910902     C                   KFLD                    TBLCOD
026600030917      * FIAR
026700030917     c     kFiar5        Klist
026800030917     c                   Kfld                    TasAas
026900030917     c                   Kfld                    TasLnp
027000030917     c                   Kfld                    TasNrs
027100030917     c                   Kfld                    TasNsp
027200030917     c                   Kfld                    kAr5Trd
027300990817     C* TITAS
027400050905     C     KTAS          KLIST
027500050905     C                   KFLD                    KSC
027600050905     C                   KFLD                    DAL
027700990817     C* TITA7
027800050905     C     KTA7          KLIST
027900941122     C                   KFLD                    TASAAS
028000941122     C                   KFLD                    TASLNP
028100941122     C                   KFLD                    TASNRS
028200941122     C                   KFLD                    TASNSP
028300050905     C                   KFLD                    TASTBL
028400990910     C     *LIKE         DEFINE    TASVA1        WVA
028500990910     C     *LIKE         DEFINE    TASSV1        WSV
028600010402     C*
028700050905     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
028800050905     C     KORM          KLIST
028900160209     C                   KFLD                    §TA4MPOE
029000160209     C                   KFLD                    §TA4MNSR
029100160209     C                   KFLD                    §TA4MNOR
029200160209     C                   KFLD                    §TA4MNRV
029300990817     C*
029400990817     C* LEGGE IL RECORD SIGLE VARIE
029500990817     C     *LIKE         DEFINE    TBLKUT        KTBKUT
029600990817     C     *LIKE         DEFINE    TBLCOD        KTBCOD
029700990817     C     *LIKE         DEFINE    TBLKEY        KTBKEY
029800000721     C     *LIKE         DEFINE    TBLCOD        KTBCODx
029900000721     C     *LIKE         DEFINE    TBLKEY        KTBKEYx
030000000721     C     KEYTABx       KLIST
030100000721     C                   KFLD                    KTBKUT
030200000721     C                   KFLD                    KTBCODx
030300000721     C                   KFLD                    KTBKEYx
030400990817     C     KEYTAB        KLIST
030500990817     C                   KFLD                    KTBKUT
030600990817     C                   KFLD                    KTBCOD
030700990817     C                   KFLD                    KTBKEY
030800021212      *
030900021212      ** Aggancio tabella $2 - Varie forzate a programma
031000990817     C                   Z-ADD     1             KTBKUT
031100990817     C                   MOVEL     '$2'          KTBCOD
031200990817     C                   MOVEL     1             KTBKEY
031300990817     C     KEYTAB        CHAIN     TABEL00F                           99
031400990817IF  1C     *IN99         IFEQ      *OFF
031500990817     C     TBLFLG        ANDEQ     *BLANKS
031600990817     C                   MOVEL     TBLUNI        DS$2
031700990817E   1C                   ENDIF
031800021212      *
031900050905      ** Aggancio tabella QT - campi standard tassazione
032000021212     C                   MOVEL     'QT'          KTBCOD
032100021212     C                   MOVEL     1             KTBKEY
032200021212     C     KEYTAB        CHAIN     TABEL00F                           99
032300021212IF  1C     *IN99         IFEQ      *OFF
032400021212     C     TBLFLG        ANDEQ     *BLANKS
032500021212     C                   MOVEL     TBLUNI        DSQT1
032600021212E   1C                   ENDIF
032700990817     C*
032800021212      ** Imposto key per tabella clienti con ritorno info fattura
032900990817     C                   MOVE      1             TBLKUT
033000990817     C                   MOVEL     '4C'          TBLCOD
033100990817     C                   MOVEL     *BLANKS       TBLKEY
033200990817     C*
033300910830     C                   ENDSR
033400050906     C*--------------------------------------------------------
033500050905     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
033600000124     C*--------------------------------------------------------
033700921023     C     WRTFIL        BEGSR
033800990817     C     KTAS          SETLL     TITAS31C
033900921023     C                   DO        *HIVAL
034000990817     C     KSC           READE     TITAS31C                               70
034100000124     C  N70              DO
034200000124     C     TASNFT        IFGT      *ZERO                                        N.FATT.
034300921023     C     TASDFT        ANDLE     AL
034400140108
034500140108     C*
034600140108     C* Se richiesto filtro per numero fattura => verifico
034700140108     C                   IF        NFT <> *zeros AND
034800140108     C                             NFT <> tasNFT
034900140108     C* Skippo spedizione corrente
035000140108     C                   ELSE
035100050126
035200050126      * per prima cosa controllo se devo scrivere un membro apposta x l'accorpamento bolle
035300111026     c****               If        Tasxco <> *Blanks
035400050906      * se in tabella è prevista la creazione di un nuovo membro
035500111026     c****               If        §4cRfo = 'X'    and
035600050906      * e non era già stato aperto il membro x questo cliente
035700111026     c****                         wxco = *Off
035800050906      * chiudo il vecchio membro e apro il nuovo
035900111026     c****               Move      ') '          var               9
036000111026     c****               Movel     §4cMbx        var
036100111026     c****               ExSr      opnfil
036200111026     c****               Eval      wxco = *On
036300111026     c****               EndIf
036400111026     c****               EndIf
036500101025     c*
036600101025     c                   exsr      SCriviDati
036700101025     c
036800050906     C*
036900050906     C* Verifico se richiesta scrittura membro "particolare" x gestone XCO
037000050906     C                   if        wxco = *on
037100050906     C                   write     tivaf000
037200050906     C                   else
037300060203     C*
037400060203     C* Scrivo il file TIVGD00F (file VAS Generico Download)
037500140319     C                   movel     *all'0'       curKSU
037600140319     C                   if        not *in45
037700060203     C                   move      KSCU          curKSU
037800140319     C                   else
037900140319     C                   move      CLIVAS        curKSU
038000140319     C                   endif
038100060203     C                   eval      curTSC = §4CFSD
038200060203     C*
038300060203     C* Verifica il tipo scarico attribuito al cliente su cui unificare i dati del cliente corrente
038400060203     C                   clear                   DS4C_W
038500060203     C                   move      '4C'          ktbcodx
038600060203     C                   movel     KSCU          ktbkeyx
038700060203     C     keytabx       chain     tabelxxf                           89
038800060203     C                   if        %found(tabelxxf)
038900060203     C                   movel     xtbluni       DS4C_W
039000060203     C                   eval      curTSC = W_§4CFSD
039100060203     C                   endif
039200151016     C*
039300151016     C* Eseguo eventuali "forzature"
039400151016     C                   EXSR      RTVAR5FAT
039500060203     C*
039600060203     C* Quindi scarico il buffer nel file d out generico download
039700050905     C                   exsr      WRIVGD
039800050906     C                   endif
039900050905     C*
040000050905     C* Verifica se richiesta gestione multimembro (stesso dato a 2 clienti)
040100111018     c**!!!              move      '4R'          ktbcodx
040200111018     c**!!!              movel     tblkey        ktbkeyx
040300111018     c**!!!keytabx       chain     tabelxxf                           89
040400111018     c**!!!              if        %found(tabelxxf)
040500111018     c**!!!              movel     xtbluni       ds4r
040600111018     C**!!!              movel     *all'0'       curKSU
040700111018     C**!!!              move      §4RCKS        curKSU
040800111018     C**!!!              eval      curTSC = §4RCSD
040900111018     C**!!!              exsr      WRIVGD
041000111018     c**!!!              endif
041100050905     C*
041200140108     C                   ENDIF
041300140108     C*
041400940910     C                   END
041500921023     C                   END
041600000124     C  N70              END
041700000124     C                   ENDSR
041800101025     C*-----------------------------------------------------------*
041900101025     C* Scrivo dati nel file TIVAF
042000101025     C*-----------------------------------------------------------*
042100101025     c     Scrividati    BEGSR
042200101025     C*
042300101025     C* Inizializzo il buffer d output
042400101025     C                   clear                   TIVAFDS
042500160208     C*
042600160209     C* Reperisco TITA4 tipo record 'A'
042700160209     C                   MOVEL     'A'           UBTA4ITRC
042800160209     C                   EXSR      RTVTA4
042900101025     C*
043000101025     C                   MOVEL     TASTBL        VAFTBL
043100101025     C                   Z-ADD     TASLNP        VAFLNP
043200101025     C                   Z-ADD     TASMGS        VAFMGS
043300101025     C                   Z-ADD     TASAAS        VAFAAS
043400101025     C                   Z-ADD     TASNRS        VAFNRS
043500101025     C                   Z-ADD     TASNSP        VAFNSP
043600101025     C                   Z-ADD     TASKSC        VAFKSC
043700101025     C                   MOVEL     TASFIN        VAFFIN
043800101025     C                   Z-ADD     TASLNA        VAFLNA
043900101025      * se particolità varia = 'O' recupero da FIAR5 il numero colli originali
044000101025      *
044100101025     c                   If        %Subst(TasGva:1:1) = 'O'
044200101025
044300101025     c                   eval      kAr5Trd  = 'BNB'
044400101025     c                   clear                   dAr5Bnb
044500101025     c     kFiar5        Chain     Fiar531c
044600101025     c                   If        %Found(Fiar531c)
044700101025     c                   Movel     Ar5Uni        dAr5Bnb
044800101025     c                   EndIf
044900101025      * Colli originali
045000101025     c                   Eval      VafNcl = §Ar5bcor
045100101025
045200101025     c                   else
045300101025      * Colli TITAS
045400101025     C                   Z-ADD     TASNCL        VAFNCL
045500101025     c                   endif
045600101025
045700101025     C                   Z-ADD     TASVLF        VAFVLF
045800101025     C                   Z-ADD     TASPOR        VAFPOR
045900101025      * PESO
046000101025     C                   Z-ADD     TASPKF        VAFPKB
046100101025     C                   IF        TASFPF = 'V'
046200150716     C*
046300150716     C                   IF        TASDFT <= §QTDST
046400150716     C     §QTTCO        MULT      TASNCP        WTARA
046500150716     C     TASPKC        SUB(h)    WTARA         WNTARA
046600150716     C                   Z-ADD     WNTARA        VAFPKC
046700150716     C                   ELSE
046800101025     C     §QTTPC        MULT      TASNCP        WTARA
046900101025     C     TASPKC        SUB(h)    WTARA         WNTARA
047000150716     C                   Z-ADD     WNTARA        VAFPKC
047100150716     C                   ENDIF
047200150716     C*
047300101025     C                   ELSE
047400151020     C                   IF        TASFPF <> 'D'
047500150716     C                   Z-ADD     *zeros        VAFPKC
047600151020     C                   ENDIF
047700101025     C                   ENDIF
047800150716     C*
047900101025     C* AZZERO CAMPI NON SEMPRE PRESENTI
048000101025     C                   MOVE      *ZERO         WSV
048100101025     C                   MOVE      *ZERO         WVA
048200101025     C                   MOVEA     *BLANKS       VSV
048300101025     C                   MOVEA     *ZERO         VVA
048400101025     C                   CLEAR                   A
048500101025     C                   CLEAR                   VAFSV1
048600101025     C                   CLEAR                   VAFVA1
048700101025     C                   CLEAR                   VAFSV2
048800101025     C                   CLEAR                   VAFVA2
048900101025     C                   CLEAR                   VAFSV3
049000101025     C                   CLEAR                   VAFVA3
049100101025     C                   CLEAR                   VAFSV4
049200101025     C                   CLEAR                   VAFVA4
049300101025     C                   CLEAR                   VAFSV5
049400101025     C                   CLEAR                   VAFVA5
049500101025     C                   CLEAR                   VAFSV6
049600101025     C                   CLEAR                   VAFVA6
049700101025     C                   CLEAR                   VAFSV7
049800101025     C                   CLEAR                   VAFVA7
049900101025     C                   CLEAR                   VAFSV8
050000101025     C                   CLEAR                   VAFVA8
050100101025     C                   CLEAR                   VAFVAX
050200101025     C*
050300101025     C                   MOVEL     TASSV1        WSV
050400101025     C                   Z-ADD     TASVA1        WVA
050500101025     C                   EXSR      CONTR
050600101025     C                   MOVEL     TASSV2        WSV
050700101025     C                   Z-ADD     TASVA2        WVA
050800101025     C                   EXSR      CONTR
050900101025     C                   MOVEL     TASSV3        WSV
051000101025     C                   Z-ADD     TASVA3        WVA
051100101025     C                   EXSR      CONTR
051200101025     C*
051300101025     C                   Z-ADD     TASDFT        VAFDFT
051400101025     C                   Z-ADD     TASNFT        VAFNFT
051500101025      *
051600101025     c                   if        tascbo <> 'FO'
051700101025     C                   Z-ADD     TASRMN        VAFRMN
051800101025     c                   endif
051900101025      *
052000101025     C                   MOVEL     TASFTC        VAFFTC
052100101025     C                   MOVEL     TASTSP        VAFTSP
052200101025     C                   MOVEL     TASCTS        VAFCTS
052300101025     C                   MOVEL     TASFAP        VAFFAP
052400101025     C                   Z-ADD     TASCTR        VAFCTR
052500101025     C                   Z-ADD     TASQFT        VAFQFT
052600101025     C                   MOVEL     TASTC2        VAFTC2
052700101025     C                   Z-ADD     TASIMV        VAFIMV
052800101025     C                   Z-ADD     TASFIV        VAFFIV
052900101025     C                   MOVEL     TASDIV        VAFDIV
053000101025     C                   Z-ADD     TASDRT        VAFDRT
053100160208     C                   MOVEL     §TA4ANAS      VAFNAS
053200101025     C                   MOVEL     TASCAD        VAFCAD
053300101025     C                   MOVEL     TASLOD        VAFLOD
053400101025     C                   MOVEL     TASPRD        VAFPRD
053500101025     C                   MOVEL     TASNZD        VAFNZD
053600101025     C                   MOVEL     TASVAS        VAFVAS
053700101025     C                   Z-ADD     TASIAS        VAFIAS
053800101025    *C                   MOVEL     *blanks       VAFFPC
053900101025    *C                   MOVEL     *blanks       VAFFVC
054000101025    *C                   Z-ADD     *zeros        VAFVLC
054100101025     C     TASSV3        IFNE      *BLANK
054200101025     C                   EXSR      SRTA7
054300101025     C                   END
054400101025     C                   MOVEL     *BLANKS       VAFRMA
054500101025     C* se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
054600160222     c                   if        tascbo <> 'FO'
054700160208     C                   MOVEL     DTA4A         VAFRMA
054800101025     C                   ENDIF
054900101025     C* se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
055000160222     c                   if        tascbo = 'FO'
055100101025     c                   if        §ta4arma <> *blanks
055200101025     C                   MOVEL     §TA4ARMA      Rma15
055300101025      * verifico se campo numerico escludendo i blanks
055400101025     c                   eval      II = 1
055500101025     c                   dow       II <= %len(%trim(Rma15))
055600101025      *
055700101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
055800101025     c                   eval      WRma15 = (%trim(Wrma15)
055900101025     c                                      + %subst(%trim(Rma15):II:1))
056000101025     c                   endif
056100101025      *
056200101025     c                   eval      II = II + 1
056300101025      *
056400101025     c                   enddo
056500101025     c                   clear                   rma15
056600101025     c                   evalr     rma15 = %trim(Wrma15)
056700101025      *
056800101025     c                   eval      II = 1
056900101025     c                   setoff                                       29
057000101025     c                   dow       II <= %len(%trim(Rma15))
057100101025      *
057200101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
057300101025     c                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
057400101025     c                             VALNUM(1))
057500101025     c                   if        pos = 0
057600101025     c                   seton                                        29
057700101025     c                   leave
057800101025     c                   endif
057900101025     c                   endif
058000101025      *
058100101025     c                   eval      II = II + 1
058200101025      *
058300101025     c                   enddo
058400101025     c                   if        *in29 = *OFF
058500101025     c                   eval      VAFRMA= %editc(tasrmn:'Z')
058600101025     C                   MOVEL     rma15         VAFRMN
058700101025     c                   else
058800101025     C                   EVAL      VAFRMA = §ta4arma
058900101025     C                   EVAL      VAFRMN = tasrmn
059000101025     c                   endif
059100101025      *
059200101025     C                   endif
059300101025     C                   endif
059400101025     C*
059500101025     C* SCRIVO LE VARIE
059600101025     C                   Z-ADD     1             A
059700101025     C     VSV(A)        DOWNE     ' '
059800101025     C                   SELECT
059900101025     C     VAFSV1        WHENEQ    ' '
060000101025     C                   MOVEL     VSV(A)        VAFSV1
060100101025     C                   Z-ADD     VVA(A)        VAFVA1
060200101025     C*
060300101025     C     VAFSV2        WHENEQ    ' '
060400101025     C                   MOVEL     VSV(A)        VAFSV2
060500101025     C                   Z-ADD     VVA(A)        VAFVA2
060600101025     C*
060700101025     C     VAFSV3        WHENEQ    ' '
060800101025     C                   MOVEL     VSV(A)        VAFSV3
060900101025     C                   Z-ADD     VVA(A)        VAFVA3
061000101025     C*
061100101025     C     VAFSV4        WHENEQ    ' '
061200101025     C                   MOVEL     VSV(A)        VAFSV4
061300101025     C                   Z-ADD     VVA(A)        VAFVA4
061400101025     C*
061500101025     C     VAFSV5        WHENEQ    ' '
061600101025     C                   MOVEL     VSV(A)        VAFSV5
061700101025     C                   Z-ADD     VVA(A)        VAFVA5
061800101025     C*
061900101025     C     VAFSV6        WHENEQ    ' '
062000101025     C                   MOVEL     VSV(A)        VAFSV6
062100101025     C                   Z-ADD     VVA(A)        VAFVA6
062200101025     C*
062300101025     C     VAFSV7        WHENEQ    ' '
062400101025     C                   MOVEL     VSV(A)        VAFSV7
062500101025     C                   Z-ADD     VVA(A)        VAFVA7
062600101025     C*
062700101025     C     VAFSV8        WHENEQ    ' '
062800101025     C                   MOVEL     VSV(A)        VAFSV8
062900101025     C                   Z-ADD     VVA(A)        VAFVA8
063000101025     C                   OTHER
063100101025     C                   ADD       VVA(A)        VAFVAX
063200101025     C                   ENDSL
063300101025     C*
063400101025     C                   ADD       1             A
063500101025     C                   ENDDO
063600101025      * Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
063700101025     c                   Clear                   VafRmx
063800101025     C                   MOVEL     VAFKSC        WLB3              3 0
063900101025     c     WLB3          chain     AZORG
064000101025     c                   if            %found(AZORG01L)
064100101025     c                             and ORGfva = *blanks
064200101025     c                   movel     ORGde3        Og143
064300101025     c                   else
064400101025     c                   clear                   Og143
064500101025     c                   endif
064600101025
064700101025     c                   Select
064800101025
064900160209     C* Recupera Riferimento Alfanumerico da ORM
065000101025     C                   When      §4cRfo = 'S'
065100160209     C*
065200160209     C* Reperisco TITA4 tipo record 'M'
065300160209     C                   MOVEL     'M'           UBTA4ITRC
065400160209     C                   EXSR      RTVTA4
065500160209     C*
065600160209     C                   IF        DTA4M <> *zeros
065700101025     C     KORM          CHAIN     FNORM01L                           61
065800160209     C     *IN61         IFEQ      *OFF
065900101025     C                   MOVEL     ORMRFA        VAFRMX
066000101025     C                   ENDIF
066100160209     C                   ENDIF
066200160209     C*
066300101025      * Ritorna al cliente il campo XCO
066400111026     c**                 When      §4cRfo = 'X' and TasXco <> *Blanks
066500101025      * recupero il valore da tornare dalla tabella VUS
066600111026     c**                 Clear                   Tibs02ds
066700111026     c**                 Clear                   dvus
066800111026     c**                 Eval      T02mod = 'C'
066900111026     c**                 Eval      T02sif = knsif
067000111026     c**                 Eval      T02cod = 'VUS'
067100111026     c**                 Eval      T02ke1 = TasXco
067200111026     c**                 Call      'TIBS02R'
067300111026     c**                 Parm                    kpjba
067400111026     c**                 Parm                    Tibs02ds
067500111026     c**                 If        T02err = *blanks
067600111026     c**                 Eval      dvus = T02uni
067700111026     c**                 EndIf
067800111026     c**                 Eval      vafrmx = §vuvaf
067900101025
068000160209     C*
068100101025     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
068200101025     ***C                   When      wlb3 = 190  or wlb3 = 195 or wlb3 = 191
068300101025     c                   When      §OGntw = 'DPD'
068400101025     C                   movel     TASDCM        VAFRMX
068500101025
068600101025     c                   EndSl
068700101025     c
068800101025     c                   ENDSR
068900000124     C*-----------------------------------------------------------*
069000000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
069100000124     C*-----------------------------------------------------------*
069200941122     C     SRTA7         BEGSR
069300990817     C*
069400990823     C     KTA7          SETLL     TITA730C
069500990823     C     KTA7          READE     TITA730C                               59
069600990817DO  1C     *IN59         DOWEQ     *OFF
069700990910     C                   MOVEL     TA7SVN        WSV
069800990910     C                   Z-ADD     TA7VAN        WVA
069900990910     C                   EXSR      CONTR
070000990910     C*
070100990823     C     KTA7          READE     TITA730C                               59
070200990817E   1C                   ENDDO
070300990817     C*
070400941122     C                   ENDSR
070500990910     C*--------------------------------------------------------
070600000124     C* CONTROLLO SE VARIE GIA' PRESENTI
070700000124     C*--------------------------------------------------------
070800990910     C     CONTR         BEGSR
070900000124IF  1C     WSV           IFNE      ' '
071000990910     C*
071100990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
071200990910     C     WSV           ANDNE     §$2IVA
071300990910     C                   ADD       1             A                 3 0
071400990910     C                   MOVEL     WSV           VSV(A)
071500990910     C                   Z-ADD     WVA           VVA(A)
071600990910E   4C                   ENDIF
071700000124E   1C                   ENDIF
071800000124     C                   ENDSR
071900050905
072000170227
072100170227
072200050905      /TITLE Scrittura record TIVAF in file TIVGD00F (file VAS generico download)
072300170227     C     wriVGD        BEGSR
072400140109     C*
072500050905     C                   clear                   tivgd000
072600080404     C                   eval      vgdDTA = %subst(TIVAFDS:1:%size(TIVAFDS))
072700050905     C                   eval      vgdTIP = 'VF'
072800050905     C                   eval      vgdKSU = curKSU
072900080404     C                   eval      vgdTSC = curTSC
073000050905     C                   eval      vgdDAT = datcor
073100140109     C                   eval      vgdPGM = 'TRTC28R'
073200170227     C                   eval      vgdSTO = '?'
073300140109     C*
073400140109     C*************
073500170406     C* Se specificatamente richiesto nel lancio ...
073600140113     C                   if        FILESEP <> 'N'
073700140113     C*
073800140109     C* Verifico la rottura di codice per KSU / DFT / NFT / FIV
073900140109     C                   eval      ds_KSU = curKSU
074000140109     C                   eval      ds_DFT = tasDFT
074100140109     C                   eval      ds_NFT = tasNFT
074200140109     C                   eval      ds_FIV = tasFIV
074300140109     C*
074400140109     C                   z-add     1             jPRG
074500140109     C     DS_PRG        lookup    skKSCU(jPRG)                           55
074600140109     C                   if        %found
074700140109     C                   eval      vgdPRG = skPRG(jPRG)
074800140109     C                   else
074900140109     C                   exsr      calprog
075000140109     C                   eval      vgdPRG = f_tis7prgf
075100140109     C                   add       1             tPRG
075200140109     C                   eval      skKSCU(tPRG) = DS_PRG
075300140109     C                   eval      skPRG(tPRG)  = vgdPRG
075400140109     C                   endif
075500170308     C*
075600170308     C                   else
075700170308     C                   eval      vgdPRG = savPRG
075800140113     C*
075900140113     C                   endif
076000170227     C*************
076100170227     C*
076200170227     C* Salvo tutti i clienti unificanti scritti
076300170227     C                   eval      DS_VGDKSU.ds_KSU = vgdKSU
076400170227     C                   eval      DS_VGDKSU.ds_PRG = vgdPRG
076500170227     C     DS_VGDKSU     lookup    skVGDKSU                               55
076600170227     C                   if        not %found
076700170227     C                   add       1             jKSU
076800170227     C                   eval      skVGDKSU(jKSU) = DS_VGDKSU
076900170227     C                   endif
077000170227     C*
077100170227     C* Scarico ul buffer di output download
077200050905     C                   write     tivgd000
077300050905     C*
077400050905     C                   ENDSR
077500140108
077600140108
077700140108      /TITLE Valorizzazione Progressivo Applicazione
077800140108     C     calprog       begsr
077900140108     C*
078000140108     C                   movel     *blanks       dwlisv            2
078100140108     C                   movel     *all'0'       dwlprg           10
078200140108     C                   z-add     *zeros        wrkprg            8 0
078300140108     C*
078400140108     C                   eval      dwlisv = 'VF'
078500140108     C*
078600170324     C     *start        setll     tis7prgf
078700140108     C                   read(e)   tis7prgf
078800140108     C                   if        not %error
078900140108     C                   eval      dwlprg = f_tis7prgf
079000140108     C*
079100140108     C                   move(p)   dwlprg        wrkprg
079200140108     C                   add       1             wrkprg
079300140108     C                   move(p)   wrkprg        dwlprg
079400140108     C                   movel     dwlisv        dwlprg
079500140108     C*
079600140108     C                   eval      f_tis7prgf = dwlprg
079700140108     C                   update    tis7prg0
079800170324     C                   else
079900170324     C                   eval      f_tis7prgf = 'XX00000000'
080000140108     C                   endif
080100140108     C*
080200140108     C                   endsr
080300140108
080400140108
080500050906     C*--------------------------------------------------------
080600050906     C* OPNFIL    CONTROLLO ESISTENZA DEL MEMBRO (MXXXXXXX    *
080700050906     C*           DOVE XXXXXXX=COD CLI) SE NON C'E AGGIUNGO E *
080800050906     C*           APRO IL FILE                                *
080900050906     C*--------------------------------------------------------
081000050906     C     OPNFIL        BEGSR
081100101025     C                   Z-ADD     50            LUNG             15 5
081200050906     C*
081300050906     C** CONTROLLO SE ESISTE IL MEMBRO
081400050906     C                   SETOFF                                       33        TIVAF00T
081500050906     C                   MOVE      VAR           CMD1
081600050906     C                   MOVEL     *BLANKS       COMMAN           80
081700050906     C                   MOVEA     CMD1(1)       COMMAN
081800050906     C                   CALL      'QCMDEXC'                            33
081900050906     C                   PARM                    COMMAN
082000050906     C                   PARM                    LUNG
082100050906     C** ESEGUE ADDPFM SOLO SE NON ESISTE MEMBRO
082200050906     C   33              DO
082300050906     C                   MOVE      VAR           CMD2
082400050906     C                   MOVEL     *BLANKS       COMMAN
082500050906     C                   MOVEA     CMD2(1)       COMMAN
082600050906     C                   CALL      'QCMDEXC'
082700050906     C                   PARM                    COMMAN
082800050906     C                   PARM                    LUNG
082900050906     C                   END
083000050906     C** FACCIO OVRDBF DEL FILE E LO APRO
083100050906     C                   MOVE      VAR           CMD3
083200050906     C                   MOVEL     *BLANKS       COMMAN
083300050906     C                   MOVEA     CMD3(1)       COMMAN
083400050906     C                   CALL      'QCMDEXC'
083500050906     C                   PARM                    COMMAN
083600050906     C                   PARM                    LUNG
083700050906     C**
083800060203     C                   IF        not %open(TIVAF00T)
083900050906     C                   OPEN      TIVAF00T
084000060203     C                   ENDIF
084100060203     C*
084200050906     C                   ENDSR
084300050905?    C*-------------------------------------------------
084400050906?    C* esegue - esegue comandi "CL"
084500050905?    C*-------------------------------------------------
084600050905     c     esegue        begsr
084700050905     c                   call      'QCMDEXC'                            60
084800050905     c                   parm                    comman
084900050905     c                   parm                    lung
085000050905     c                   endsr
085100101025     C*----------------------------------------------------*
085200101025     c* Scrittuta TIVAF per embro revisori
085300101025     C*----------------------------------------------------*
085400101025     c     ElaREV        BEGSR
085500101026     c
085600101026     C                   EXSR      DEFCAM
085700101026
085800101025     c                   eval      $Fine='0'
085900101025     c                   clear                   ksc
086000101025     c                   eval      VAR='REVISORI)'
086100101025     c                   exsr      opnfil
086200101025     c
086300101025     c* Lettura del primo codice cliente
086400101025    1c                   dow       $Fine='0'
086500101025     c     ksc           setgt     titas31c
086600101025     c                   read      titas31c
086700101025     c
086800101025    2c                   if        %eof(titas31c)
086900101025     c                   eval      $Fine='1'
087000101025   x2c                   else
087100101025     c
087200101025     c                   eval      ksc=tasksc
087300101025     c     ktas          setll     titas31c
087400101025     c     ktas          reade     titas31c
087500101025     c
087600101025    3c                   if        not %eof(titas31c)
087700101025     c* Escludo se non si tratta di cliente codificato
087800101025     c                   move      tasksc        w0040             4 0
087900101025
088000101025    4c                   if        w0040<>8888 and tasksc<>9999
088100101025
088200101025    5c                   dow       not %eof(titas31c)
088300101025     c
088400101026     c                   if        tasfiv>990
088500101025     c                   exsr      ScriviDati
088600101025     c
088700101025     c                   write     tivaf000
088800101026     c                   endif
088900101025     c
089000101025     c     ktas          reade     titas31c
089100101025    5c                   enddo
089200101025     c
089300101025    4c                   endif
089400101025     c
089500101025    3c                   endif
089600101025    2c                   endif
089700101025    1c                   enddo
089800101025     c*
089900101025     c                   if        %open(tivaf00t)
090000101025     c                   close     tivaf00t
090100101025     c                   endif
090200101025     c
090300101025     c                   ENDSR
090400990817     C*--------------------------------------------------------
090500151016
090600151016
090700151016
090800151016     C     RTVAR5FAT     BEGSR
090900151016     C*
091000151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
091100151028     C                   SETOFF                                       7172
091200151016     C                   EVAL      kAr5Trd = 'FAT'
091300151016     C                   CLEAR                   DAR5FAT
091400151016     C     kFiar5        CHAIN     FIAR531C
091500151016     C                   IF        %found(FIAR531C)
091600151016     C                   MOVEL     AR5UNI        DAR5FAT
091700170130     C                   IF        (§AR5FPTAS = 'T' OR §AR5FPTAS = 'D') AND
091800170130     C                             §AR5PKTAS > *zeros
091900151028     C                   SETON                                        71
092000151016     C                   EVAL      VAFPKC = §AR5PKTAS
092100151016     C                   ENDIF
092200170130     C                   IF        (§AR5FVTAS = 'T' OR §AR5FVTAS = 'D') AND
092300170130     C                             §AR5VLTAS > *zeros
092400151028     C                   SETON                                        72
092500160505     C                   EVAL      VAFVLC = §AR5VLTAS
092600151016     C                   ENDIF
092700151016     C                   ENDIF
092800151016     C*
092900151016     C                   ENDSR
093000160208
093100160208
093200160208
093300160209     C     RTVTA4        BEGSR
093400160208     C*
093500160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
093600160208     C                   CLEAR                   DTA4A
093700160209     C                   CLEAR                   DTA4M
093800160208     C*
093900160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
094000160208     C                   CALL      'UBTA400R'
094100160208     C                   PARM      *blanks       UBTA4IOPZ
094200160208     C                   PARM      *blanks       UBTA4ITLA
094300160208     C                   PARM      tasAAS        UBTA4IAAS
094400160208     C                   PARM      tasLNP        UBTA4ILNP
094500160208     C                   PARM      tasNRS        UBTA4INRS
094600160208     C                   PARM      tasNSP        UBTA4INSP
094700160209     C                   PARM                    UBTA4ITRC
094800160208     C                   PARM                    UBTA4OERR
094900160208     C                   PARM                    UBTA4ODS
095000160208     C                   PARM                    UBTA4OLEN
095100160208     C                   PARM                    UBTA4ODATI
095200160208     C*
095300160208     C                   IF        UBTA4OERR = *zeros
095400160208     C                   SELECT
095500160208     C* Gestione output tipo record 'A'
095600160208     C                   WHEN      UBTA4ODS = 'DTA4A'
095700160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
095800160209     C* Gestione output tipo record 'M'
095900160209     C                   WHEN      UBTA4ODS = 'DTA4M'
096000160209     C                   EVAL      DTA4M = %subst(UBTA4ODATI:1:UBTA4OLEN)
096100160208     C                   ENDSL
096200160208     C*
096300160208     C                   ENDIF
096400160208     C*
096500160208     C                   ENDSR
096600151016     C*--------------------------------------------------------
096700170227
096800170227
096900170227
097000170227     C*------------------------------------------------------------------------*
097100170227     C* EXEERR - Routine di esecuzione azioni su Errore
097200170227     C*------------------------------------------------------------------------*
097300170227     C     EXEERR        BEGSR
097400170227     C*
097500170227     C                   dump(A)
097600170227     C                   rolbk
097700170227     C                   seton                                        lr
097800170227     C                   return
097900170227     C*
098000170227     C                   ENDSR
098100170227     C*------------------------------------------------------------------------*
098200991110**         CMD1
098300000124CHKOBJ OBJ(TIVAF00T) OBJTYPE(*FILE) MBR(M0000000)
098400910830**         CMD2
098500000124ADDPFM FILE(TIVAF00T)               MBR(M0000000)
098600910830**         CMD3
098700000124OVRDBF FILE(TIVAF00T)               MBR(M0000000)
098800000721**         CMD5
098900000724OVRDBF FILE(TABELXXF) TOFILE(*LIBL/TABEL00F)
099000991110**         CMD
099100991110DLTOVR FILE(*ALL)
099200030515**         VALNUM
0993000305150123456789
