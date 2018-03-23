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
002600170227     F                                     USROPN
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
005300170227
005400890921     D KPJBA         E DS
005500990817     D DS4C          E DS
005600060203     D DS4C_W        E DS                  EXTNAME(DS4C) PREFIX(W_)
005700050906     D DSQT1         E DS
005800111018     D**!!!DS4r          E DS
005900050906     D DS$2          E DS
006000030516     D DTA4A         E DS
006100160209     D DTA4M         E DS
006200050905     D tivafds       e ds                  extname(tivaf00t)
006300170227     D TIS7VASDS     E DS
006400170227
006500050906     D PARAM           DS
006600941122     D  DAL                    1      8  0
006700941122     D  AL                     9     16  0
006800050906     D  CLIU                  17     23  0
006900101025     D  REVISORI              24     24
007000140108     D  NFT                   25     30  0
007100140113     D  FILESEP               31     31
007200140319     D  FLGUNI                32     32
007300140319     D  CLIVAS                33     40
007400010402     D*------------------
007500030917      * DS FIAR5
007600030917     d dAr5Bnb       e ds
007700151016     D DAR5FAT       E DS
007800050126
007900050126     d Tibs02ds      e ds
008000111026     d*******dvus          e ds
008100051219
008200051219     d Og143         e ds                  inz
008300060127
008400030917      *
008500021212     D Wtara           s                   like(§QTTPC)
008600021212     D Wntara          s                   like(TASPKF)
008700030917
008800030917     d kAr5Trd         s                   Like(Ar5Trd)
008900030515
009000030515     D Rma15           s             15
009100030515     D WRma15          s             15
009200030515     D II              s              2  0
009300030515     D Pos             s              2  0
009400111026     D Var             s              9
009500050126     d wxco            s              1    inz(*Off)
009600101025     d $Fine           s              1    inz('0')
009700160208
009800160208     D*--------------------
009900160208     D* DEFINIZIONI A PROCEDURE ESTERNE
010000160208     D*--------------------
010100160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
010200160208
010300010402
010400101025     C*------------------------------------------------------------------
010500101025     C     *ENTRY        PLIST
010600101025     C                   PARM                    KPJBA
010700140319     C                   MOVEL     KPJBU         PARAM
010800140319     C*
010900140319     C* Verifico subito se richiesto un particolare cliente VAS (per output)
011000140319     C                   setoff                                       45
011100140319     C                   if        CLIVAS = *all'0' and CLIU <> *zeros
011200140319     C                   move      CLIU          CLIVAS
011300140319     C                   endif
011400140319     C                   if        CLIVAS <> *all'0'
011500140319     C                   seton                                        45
011600140319     C                   endif
011700140319     c*
011800101025     c* Verifico se elaborazione per REVISORI o per scarico a clienti
011900101025     c                   if        revisori='S'
012000101025     c                   exsr      ElaREV
012100101025     c                   else
012200101025     c                   exsr      ElaCLI
012300101025     c                   endif
012400160208     C*
012500160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
012600160208     C                   CALL      'UBTA400R'
012700160208     C                   PARM                    UBTA4IOPZ
012800160208     C                   PARM      'C'           UBTA4ITLA
012900160208     C                   PARM                    UBTA4IAAS
013000160208     C                   PARM                    UBTA4ILNP
013100160208     C                   PARM                    UBTA4INRS
013200160208     C                   PARM                    UBTA4INSP
013300160208     C                   PARM                    UBTA4ITRC
013400160208     C                   PARM                    UBTA4OERR
013500160208     C                   PARM                    UBTA4ODS
013600160208     C                   PARM                    UBTA4OLEN
013700160208     C                   PARM                    UBTA4ODATI
013800160208     C*
013900921023     C                   SETON                                        LR
014000910830     C*----------------------------------------------------*
014100101025     c* Scrittuta TIVGD per i clienti che lo hanno richiesto
014200101025     C*----------------------------------------------------*
014300101025     c     ElaCLI        BEGSR
014400101025     c
014500101025     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VF'
014600170227     C                   CLEAR                   TIS7VASDS
014700170227     C                   EVAL      i§7VASOPZ = 'SEM'
014800170227     C                   EVAL      i§7VASKSU = *blanks
014900170227     C                   EVAL      i§7VASTIP = 'VF'
015000170227     C                   CALL(e)   'TIS7VASR1'
015100170227     C                   PARM                    TIS7VASDS
015200170227     C*
015300170227     C* Verifico esito chiamata
015400170227     C                   if        not %error AND
015500170227     C                             o§7VASOK = *on
015600101025     C*
015700101025     C                   EXSR      DEFCAM
015800101025      *
015900101025     C     KTB           SETLL     TABEL
016000140326     C                   SETOFF                                       58
016100140326    1C                   IF        %equal(TABEL00F)
016200140326     C     KTB           READE     TABEL
016300140326    2C                   DOW       not %eof(TABEL00F)
016400101025    3C     TBLFLG        IFNE      '*'
016500101025     C                   MOVEL     TBLUNI        DS4C
016600101025    4C     §4CFSD        IFNE      *BLANK                                        CLI DA SCAR
016700101025     C                   MOVE      §4CFKS        KSCU              7 0
016800101025     C                   MOVEL     TBLKEY        KSC               7 0
016900101025     C*
017000140326    5C                   if        (CLIU>*zeros AND FLGUNI='S' AND CLIU=KSCU) OR
017100140326     C                             (CLIU>*zeros AND FLGUNI='N')               OR
017200140319     C                              CLIU=*zeros
017300140326     C                   if         CLIU>*zeros AND FLGUNI='N'
017400140326     C                   eval      KSC = CLIU
017500140326     C                   seton                                        58
017600140326     C                   endif
017700140326     C*
017800101025     c                   Eval      wxco = *Off
017900101025     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
018000140326   e5C                   endif
018100140326     C*
018200140326     C                   if        *in58
018300140326     C                   leave
018400140326     C                   endif
018500140326     C*
018600140326   e4C                   ENDIF
018700140326   e3C                   ENDIF
018800140326     C     KTB           READE     TABEL
018900140326   e2C                   ENDDO
019000140326   e1C                   ENDIF
019100101025     C*
019200170227     C* CANCELLO LE OVR ESEGUITE
019300101025     C                   MOVEL     *BLANKS       COMMAN           80
019400101025     C                   Z-ADD     17            LUNG
019500101025     C                   MOVEA     CMD(1)        COMMAN
019600101025     C                   CALL      'QCMDEXC'                            21
019700101025     C                   PARM                    COMMAN
019800101025     C                   PARM                    LUNG
019900101025     C*
020000170227     C                   else
020100170227     C                   exsr      EXEERR
020200101025     C                   endif
020300170227     C*
020400170227     C* Sancisco le transazioni logiche per il download del VF
020500170227     C                   z-add     1             jKSU
020600170227     C                   dow       jKSU < %elem(skVGDKSU)
020700170227     C*
020800170227     C* Finchè elementi valorizzati
020900170227     C                   if        skVGDKSU(jKSU) = *blanks
021000170227     C                   leave
021100170227     C                   else
021200170227     C                   eval      DS_VGDKSU = skVGDKSU(jKSU)
021300170227     C*
021400170227     C* Finalizzo la transazione
021500170227     C                   EVAL      i§7VASOPZ  = 'RLS'
021600170227     C                   EVAL      i§7VASTIP  = 'VF'
021700170227     C                   EVAL      i§7VASKSU  = DS_VGDKSU.ds_KSU
021800170227     C                   EVAL      i§7VASPRG  = DS_VGDKSU.ds_PRG
021900170227     C                   EVAL      i§7VASTSC  = 'WW'
022000170227     C                   EVAL      i§7VASSTO  = '?'
022100170227     C                   EVAL      i§7VASSTTO = 'L'
022200170227     C                   CALL(e)   'TIS7VASR1'
022300170227     C                   PARM                    TIS7VASDS
022400170227     C                   endif
022500170227     C*
022600170227     C                   add       1             jKSU
022700170227     C                   enddo
022800101025     C*
022900170227     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
023000170227     C                   CLEAR                   TIS7VASDS
023100170227     C                   EVAL      i§7VASOPZ = 'DEL'
023200170227     C                   EVAL      i§7VASKSU = *blanks
023300170227     C                   EVAL      i§7VASTIP = 'VF'
023400170227     C                   CALL(e)   'TIS7VASR1'
023500170227     C                   PARM                    TIS7VASDS
023600101025     C*
023700101025     c                   ENDSR
023800101025     C*----------------------------------------------------*
023900101025     c* operazioni  iniziali
024000101025     C*----------------------------------------------------*
024100910830     C     DEFCAM        BEGSR
024200050905      *
024300050905      * calcola la data corrente
024400160209     c                   z-add     *zeros        datcor            8 0          *d.corrente aa/m/g
024500160209     C                   eval      datcor = %dec(%date() : *ISO)
024600000721      *
024700050905      * Overrida tabel in tabelxx per gestione multimembro (stesso dato a 2 clienti)
024800000721     c                   movel     *blanks       comman
024900000721     c                   movea     cmd5          comman
025000000721     c                   z-add     44            lung
025100050906     c                   exsr      esegue
025200000721     c                   open      tabelxxf
025300941122     C* TABEL
025400921023     C     KTB           KLIST
025500910830     C                   KFLD                    TBLKUT
025600910902     C                   KFLD                    TBLCOD
025700030917      * FIAR
025800030917     c     kFiar5        Klist
025900030917     c                   Kfld                    TasAas
026000030917     c                   Kfld                    TasLnp
026100030917     c                   Kfld                    TasNrs
026200030917     c                   Kfld                    TasNsp
026300030917     c                   Kfld                    kAr5Trd
026400990817     C* TITAS
026500050905     C     KTAS          KLIST
026600050905     C                   KFLD                    KSC
026700050905     C                   KFLD                    DAL
026800990817     C* TITA7
026900050905     C     KTA7          KLIST
027000941122     C                   KFLD                    TASAAS
027100941122     C                   KFLD                    TASLNP
027200941122     C                   KFLD                    TASNRS
027300941122     C                   KFLD                    TASNSP
027400050905     C                   KFLD                    TASTBL
027500990910     C     *LIKE         DEFINE    TASVA1        WVA
027600990910     C     *LIKE         DEFINE    TASSV1        WSV
027700010402     C*
027800050905     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
027900050905     C     KORM          KLIST
028000160209     C                   KFLD                    §TA4MPOE
028100160209     C                   KFLD                    §TA4MNSR
028200160209     C                   KFLD                    §TA4MNOR
028300160209     C                   KFLD                    §TA4MNRV
028400990817     C*
028500990817     C* LEGGE IL RECORD SIGLE VARIE
028600990817     C     *LIKE         DEFINE    TBLKUT        KTBKUT
028700990817     C     *LIKE         DEFINE    TBLCOD        KTBCOD
028800990817     C     *LIKE         DEFINE    TBLKEY        KTBKEY
028900000721     C     *LIKE         DEFINE    TBLCOD        KTBCODx
029000000721     C     *LIKE         DEFINE    TBLKEY        KTBKEYx
029100000721     C     KEYTABx       KLIST
029200000721     C                   KFLD                    KTBKUT
029300000721     C                   KFLD                    KTBCODx
029400000721     C                   KFLD                    KTBKEYx
029500990817     C     KEYTAB        KLIST
029600990817     C                   KFLD                    KTBKUT
029700990817     C                   KFLD                    KTBCOD
029800990817     C                   KFLD                    KTBKEY
029900021212      *
030000021212      ** Aggancio tabella $2 - Varie forzate a programma
030100990817     C                   Z-ADD     1             KTBKUT
030200990817     C                   MOVEL     '$2'          KTBCOD
030300990817     C                   MOVEL     1             KTBKEY
030400990817     C     KEYTAB        CHAIN     TABEL00F                           99
030500990817IF  1C     *IN99         IFEQ      *OFF
030600990817     C     TBLFLG        ANDEQ     *BLANKS
030700990817     C                   MOVEL     TBLUNI        DS$2
030800990817E   1C                   ENDIF
030900021212      *
031000050905      ** Aggancio tabella QT - campi standard tassazione
031100021212     C                   MOVEL     'QT'          KTBCOD
031200021212     C                   MOVEL     1             KTBKEY
031300021212     C     KEYTAB        CHAIN     TABEL00F                           99
031400021212IF  1C     *IN99         IFEQ      *OFF
031500021212     C     TBLFLG        ANDEQ     *BLANKS
031600021212     C                   MOVEL     TBLUNI        DSQT1
031700021212E   1C                   ENDIF
031800990817     C*
031900021212      ** Imposto key per tabella clienti con ritorno info fattura
032000990817     C                   MOVE      1             TBLKUT
032100990817     C                   MOVEL     '4C'          TBLCOD
032200990817     C                   MOVEL     *BLANKS       TBLKEY
032300990817     C*
032400910830     C                   ENDSR
032500050906     C*--------------------------------------------------------
032600050905     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
032700000124     C*--------------------------------------------------------
032800921023     C     WRTFIL        BEGSR
032900990817     C     KTAS          SETLL     TITAS31C
033000921023     C                   DO        *HIVAL
033100990817     C     KSC           READE     TITAS31C                               70
033200000124     C  N70              DO
033300000124     C     TASNFT        IFGT      *ZERO                                        N.FATT.
033400921023     C     TASDFT        ANDLE     AL
033500140108
033600140108     C*
033700140108     C* Se richiesto filtro per numero fattura => verifico
033800140108     C                   IF        NFT <> *zeros AND
033900140108     C                             NFT <> tasNFT
034000140108     C* Skippo spedizione corrente
034100140108     C                   ELSE
034200050126
034300050126      * per prima cosa controllo se devo scrivere un membro apposta x l'accorpamento bolle
034400111026     c****               If        Tasxco <> *Blanks
034500050906      * se in tabella è prevista la creazione di un nuovo membro
034600111026     c****               If        §4cRfo = 'X'    and
034700050906      * e non era già stato aperto il membro x questo cliente
034800111026     c****                         wxco = *Off
034900050906      * chiudo il vecchio membro e apro il nuovo
035000111026     c****               Move      ') '          var               9
035100111026     c****               Movel     §4cMbx        var
035200111026     c****               ExSr      opnfil
035300111026     c****               Eval      wxco = *On
035400111026     c****               EndIf
035500111026     c****               EndIf
035600101025     c*
035700101025     c                   exsr      SCriviDati
035800101025     c
035900050906     C*
036000050906     C* Verifico se richiesta scrittura membro "particolare" x gestone XCO
036100050906     C                   if        wxco = *on
036200050906     C                   write     tivaf000
036300050906     C                   else
036400060203     C*
036500060203     C* Scrivo il file TIVGD00F (file VAS Generico Download)
036600140319     C                   movel     *all'0'       curKSU
036700140319     C                   if        not *in45
036800060203     C                   move      KSCU          curKSU
036900140319     C                   else
037000140319     C                   move      CLIVAS        curKSU
037100140319     C                   endif
037200060203     C                   eval      curTSC = §4CFSD
037300060203     C*
037400060203     C* Verifica il tipo scarico attribuito al cliente su cui unificare i dati del cliente corrente
037500060203     C                   clear                   DS4C_W
037600060203     C                   move      '4C'          ktbcodx
037700060203     C                   movel     KSCU          ktbkeyx
037800060203     C     keytabx       chain     tabelxxf                           89
037900060203     C                   if        %found(tabelxxf)
038000060203     C                   movel     xtbluni       DS4C_W
038100060203     C                   eval      curTSC = W_§4CFSD
038200060203     C                   endif
038300151016     C*
038400151016     C* Eseguo eventuali "forzature"
038500151016     C                   EXSR      RTVAR5FAT
038600060203     C*
038700060203     C* Quindi scarico il buffer nel file d out generico download
038800050905     C                   exsr      WRIVGD
038900050906     C                   endif
039000050905     C*
039100050905     C* Verifica se richiesta gestione multimembro (stesso dato a 2 clienti)
039200111018     c**!!!              move      '4R'          ktbcodx
039300111018     c**!!!              movel     tblkey        ktbkeyx
039400111018     c**!!!keytabx       chain     tabelxxf                           89
039500111018     c**!!!              if        %found(tabelxxf)
039600111018     c**!!!              movel     xtbluni       ds4r
039700111018     C**!!!              movel     *all'0'       curKSU
039800111018     C**!!!              move      §4RCKS        curKSU
039900111018     C**!!!              eval      curTSC = §4RCSD
040000111018     C**!!!              exsr      WRIVGD
040100111018     c**!!!              endif
040200050905     C*
040300140108     C                   ENDIF
040400140108     C*
040500940910     C                   END
040600921023     C                   END
040700000124     C  N70              END
040800000124     C                   ENDSR
040900101025     C*-----------------------------------------------------------*
041000101025     C* Scrivo dati nel file TIVAF
041100101025     C*-----------------------------------------------------------*
041200101025     c     Scrividati    BEGSR
041300101025     C*
041400101025     C* Inizializzo il buffer d output
041500101025     C                   clear                   TIVAFDS
041600160208     C*
041700160209     C* Reperisco TITA4 tipo record 'A'
041800160209     C                   MOVEL     'A'           UBTA4ITRC
041900160209     C                   EXSR      RTVTA4
042000101025     C*
042100101025     C                   MOVEL     TASTBL        VAFTBL
042200101025     C                   Z-ADD     TASLNP        VAFLNP
042300101025     C                   Z-ADD     TASMGS        VAFMGS
042400101025     C                   Z-ADD     TASAAS        VAFAAS
042500101025     C                   Z-ADD     TASNRS        VAFNRS
042600101025     C                   Z-ADD     TASNSP        VAFNSP
042700101025     C                   Z-ADD     TASKSC        VAFKSC
042800101025     C                   MOVEL     TASFIN        VAFFIN
042900101025     C                   Z-ADD     TASLNA        VAFLNA
043000101025      * se particolità varia = 'O' recupero da FIAR5 il numero colli originali
043100101025      *
043200101025     c                   If        %Subst(TasGva:1:1) = 'O'
043300101025
043400101025     c                   eval      kAr5Trd  = 'BNB'
043500101025     c                   clear                   dAr5Bnb
043600101025     c     kFiar5        Chain     Fiar531c
043700101025     c                   If        %Found(Fiar531c)
043800101025     c                   Movel     Ar5Uni        dAr5Bnb
043900101025     c                   EndIf
044000101025      * Colli originali
044100101025     c                   Eval      VafNcl = §Ar5bcor
044200101025
044300101025     c                   else
044400101025      * Colli TITAS
044500101025     C                   Z-ADD     TASNCL        VAFNCL
044600101025     c                   endif
044700101025
044800101025     C                   Z-ADD     TASVLF        VAFVLF
044900101025     C                   Z-ADD     TASPOR        VAFPOR
045000101025      * PESO
045100101025     C                   Z-ADD     TASPKF        VAFPKB
045200101025     C                   IF        TASFPF = 'V'
045300150716     C*
045400150716     C                   IF        TASDFT <= §QTDST
045500150716     C     §QTTCO        MULT      TASNCP        WTARA
045600150716     C     TASPKC        SUB(h)    WTARA         WNTARA
045700150716     C                   Z-ADD     WNTARA        VAFPKC
045800150716     C                   ELSE
045900101025     C     §QTTPC        MULT      TASNCP        WTARA
046000101025     C     TASPKC        SUB(h)    WTARA         WNTARA
046100150716     C                   Z-ADD     WNTARA        VAFPKC
046200150716     C                   ENDIF
046300150716     C*
046400101025     C                   ELSE
046500151020     C                   IF        TASFPF <> 'D'
046600150716     C                   Z-ADD     *zeros        VAFPKC
046700151020     C                   ENDIF
046800101025     C                   ENDIF
046900150716     C*
047000101025     C* AZZERO CAMPI NON SEMPRE PRESENTI
047100101025     C                   MOVE      *ZERO         WSV
047200101025     C                   MOVE      *ZERO         WVA
047300101025     C                   MOVEA     *BLANKS       VSV
047400101025     C                   MOVEA     *ZERO         VVA
047500101025     C                   CLEAR                   A
047600101025     C                   CLEAR                   VAFSV1
047700101025     C                   CLEAR                   VAFVA1
047800101025     C                   CLEAR                   VAFSV2
047900101025     C                   CLEAR                   VAFVA2
048000101025     C                   CLEAR                   VAFSV3
048100101025     C                   CLEAR                   VAFVA3
048200101025     C                   CLEAR                   VAFSV4
048300101025     C                   CLEAR                   VAFVA4
048400101025     C                   CLEAR                   VAFSV5
048500101025     C                   CLEAR                   VAFVA5
048600101025     C                   CLEAR                   VAFSV6
048700101025     C                   CLEAR                   VAFVA6
048800101025     C                   CLEAR                   VAFSV7
048900101025     C                   CLEAR                   VAFVA7
049000101025     C                   CLEAR                   VAFSV8
049100101025     C                   CLEAR                   VAFVA8
049200101025     C                   CLEAR                   VAFVAX
049300101025     C*
049400101025     C                   MOVEL     TASSV1        WSV
049500101025     C                   Z-ADD     TASVA1        WVA
049600101025     C                   EXSR      CONTR
049700101025     C                   MOVEL     TASSV2        WSV
049800101025     C                   Z-ADD     TASVA2        WVA
049900101025     C                   EXSR      CONTR
050000101025     C                   MOVEL     TASSV3        WSV
050100101025     C                   Z-ADD     TASVA3        WVA
050200101025     C                   EXSR      CONTR
050300101025     C*
050400101025     C                   Z-ADD     TASDFT        VAFDFT
050500101025     C                   Z-ADD     TASNFT        VAFNFT
050600101025      *
050700101025     c                   if        tascbo <> 'FO'
050800101025     C                   Z-ADD     TASRMN        VAFRMN
050900101025     c                   endif
051000101025      *
051100101025     C                   MOVEL     TASFTC        VAFFTC
051200101025     C                   MOVEL     TASTSP        VAFTSP
051300101025     C                   MOVEL     TASCTS        VAFCTS
051400101025     C                   MOVEL     TASFAP        VAFFAP
051500101025     C                   Z-ADD     TASCTR        VAFCTR
051600101025     C                   Z-ADD     TASQFT        VAFQFT
051700101025     C                   MOVEL     TASTC2        VAFTC2
051800101025     C                   Z-ADD     TASIMV        VAFIMV
051900101025     C                   Z-ADD     TASFIV        VAFFIV
052000101025     C                   MOVEL     TASDIV        VAFDIV
052100101025     C                   Z-ADD     TASDRT        VAFDRT
052200160208     C                   MOVEL     §TA4ANAS      VAFNAS
052300101025     C                   MOVEL     TASCAD        VAFCAD
052400101025     C                   MOVEL     TASLOD        VAFLOD
052500101025     C                   MOVEL     TASPRD        VAFPRD
052600101025     C                   MOVEL     TASNZD        VAFNZD
052700101025     C                   MOVEL     TASVAS        VAFVAS
052800101025     C                   Z-ADD     TASIAS        VAFIAS
052900101025    *C                   MOVEL     *blanks       VAFFPC
053000101025    *C                   MOVEL     *blanks       VAFFVC
053100101025    *C                   Z-ADD     *zeros        VAFVLC
053200101025     C     TASSV3        IFNE      *BLANK
053300101025     C                   EXSR      SRTA7
053400101025     C                   END
053500101025     C                   MOVEL     *BLANKS       VAFRMA
053600101025     C* se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
053700160222     c                   if        tascbo <> 'FO'
053800160208     C                   MOVEL     DTA4A         VAFRMA
053900101025     C                   ENDIF
054000101025     C* se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
054100160222     c                   if        tascbo = 'FO'
054200101025     c                   if        §ta4arma <> *blanks
054300101025     C                   MOVEL     §TA4ARMA      Rma15
054400101025      * verifico se campo numerico escludendo i blanks
054500101025     c                   eval      II = 1
054600101025     c                   dow       II <= %len(%trim(Rma15))
054700101025      *
054800101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
054900101025     c                   eval      WRma15 = (%trim(Wrma15)
055000101025     c                                      + %subst(%trim(Rma15):II:1))
055100101025     c                   endif
055200101025      *
055300101025     c                   eval      II = II + 1
055400101025      *
055500101025     c                   enddo
055600101025     c                   clear                   rma15
055700101025     c                   evalr     rma15 = %trim(Wrma15)
055800101025      *
055900101025     c                   eval      II = 1
056000101025     c                   setoff                                       29
056100101025     c                   dow       II <= %len(%trim(Rma15))
056200101025      *
056300101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
056400101025     c                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
056500101025     c                             VALNUM(1))
056600101025     c                   if        pos = 0
056700101025     c                   seton                                        29
056800101025     c                   leave
056900101025     c                   endif
057000101025     c                   endif
057100101025      *
057200101025     c                   eval      II = II + 1
057300101025      *
057400101025     c                   enddo
057500101025     c                   if        *in29 = *OFF
057600101025     c                   eval      VAFRMA= %editc(tasrmn:'Z')
057700101025     C                   MOVEL     rma15         VAFRMN
057800101025     c                   else
057900101025     C                   EVAL      VAFRMA = §ta4arma
058000101025     C                   EVAL      VAFRMN = tasrmn
058100101025     c                   endif
058200101025      *
058300101025     C                   endif
058400101025     C                   endif
058500101025     C*
058600101025     C* SCRIVO LE VARIE
058700101025     C                   Z-ADD     1             A
058800101025     C     VSV(A)        DOWNE     ' '
058900101025     C                   SELECT
059000101025     C     VAFSV1        WHENEQ    ' '
059100101025     C                   MOVEL     VSV(A)        VAFSV1
059200101025     C                   Z-ADD     VVA(A)        VAFVA1
059300101025     C*
059400101025     C     VAFSV2        WHENEQ    ' '
059500101025     C                   MOVEL     VSV(A)        VAFSV2
059600101025     C                   Z-ADD     VVA(A)        VAFVA2
059700101025     C*
059800101025     C     VAFSV3        WHENEQ    ' '
059900101025     C                   MOVEL     VSV(A)        VAFSV3
060000101025     C                   Z-ADD     VVA(A)        VAFVA3
060100101025     C*
060200101025     C     VAFSV4        WHENEQ    ' '
060300101025     C                   MOVEL     VSV(A)        VAFSV4
060400101025     C                   Z-ADD     VVA(A)        VAFVA4
060500101025     C*
060600101025     C     VAFSV5        WHENEQ    ' '
060700101025     C                   MOVEL     VSV(A)        VAFSV5
060800101025     C                   Z-ADD     VVA(A)        VAFVA5
060900101025     C*
061000101025     C     VAFSV6        WHENEQ    ' '
061100101025     C                   MOVEL     VSV(A)        VAFSV6
061200101025     C                   Z-ADD     VVA(A)        VAFVA6
061300101025     C*
061400101025     C     VAFSV7        WHENEQ    ' '
061500101025     C                   MOVEL     VSV(A)        VAFSV7
061600101025     C                   Z-ADD     VVA(A)        VAFVA7
061700101025     C*
061800101025     C     VAFSV8        WHENEQ    ' '
061900101025     C                   MOVEL     VSV(A)        VAFSV8
062000101025     C                   Z-ADD     VVA(A)        VAFVA8
062100101025     C                   OTHER
062200101025     C                   ADD       VVA(A)        VAFVAX
062300101025     C                   ENDSL
062400101025     C*
062500101025     C                   ADD       1             A
062600101025     C                   ENDDO
062700101025      * Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
062800101025     c                   Clear                   VafRmx
062900101025     C                   MOVEL     VAFKSC        WLB3              3 0
063000101025     c     WLB3          chain     AZORG
063100101025     c                   if            %found(AZORG01L)
063200101025     c                             and ORGfva = *blanks
063300101025     c                   movel     ORGde3        Og143
063400101025     c                   else
063500101025     c                   clear                   Og143
063600101025     c                   endif
063700101025
063800101025     c                   Select
063900101025
064000160209     C* Recupera Riferimento Alfanumerico da ORM
064100101025     C                   When      §4cRfo = 'S'
064200160209     C*
064300160209     C* Reperisco TITA4 tipo record 'M'
064400160209     C                   MOVEL     'M'           UBTA4ITRC
064500160209     C                   EXSR      RTVTA4
064600160209     C*
064700160209     C                   IF        DTA4M <> *zeros
064800101025     C     KORM          CHAIN     FNORM01L                           61
064900160209     C     *IN61         IFEQ      *OFF
065000101025     C                   MOVEL     ORMRFA        VAFRMX
065100101025     C                   ENDIF
065200160209     C                   ENDIF
065300160209     C*
065400101025      * Ritorna al cliente il campo XCO
065500111026     c**                 When      §4cRfo = 'X' and TasXco <> *Blanks
065600101025      * recupero il valore da tornare dalla tabella VUS
065700111026     c**                 Clear                   Tibs02ds
065800111026     c**                 Clear                   dvus
065900111026     c**                 Eval      T02mod = 'C'
066000111026     c**                 Eval      T02sif = knsif
066100111026     c**                 Eval      T02cod = 'VUS'
066200111026     c**                 Eval      T02ke1 = TasXco
066300111026     c**                 Call      'TIBS02R'
066400111026     c**                 Parm                    kpjba
066500111026     c**                 Parm                    Tibs02ds
066600111026     c**                 If        T02err = *blanks
066700111026     c**                 Eval      dvus = T02uni
066800111026     c**                 EndIf
066900111026     c**                 Eval      vafrmx = §vuvaf
067000101025
067100160209     C*
067200101025     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
067300101025     ***C                   When      wlb3 = 190  or wlb3 = 195 or wlb3 = 191
067400101025     c                   When      §OGntw = 'DPD'
067500101025     C                   movel     TASDCM        VAFRMX
067600101025
067700101025     c                   EndSl
067800101025     c
067900101025     c                   ENDSR
068000000124     C*-----------------------------------------------------------*
068100000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
068200000124     C*-----------------------------------------------------------*
068300941122     C     SRTA7         BEGSR
068400990817     C*
068500990823     C     KTA7          SETLL     TITA730C
068600990823     C     KTA7          READE     TITA730C                               59
068700990817DO  1C     *IN59         DOWEQ     *OFF
068800990910     C                   MOVEL     TA7SVN        WSV
068900990910     C                   Z-ADD     TA7VAN        WVA
069000990910     C                   EXSR      CONTR
069100990910     C*
069200990823     C     KTA7          READE     TITA730C                               59
069300990817E   1C                   ENDDO
069400990817     C*
069500941122     C                   ENDSR
069600990910     C*--------------------------------------------------------
069700000124     C* CONTROLLO SE VARIE GIA' PRESENTI
069800000124     C*--------------------------------------------------------
069900990910     C     CONTR         BEGSR
070000000124IF  1C     WSV           IFNE      ' '
070100990910     C*
070200990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
070300990910     C     WSV           ANDNE     §$2IVA
070400990910     C                   ADD       1             A                 3 0
070500990910     C                   MOVEL     WSV           VSV(A)
070600990910     C                   Z-ADD     WVA           VVA(A)
070700990910E   4C                   ENDIF
070800000124E   1C                   ENDIF
070900000124     C                   ENDSR
071000050905
071100170227
071200170227
071300050905      /TITLE Scrittura record TIVAF in file TIVGD00F (file VAS generico download)
071400170227     C     wriVGD        BEGSR
071500140109     C*
071600050905     C                   clear                   tivgd000
071700080404     C                   eval      vgdDTA = %subst(TIVAFDS:1:%size(TIVAFDS))
071800050905     C                   eval      vgdTIP = 'VF'
071900050905     C                   eval      vgdKSU = curKSU
072000080404     C                   eval      vgdTSC = curTSC
072100050905     C                   eval      vgdDAT = datcor
072200140109     C                   eval      vgdPGM = 'TRTC28R'
072300170227     C                   eval      vgdSTO = '?'
072400140109     C*
072500140109     C*************
072600140113     C* Se specificatamente NON richiesto nel lancio ...
072700140113     C                   if        FILESEP <> 'N'
072800140113     C*
072900140109     C* Verifico la rottura di codice per KSU / DFT / NFT / FIV
073000140109     C                   eval      ds_KSU = curKSU
073100140109     C                   eval      ds_DFT = tasDFT
073200140109     C                   eval      ds_NFT = tasNFT
073300140109     C                   eval      ds_FIV = tasFIV
073400140109     C*
073500140109     C                   z-add     1             jPRG
073600140109     C     DS_PRG        lookup    skKSCU(jPRG)                           55
073700140109     C                   if        %found
073800140109     C                   eval      vgdPRG = skPRG(jPRG)
073900140109     C                   else
074000140109     C                   exsr      calprog
074100140109     C                   eval      vgdPRG = f_tis7prgf
074200140109     C                   add       1             tPRG
074300140109     C                   eval      skKSCU(tPRG) = DS_PRG
074400140109     C                   eval      skPRG(tPRG)  = vgdPRG
074500140109     C                   endif
074600140113     C*
074700140113     C                   endif
074800170227     C*************
074900170227     C*
075000170227     C* Salvo tutti i clienti unificanti scritti
075100170227     C                   eval      DS_VGDKSU.ds_KSU = vgdKSU
075200170227     C                   eval      DS_VGDKSU.ds_PRG = vgdPRG
075300170227     C     DS_VGDKSU     lookup    skVGDKSU                               55
075400170227     C                   if        not %found
075500170227     C                   add       1             jKSU
075600170227     C                   eval      skVGDKSU(jKSU) = DS_VGDKSU
075700170227     C                   endif
075800170227     C*
075900170227     C* Scarico ul buffer di output download
076000050905     C                   write     tivgd000
076100050905     C*
076200050905     C                   ENDSR
076300140108
076400140108
076500140108      /TITLE Valorizzazione Progressivo Applicazione
076600140108     C     calprog       begsr
076700140108     C*
076800140108     C                   movel     *blanks       dwlisv            2
076900140108     C                   movel     *all'0'       dwlprg           10
077000140108     C                   z-add     *zeros        wrkprg            8 0
077100140108     C*
077200140108     C                   eval      dwlisv = 'VF'
077300140108     C*
077400140108     C                   open      tis7prgf
077500140108     C*
077600140108     C                   read(e)   tis7prgf
077700140108     C                   if        not %error
077800140108     C                   eval      dwlprg = f_tis7prgf
077900140108     C*
078000140108     C                   move(p)   dwlprg        wrkprg
078100140108     C                   add       1             wrkprg
078200140108     C                   move(p)   wrkprg        dwlprg
078300140108     C                   movel     dwlisv        dwlprg
078400140108     C*
078500140108     C                   eval      f_tis7prgf = dwlprg
078600140108     C                   update    tis7prg0
078700140108     C                   endif
078800140108     C*
078900140108     C                   close     tis7prgf
079000140108     C*
079100140108     C                   endsr
079200140108
079300140108
079400050906     C*--------------------------------------------------------
079500050906     C* OPNFIL    CONTROLLO ESISTENZA DEL MEMBRO (MXXXXXXX    *
079600050906     C*           DOVE XXXXXXX=COD CLI) SE NON C'E AGGIUNGO E *
079700050906     C*           APRO IL FILE                                *
079800050906     C*--------------------------------------------------------
079900050906     C     OPNFIL        BEGSR
080000101025     C                   Z-ADD     50            LUNG             15 5
080100050906     C*
080200050906     C** CONTROLLO SE ESISTE IL MEMBRO
080300050906     C                   SETOFF                                       33        TIVAF00T
080400050906     C                   MOVE      VAR           CMD1
080500050906     C                   MOVEL     *BLANKS       COMMAN           80
080600050906     C                   MOVEA     CMD1(1)       COMMAN
080700050906     C                   CALL      'QCMDEXC'                            33
080800050906     C                   PARM                    COMMAN
080900050906     C                   PARM                    LUNG
081000050906     C** ESEGUE ADDPFM SOLO SE NON ESISTE MEMBRO
081100050906     C   33              DO
081200050906     C                   MOVE      VAR           CMD2
081300050906     C                   MOVEL     *BLANKS       COMMAN
081400050906     C                   MOVEA     CMD2(1)       COMMAN
081500050906     C                   CALL      'QCMDEXC'
081600050906     C                   PARM                    COMMAN
081700050906     C                   PARM                    LUNG
081800050906     C                   END
081900050906     C** FACCIO OVRDBF DEL FILE E LO APRO
082000050906     C                   MOVE      VAR           CMD3
082100050906     C                   MOVEL     *BLANKS       COMMAN
082200050906     C                   MOVEA     CMD3(1)       COMMAN
082300050906     C                   CALL      'QCMDEXC'
082400050906     C                   PARM                    COMMAN
082500050906     C                   PARM                    LUNG
082600050906     C**
082700060203     C                   IF        not %open(TIVAF00T)
082800050906     C                   OPEN      TIVAF00T
082900060203     C                   ENDIF
083000060203     C*
083100050906     C                   ENDSR
083200050905?    C*-------------------------------------------------
083300050906?    C* esegue - esegue comandi "CL"
083400050905?    C*-------------------------------------------------
083500050905     c     esegue        begsr
083600050905     c                   call      'QCMDEXC'                            60
083700050905     c                   parm                    comman
083800050905     c                   parm                    lung
083900050905     c                   endsr
084000101025     C*----------------------------------------------------*
084100101025     c* Scrittuta TIVAF per embro revisori
084200101025     C*----------------------------------------------------*
084300101025     c     ElaREV        BEGSR
084400101026     c
084500101026     C                   EXSR      DEFCAM
084600101026
084700101025     c                   eval      $Fine='0'
084800101025     c                   clear                   ksc
084900101025     c                   eval      VAR='REVISORI)'
085000101025     c                   exsr      opnfil
085100101025     c
085200101025     c* Lettura del primo codice cliente
085300101025    1c                   dow       $Fine='0'
085400101025     c     ksc           setgt     titas31c
085500101025     c                   read      titas31c
085600101025     c
085700101025    2c                   if        %eof(titas31c)
085800101025     c                   eval      $Fine='1'
085900101025   x2c                   else
086000101025     c
086100101025     c                   eval      ksc=tasksc
086200101025     c     ktas          setll     titas31c
086300101025     c     ktas          reade     titas31c
086400101025     c
086500101025    3c                   if        not %eof(titas31c)
086600101025     c* Escludo se non si tratta di cliente codificato
086700101025     c                   move      tasksc        w0040             4 0
086800101025
086900101025    4c                   if        w0040<>8888 and tasksc<>9999
087000101025
087100101025    5c                   dow       not %eof(titas31c)
087200101025     c
087300101026     c                   if        tasfiv>990
087400101025     c                   exsr      ScriviDati
087500101025     c
087600101025     c                   write     tivaf000
087700101026     c                   endif
087800101025     c
087900101025     c     ktas          reade     titas31c
088000101025    5c                   enddo
088100101025     c
088200101025    4c                   endif
088300101025     c
088400101025    3c                   endif
088500101025    2c                   endif
088600101025    1c                   enddo
088700101025     c*
088800101025     c                   if        %open(tivaf00t)
088900101025     c                   close     tivaf00t
089000101025     c                   endif
089100101025     c
089200101025     c                   ENDSR
089300990817     C*--------------------------------------------------------
089400151016
089500151016
089600151016
089700151016     C     RTVAR5FAT     BEGSR
089800151016     C*
089900151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
090000151028     C                   SETOFF                                       7172
090100151016     C                   EVAL      kAr5Trd = 'FAT'
090200151016     C                   CLEAR                   DAR5FAT
090300151016     C     kFiar5        CHAIN     FIAR531C
090400151016     C                   IF        %found(FIAR531C)
090500151016     C                   MOVEL     AR5UNI        DAR5FAT
090600170130     C                   IF        (§AR5FPTAS = 'T' OR §AR5FPTAS = 'D') AND
090700170130     C                             §AR5PKTAS > *zeros
090800151028     C                   SETON                                        71
090900151016     C                   EVAL      VAFPKC = §AR5PKTAS
091000151016     C                   ENDIF
091100170130     C                   IF        (§AR5FVTAS = 'T' OR §AR5FVTAS = 'D') AND
091200170130     C                             §AR5VLTAS > *zeros
091300151028     C                   SETON                                        72
091400160505     C                   EVAL      VAFVLC = §AR5VLTAS
091500151016     C                   ENDIF
091600151016     C                   ENDIF
091700151016     C*
091800151016     C                   ENDSR
091900160208
092000160208
092100160208
092200160209     C     RTVTA4        BEGSR
092300160208     C*
092400160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
092500160208     C                   CLEAR                   DTA4A
092600160209     C                   CLEAR                   DTA4M
092700160208     C*
092800160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
092900160208     C                   CALL      'UBTA400R'
093000160208     C                   PARM      *blanks       UBTA4IOPZ
093100160208     C                   PARM      *blanks       UBTA4ITLA
093200160208     C                   PARM      tasAAS        UBTA4IAAS
093300160208     C                   PARM      tasLNP        UBTA4ILNP
093400160208     C                   PARM      tasNRS        UBTA4INRS
093500160208     C                   PARM      tasNSP        UBTA4INSP
093600160209     C                   PARM                    UBTA4ITRC
093700160208     C                   PARM                    UBTA4OERR
093800160208     C                   PARM                    UBTA4ODS
093900160208     C                   PARM                    UBTA4OLEN
094000160208     C                   PARM                    UBTA4ODATI
094100160208     C*
094200160208     C                   IF        UBTA4OERR = *zeros
094300160208     C                   SELECT
094400160208     C* Gestione output tipo record 'A'
094500160208     C                   WHEN      UBTA4ODS = 'DTA4A'
094600160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
094700160209     C* Gestione output tipo record 'M'
094800160209     C                   WHEN      UBTA4ODS = 'DTA4M'
094900160209     C                   EVAL      DTA4M = %subst(UBTA4ODATI:1:UBTA4OLEN)
095000160208     C                   ENDSL
095100160208     C*
095200160208     C                   ENDIF
095300160208     C*
095400160208     C                   ENDSR
095500151016     C*--------------------------------------------------------
095600170227
095700170227
095800170227
095900170227     C*------------------------------------------------------------------------*
096000170227     C* EXEERR - Routine di esecuzione azioni su Errore
096100170227     C*------------------------------------------------------------------------*
096200170227     C     EXEERR        BEGSR
096300170227     C*
096400170227     C                   dump(A)
096500170227     C                   rolbk
096600170227     C                   seton                                        lr
096700170227     C                   return
096800170227     C*
096900170227     C                   ENDSR
097000170227     C*------------------------------------------------------------------------*
097100991110**         CMD1
097200000124CHKOBJ OBJ(TIVAF00T) OBJTYPE(*FILE) MBR(M0000000)
097300910830**         CMD2
097400000124ADDPFM FILE(TIVAF00T)               MBR(M0000000)
097500910830**         CMD3
097600000124OVRDBF FILE(TIVAF00T)               MBR(M0000000)
097700000721**         CMD5
097800000724OVRDBF FILE(TABELXXF) TOFILE(*LIBL/TABEL00F)
097900991110**         CMD
098000991110DLTOVR FILE(*ALL)
098100030515**         VALNUM
0982000305150123456789
