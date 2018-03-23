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
002400140108     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002500140108     F                                     PREFIX(f_)
002600140108     F                                     USROPN
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
003900140109     D skKSCU          s                   dim(50000) like(DS_PRG)
004000140109     D skPRG           s                   dim(50000) like(vgdPRG)
004100140109     D jPRG            s              5  0 inz
004200140109     D tPRG            s                   inz like(jPRG)
004300140109     D DS_PRG          ds
004400140109     D  ds_KSU                             like(vgdKSU)
004500140109     D  ds_DFT                             like(tasDFT)
004600140109     D  ds_NFT                             like(tasNFT)
004700140109     D  ds_FIV                             like(tasFIV)
004800140108     D
004900890921     D KPJBA         E DS
005000990817     D DS4C          E DS
005100060203     D DS4C_W        E DS                  EXTNAME(DS4C) PREFIX(W_)
005200050906     D DSQT1         E DS
005300111018     D**!!!DS4r          E DS
005400050906     D DS$2          E DS
005500030516     D DTA4A         E DS
005600160209     D DTA4M         E DS
005700050905     D tivafds       e ds                  extname(tivaf00t)
005800050906     D PARAM           DS
005900941122     D  DAL                    1      8  0
006000941122     D  AL                     9     16  0
006100050906     D  CLIU                  17     23  0
006200101025     D  REVISORI              24     24
006300140108     D  NFT                   25     30  0
006400140113     D  FILESEP               31     31
006500140319     D  FLGUNI                32     32
006600140319     D  CLIVAS                33     40
006700010402     D*------------------
006800030917      * DS FIAR5
006900030917     d dAr5Bnb       e ds
007000151016     D DAR5FAT       E DS
007100050126
007200050126     d Tibs02ds      e ds
007300111026     d*******dvus          e ds
007400051219
007500051219     d Og143         e ds                  inz
007600060127
007700060127     D trul47ds      e ds
007800030917      *
007900021212     D Wtara           s                   like(§QTTPC)
008000021212     D Wntara          s                   like(TASPKF)
008100030917
008200030917     d kAr5Trd         s                   Like(Ar5Trd)
008300030515
008400030515     D Rma15           s             15
008500030515     D WRma15          s             15
008600030515     D II              s              2  0
008700030515     D Pos             s              2  0
008800111026     D Var             s              9
008900050126     d wxco            s              1    inz(*Off)
009000101025     d $Fine           s              1    inz('0')
009100160208
009200160208     D*--------------------
009300160208     D* DEFINIZIONI A PROCEDURE ESTERNE
009400160208     D*--------------------
009500160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
009600160208
009700010402
009800101025     C*------------------------------------------------------------------
009900101025     C     *ENTRY        PLIST
010000101025     C                   PARM                    KPJBA
010100140319     C                   MOVEL     KPJBU         PARAM
010200140319     C*
010300140319     C* Verifico subito se richiesto un particolare cliente VAS (per output)
010400140319     C                   setoff                                       45
010500140319     C                   if        CLIVAS = *all'0' and CLIU <> *zeros
010600140319     C                   move      CLIU          CLIVAS
010700140319     C                   endif
010800140319     C                   if        CLIVAS <> *all'0'
010900140319     C                   seton                                        45
011000140319     C                   endif
011100140319     c*
011200101025     c* Verifico se elaborazione per REVISORI o per scarico a clienti
011300101025     c                   if        revisori='S'
011400101025     c                   exsr      ElaREV
011500101025     c                   else
011600101025     c                   exsr      ElaCLI
011700101025     c                   endif
011800160208     C*
011900160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
012000160208     C                   CALL      'UBTA400R'
012100160208     C                   PARM                    UBTA4IOPZ
012200160208     C                   PARM      'C'           UBTA4ITLA
012300160208     C                   PARM                    UBTA4IAAS
012400160208     C                   PARM                    UBTA4ILNP
012500160208     C                   PARM                    UBTA4INRS
012600160208     C                   PARM                    UBTA4INSP
012700160208     C                   PARM                    UBTA4ITRC
012800160208     C                   PARM                    UBTA4OERR
012900160208     C                   PARM                    UBTA4ODS
013000160208     C                   PARM                    UBTA4OLEN
013100160208     C                   PARM                    UBTA4ODATI
013200160208     C*
013300921023     C                   SETON                                        LR
013400910830     C*----------------------------------------------------*
013500101025     c* Scrittuta TIVGD per i clienti che lo hanno richiesto
013600101025     C*----------------------------------------------------*
013700101025     c     ElaCLI        BEGSR
013800101025     c
013900101025     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VF'
014000101025     C                   clear                   trul47ds
014100101025     C                   eval      d47opz  = 'I'
014200101025     C                   eval      d47tip  = 'VF'
014300101025     C                   eval      d47lck  = 'N'
014400101025     C                   eval      d47chkj = 'N'
014500101025     C                   eval      d47pgm  = 'TRTC28R'
014600101025     C                   call      'TRUL47R'
014700101025     C                   parm                    trul47ds
014800101025     C*
014900101025     C* Se elaborazione consentita => proseguo
015000101025     C                   if        d47sts <> 'A'
015100101025     C*
015200101025     C                   EXSR      DEFCAM
015300101025      *
015400101025     C     KTB           SETLL     TABEL
015500140326     C                   SETOFF                                       58
015600140326    1C                   IF        %equal(TABEL00F)
015700140326     C     KTB           READE     TABEL
015800140326    2C                   DOW       not %eof(TABEL00F)
015900101025    3C     TBLFLG        IFNE      '*'
016000101025     C                   MOVEL     TBLUNI        DS4C
016100101025    4C     §4CFSD        IFNE      *BLANK                                        CLI DA SCAR
016200101025     C                   MOVE      §4CFKS        KSCU              7 0
016300101025     C                   MOVEL     TBLKEY        KSC               7 0
016400101025     C*
016500140326    5C                   if        (CLIU>*zeros AND FLGUNI='S' AND CLIU=KSCU) OR
016600140326     C                             (CLIU>*zeros AND FLGUNI='N')               OR
016700140319     C                              CLIU=*zeros
016800140326     C                   if         CLIU>*zeros AND FLGUNI='N'
016900140326     C                   eval      KSC = CLIU
017000140326     C                   seton                                        58
017100140326     C                   endif
017200140326     C*
017300101025     c                   Eval      wxco = *Off
017400101025     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
017500140326   e5C                   endif
017600140326     C*
017700140326     C                   if        *in58
017800140326     C                   leave
017900140326     C                   endif
018000140326     C*
018100140326   e4C                   ENDIF
018200140326   e3C                   ENDIF
018300140326     C     KTB           READE     TABEL
018400140326   e2C                   ENDDO
018500140326   e1C                   ENDIF
018600101025     C*
018700101025     C* CAnCELLO LE OVR ESEGUITE
018800101025     C                   MOVEL     *BLANKS       COMMAN           80
018900101025     C                   Z-ADD     17            LUNG
019000101025     C                   MOVEA     CMD(1)        COMMAN
019100101025     C                   CALL      'QCMDEXC'                            21
019200101025     C                   PARM                    COMMAN
019300101025     C                   PARM                    LUNG
019400101025     C*
019500101025     C                   endif
019600101025     C*
019700101025     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
019800101025     C                   clear                   trul47ds
019900101025     C                   eval      d47opz  = 'F'
020000101025     C                   eval      d47tip  = 'VF'
020100101025     C                   call      'TRUL47R'
020200101025     C                   parm                    trul47ds
020300101025     C*
020400101025     c                   ENDSR
020500101025     C*----------------------------------------------------*
020600101025     c* operazioni  iniziali
020700101025     C*----------------------------------------------------*
020800910830     C     DEFCAM        BEGSR
020900050905      *
021000050905      * calcola la data corrente
021100160209     c                   z-add     *zeros        datcor            8 0          *d.corrente aa/m/g
021200160209     C                   eval      datcor = %dec(%date() : *ISO)
021300000721      *
021400050905      * Overrida tabel in tabelxx per gestione multimembro (stesso dato a 2 clienti)
021500000721     c                   movel     *blanks       comman
021600000721     c                   movea     cmd5          comman
021700000721     c                   z-add     44            lung
021800050906     c                   exsr      esegue
021900000721     c                   open      tabelxxf
022000941122     C* TABEL
022100921023     C     KTB           KLIST
022200910830     C                   KFLD                    TBLKUT
022300910902     C                   KFLD                    TBLCOD
022400030917      * FIAR
022500030917     c     kFiar5        Klist
022600030917     c                   Kfld                    TasAas
022700030917     c                   Kfld                    TasLnp
022800030917     c                   Kfld                    TasNrs
022900030917     c                   Kfld                    TasNsp
023000030917     c                   Kfld                    kAr5Trd
023100990817     C* TITAS
023200050905     C     KTAS          KLIST
023300050905     C                   KFLD                    KSC
023400050905     C                   KFLD                    DAL
023500990817     C* TITA7
023600050905     C     KTA7          KLIST
023700941122     C                   KFLD                    TASAAS
023800941122     C                   KFLD                    TASLNP
023900941122     C                   KFLD                    TASNRS
024000941122     C                   KFLD                    TASNSP
024100050905     C                   KFLD                    TASTBL
024200990910     C     *LIKE         DEFINE    TASVA1        WVA
024300990910     C     *LIKE         DEFINE    TASSV1        WSV
024400010402     C*
024500050905     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
024600050905     C     KORM          KLIST
024700160209     C                   KFLD                    §TA4MPOE
024800160209     C                   KFLD                    §TA4MNSR
024900160209     C                   KFLD                    §TA4MNOR
025000160209     C                   KFLD                    §TA4MNRV
025100990817     C*
025200990817     C* LEGGE IL RECORD SIGLE VARIE
025300990817     C     *LIKE         DEFINE    TBLKUT        KTBKUT
025400990817     C     *LIKE         DEFINE    TBLCOD        KTBCOD
025500990817     C     *LIKE         DEFINE    TBLKEY        KTBKEY
025600000721     C     *LIKE         DEFINE    TBLCOD        KTBCODx
025700000721     C     *LIKE         DEFINE    TBLKEY        KTBKEYx
025800000721     C     KEYTABx       KLIST
025900000721     C                   KFLD                    KTBKUT
026000000721     C                   KFLD                    KTBCODx
026100000721     C                   KFLD                    KTBKEYx
026200990817     C     KEYTAB        KLIST
026300990817     C                   KFLD                    KTBKUT
026400990817     C                   KFLD                    KTBCOD
026500990817     C                   KFLD                    KTBKEY
026600021212      *
026700021212      ** Aggancio tabella $2 - Varie forzate a programma
026800990817     C                   Z-ADD     1             KTBKUT
026900990817     C                   MOVEL     '$2'          KTBCOD
027000990817     C                   MOVEL     1             KTBKEY
027100990817     C     KEYTAB        CHAIN     TABEL00F                           99
027200990817IF  1C     *IN99         IFEQ      *OFF
027300990817     C     TBLFLG        ANDEQ     *BLANKS
027400990817     C                   MOVEL     TBLUNI        DS$2
027500990817E   1C                   ENDIF
027600021212      *
027700050905      ** Aggancio tabella QT - campi standard tassazione
027800021212     C                   MOVEL     'QT'          KTBCOD
027900021212     C                   MOVEL     1             KTBKEY
028000021212     C     KEYTAB        CHAIN     TABEL00F                           99
028100021212IF  1C     *IN99         IFEQ      *OFF
028200021212     C     TBLFLG        ANDEQ     *BLANKS
028300021212     C                   MOVEL     TBLUNI        DSQT1
028400021212E   1C                   ENDIF
028500990817     C*
028600021212      ** Imposto key per tabella clienti con ritorno info fattura
028700990817     C                   MOVE      1             TBLKUT
028800990817     C                   MOVEL     '4C'          TBLCOD
028900990817     C                   MOVEL     *BLANKS       TBLKEY
029000990817     C*
029100910830     C                   ENDSR
029200050906     C*--------------------------------------------------------
029300050905     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
029400000124     C*--------------------------------------------------------
029500921023     C     WRTFIL        BEGSR
029600990817     C     KTAS          SETLL     TITAS31C
029700921023     C                   DO        *HIVAL
029800990817     C     KSC           READE     TITAS31C                               70
029900000124     C  N70              DO
030000000124     C     TASNFT        IFGT      *ZERO                                        N.FATT.
030100921023     C     TASDFT        ANDLE     AL
030200140108
030300140108     C*
030400140108     C* Se richiesto filtro per numero fattura => verifico
030500140108     C                   IF        NFT <> *zeros AND
030600140108     C                             NFT <> tasNFT
030700140108     C* Skippo spedizione corrente
030800140108     C                   ELSE
030900050126
031000050126      * per prima cosa controllo se devo scrivere un membro apposta x l'accorpamento bolle
031100111026     c****               If        Tasxco <> *Blanks
031200050906      * se in tabella è prevista la creazione di un nuovo membro
031300111026     c****               If        §4cRfo = 'X'    and
031400050906      * e non era già stato aperto il membro x questo cliente
031500111026     c****                         wxco = *Off
031600050906      * chiudo il vecchio membro e apro il nuovo
031700111026     c****               Move      ') '          var               9
031800111026     c****               Movel     §4cMbx        var
031900111026     c****               ExSr      opnfil
032000111026     c****               Eval      wxco = *On
032100111026     c****               EndIf
032200111026     c****               EndIf
032300101025     c*
032400101025     c                   exsr      SCriviDati
032500101025     c
032600050906     C*
032700050906     C* Verifico se richiesta scrittura membro "particolare" x gestone XCO
032800050906     C                   if        wxco = *on
032900050906     C                   write     tivaf000
033000050906     C                   else
033100060203     C*
033200060203     C* Scrivo il file TIVGD00F (file VAS Generico Download)
033300140319     C                   movel     *all'0'       curKSU
033400140319     C                   if        not *in45
033500060203     C                   move      KSCU          curKSU
033600140319     C                   else
033700140319     C                   move      CLIVAS        curKSU
033800140319     C                   endif
033900060203     C                   eval      curTSC = §4CFSD
034000060203     C*
034100060203     C* Verifica il tipo scarico attribuito al cliente su cui unificare i dati del cliente corrente
034200060203     C                   clear                   DS4C_W
034300060203     C                   move      '4C'          ktbcodx
034400060203     C                   movel     KSCU          ktbkeyx
034500060203     C     keytabx       chain     tabelxxf                           89
034600060203     C                   if        %found(tabelxxf)
034700060203     C                   movel     xtbluni       DS4C_W
034800060203     C                   eval      curTSC = W_§4CFSD
034900060203     C                   endif
035000151016     C*
035100151016     C* Eseguo eventuali "forzature"
035200151016     C                   EXSR      RTVAR5FAT
035300060203     C*
035400060203     C* Quindi scarico il buffer nel file d out generico download
035500050905     C                   exsr      WRIVGD
035600050906     C                   endif
035700050905     C*
035800050905     C* Verifica se richiesta gestione multimembro (stesso dato a 2 clienti)
035900111018     c**!!!              move      '4R'          ktbcodx
036000111018     c**!!!              movel     tblkey        ktbkeyx
036100111018     c**!!!keytabx       chain     tabelxxf                           89
036200111018     c**!!!              if        %found(tabelxxf)
036300111018     c**!!!              movel     xtbluni       ds4r
036400111018     C**!!!              movel     *all'0'       curKSU
036500111018     C**!!!              move      §4RCKS        curKSU
036600111018     C**!!!              eval      curTSC = §4RCSD
036700111018     C**!!!              exsr      WRIVGD
036800111018     c**!!!              endif
036900050905     C*
037000140108     C                   ENDIF
037100140108     C*
037200940910     C                   END
037300921023     C                   END
037400000124     C  N70              END
037500000124     C                   ENDSR
037600101025     C*-----------------------------------------------------------*
037700101025     C* Scrivo dati nel file TIVAF
037800101025     C*-----------------------------------------------------------*
037900101025     c     Scrividati    BEGSR
038000101025     C*
038100101025     C* Inizializzo il buffer d output
038200101025     C                   clear                   TIVAFDS
038300160208     C*
038400160209     C* Reperisco TITA4 tipo record 'A'
038500160209     C                   MOVEL     'A'           UBTA4ITRC
038600160209     C                   EXSR      RTVTA4
038700101025     C*
038800101025     C                   MOVEL     TASTBL        VAFTBL
038900101025     C                   Z-ADD     TASLNP        VAFLNP
039000101025     C                   Z-ADD     TASMGS        VAFMGS
039100101025     C                   Z-ADD     TASAAS        VAFAAS
039200101025     C                   Z-ADD     TASNRS        VAFNRS
039300101025     C                   Z-ADD     TASNSP        VAFNSP
039400101025     C                   Z-ADD     TASKSC        VAFKSC
039500101025     C                   MOVEL     TASFIN        VAFFIN
039600101025     C                   Z-ADD     TASLNA        VAFLNA
039700101025      * se particolità varia = 'O' recupero da FIAR5 il numero colli originali
039800101025      *
039900101025     c                   If        %Subst(TasGva:1:1) = 'O'
040000101025
040100101025     c                   eval      kAr5Trd  = 'BNB'
040200101025     c                   clear                   dAr5Bnb
040300101025     c     kFiar5        Chain     Fiar531c
040400101025     c                   If        %Found(Fiar531c)
040500101025     c                   Movel     Ar5Uni        dAr5Bnb
040600101025     c                   EndIf
040700101025      * Colli originali
040800101025     c                   Eval      VafNcl = §Ar5bcor
040900101025
041000101025     c                   else
041100101025      * Colli TITAS
041200101025     C                   Z-ADD     TASNCL        VAFNCL
041300101025     c                   endif
041400101025
041500101025     C                   Z-ADD     TASVLF        VAFVLF
041600101025     C                   Z-ADD     TASPOR        VAFPOR
041700101025      * PESO
041800101025     C                   Z-ADD     TASPKF        VAFPKB
041900101025     C                   IF        TASFPF = 'V'
042000150716     C*
042100150716     C                   IF        TASDFT <= §QTDST
042200150716     C     §QTTCO        MULT      TASNCP        WTARA
042300150716     C     TASPKC        SUB(h)    WTARA         WNTARA
042400150716     C                   Z-ADD     WNTARA        VAFPKC
042500150716     C                   ELSE
042600101025     C     §QTTPC        MULT      TASNCP        WTARA
042700101025     C     TASPKC        SUB(h)    WTARA         WNTARA
042800150716     C                   Z-ADD     WNTARA        VAFPKC
042900150716     C                   ENDIF
043000150716     C*
043100101025     C                   ELSE
043200151020     C                   IF        TASFPF <> 'D'
043300150716     C                   Z-ADD     *zeros        VAFPKC
043400151020     C                   ENDIF
043500101025     C                   ENDIF
043600150716     C*
043700101025     C* AZZERO CAMPI NON SEMPRE PRESENTI
043800101025     C                   MOVE      *ZERO         WSV
043900101025     C                   MOVE      *ZERO         WVA
044000101025     C                   MOVEA     *BLANKS       VSV
044100101025     C                   MOVEA     *ZERO         VVA
044200101025     C                   CLEAR                   A
044300101025     C                   CLEAR                   VAFSV1
044400101025     C                   CLEAR                   VAFVA1
044500101025     C                   CLEAR                   VAFSV2
044600101025     C                   CLEAR                   VAFVA2
044700101025     C                   CLEAR                   VAFSV3
044800101025     C                   CLEAR                   VAFVA3
044900101025     C                   CLEAR                   VAFSV4
045000101025     C                   CLEAR                   VAFVA4
045100101025     C                   CLEAR                   VAFSV5
045200101025     C                   CLEAR                   VAFVA5
045300101025     C                   CLEAR                   VAFSV6
045400101025     C                   CLEAR                   VAFVA6
045500101025     C                   CLEAR                   VAFSV7
045600101025     C                   CLEAR                   VAFVA7
045700101025     C                   CLEAR                   VAFSV8
045800101025     C                   CLEAR                   VAFVA8
045900101025     C                   CLEAR                   VAFVAX
046000101025     C*
046100101025     C                   MOVEL     TASSV1        WSV
046200101025     C                   Z-ADD     TASVA1        WVA
046300101025     C                   EXSR      CONTR
046400101025     C                   MOVEL     TASSV2        WSV
046500101025     C                   Z-ADD     TASVA2        WVA
046600101025     C                   EXSR      CONTR
046700101025     C                   MOVEL     TASSV3        WSV
046800101025     C                   Z-ADD     TASVA3        WVA
046900101025     C                   EXSR      CONTR
047000101025     C*
047100101025     C                   Z-ADD     TASDFT        VAFDFT
047200101025     C                   Z-ADD     TASNFT        VAFNFT
047300101025      *
047400101025     c                   if        tascbo <> 'FO'
047500101025     C                   Z-ADD     TASRMN        VAFRMN
047600101025     c                   endif
047700101025      *
047800101025     C                   MOVEL     TASFTC        VAFFTC
047900101025     C                   MOVEL     TASTSP        VAFTSP
048000101025     C                   MOVEL     TASCTS        VAFCTS
048100101025     C                   MOVEL     TASFAP        VAFFAP
048200101025     C                   Z-ADD     TASCTR        VAFCTR
048300101025     C                   Z-ADD     TASQFT        VAFQFT
048400101025     C                   MOVEL     TASTC2        VAFTC2
048500101025     C                   Z-ADD     TASIMV        VAFIMV
048600101025     C                   Z-ADD     TASFIV        VAFFIV
048700101025     C                   MOVEL     TASDIV        VAFDIV
048800101025     C                   Z-ADD     TASDRT        VAFDRT
048900160208     C                   MOVEL     §TA4ANAS      VAFNAS
049000101025     C                   MOVEL     TASCAD        VAFCAD
049100101025     C                   MOVEL     TASLOD        VAFLOD
049200101025     C                   MOVEL     TASPRD        VAFPRD
049300101025     C                   MOVEL     TASNZD        VAFNZD
049400101025     C                   MOVEL     TASVAS        VAFVAS
049500101025     C                   Z-ADD     TASIAS        VAFIAS
049600101025    *C                   MOVEL     *blanks       VAFFPC
049700101025    *C                   MOVEL     *blanks       VAFFVC
049800101025    *C                   Z-ADD     *zeros        VAFVLC
049900101025     C     TASSV3        IFNE      *BLANK
050000101025     C                   EXSR      SRTA7
050100101025     C                   END
050200101025     C                   MOVEL     *BLANKS       VAFRMA
050300101025     C* se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
050400160222     c                   if        tascbo <> 'FO'
050500160208     C                   MOVEL     DTA4A         VAFRMA
050600101025     C                   ENDIF
050700101025     C* se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
050800160222     c                   if        tascbo = 'FO'
050900101025     c                   if        §ta4arma <> *blanks
051000101025     C                   MOVEL     §TA4ARMA      Rma15
051100101025      * verifico se campo numerico escludendo i blanks
051200101025     c                   eval      II = 1
051300101025     c                   dow       II <= %len(%trim(Rma15))
051400101025      *
051500101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
051600101025     c                   eval      WRma15 = (%trim(Wrma15)
051700101025     c                                      + %subst(%trim(Rma15):II:1))
051800101025     c                   endif
051900101025      *
052000101025     c                   eval      II = II + 1
052100101025      *
052200101025     c                   enddo
052300101025     c                   clear                   rma15
052400101025     c                   evalr     rma15 = %trim(Wrma15)
052500101025      *
052600101025     c                   eval      II = 1
052700101025     c                   setoff                                       29
052800101025     c                   dow       II <= %len(%trim(Rma15))
052900101025      *
053000101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
053100101025     c                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
053200101025     c                             VALNUM(1))
053300101025     c                   if        pos = 0
053400101025     c                   seton                                        29
053500101025     c                   leave
053600101025     c                   endif
053700101025     c                   endif
053800101025      *
053900101025     c                   eval      II = II + 1
054000101025      *
054100101025     c                   enddo
054200101025     c                   if        *in29 = *OFF
054300101025     c                   eval      VAFRMA= %editc(tasrmn:'Z')
054400101025     C                   MOVEL     rma15         VAFRMN
054500101025     c                   else
054600101025     C                   EVAL      VAFRMA = §ta4arma
054700101025     C                   EVAL      VAFRMN = tasrmn
054800101025     c                   endif
054900101025      *
055000101025     C                   endif
055100101025     C                   endif
055200101025     C*
055300101025     C* SCRIVO LE VARIE
055400101025     C                   Z-ADD     1             A
055500101025     C     VSV(A)        DOWNE     ' '
055600101025     C                   SELECT
055700101025     C     VAFSV1        WHENEQ    ' '
055800101025     C                   MOVEL     VSV(A)        VAFSV1
055900101025     C                   Z-ADD     VVA(A)        VAFVA1
056000101025     C*
056100101025     C     VAFSV2        WHENEQ    ' '
056200101025     C                   MOVEL     VSV(A)        VAFSV2
056300101025     C                   Z-ADD     VVA(A)        VAFVA2
056400101025     C*
056500101025     C     VAFSV3        WHENEQ    ' '
056600101025     C                   MOVEL     VSV(A)        VAFSV3
056700101025     C                   Z-ADD     VVA(A)        VAFVA3
056800101025     C*
056900101025     C     VAFSV4        WHENEQ    ' '
057000101025     C                   MOVEL     VSV(A)        VAFSV4
057100101025     C                   Z-ADD     VVA(A)        VAFVA4
057200101025     C*
057300101025     C     VAFSV5        WHENEQ    ' '
057400101025     C                   MOVEL     VSV(A)        VAFSV5
057500101025     C                   Z-ADD     VVA(A)        VAFVA5
057600101025     C*
057700101025     C     VAFSV6        WHENEQ    ' '
057800101025     C                   MOVEL     VSV(A)        VAFSV6
057900101025     C                   Z-ADD     VVA(A)        VAFVA6
058000101025     C*
058100101025     C     VAFSV7        WHENEQ    ' '
058200101025     C                   MOVEL     VSV(A)        VAFSV7
058300101025     C                   Z-ADD     VVA(A)        VAFVA7
058400101025     C*
058500101025     C     VAFSV8        WHENEQ    ' '
058600101025     C                   MOVEL     VSV(A)        VAFSV8
058700101025     C                   Z-ADD     VVA(A)        VAFVA8
058800101025     C                   OTHER
058900101025     C                   ADD       VVA(A)        VAFVAX
059000101025     C                   ENDSL
059100101025     C*
059200101025     C                   ADD       1             A
059300101025     C                   ENDDO
059400101025      * Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
059500101025     c                   Clear                   VafRmx
059600101025     C                   MOVEL     VAFKSC        WLB3              3 0
059700101025     c     WLB3          chain     AZORG
059800101025     c                   if            %found(AZORG01L)
059900101025     c                             and ORGfva = *blanks
060000101025     c                   movel     ORGde3        Og143
060100101025     c                   else
060200101025     c                   clear                   Og143
060300101025     c                   endif
060400101025
060500101025     c                   Select
060600101025
060700160209     C* Recupera Riferimento Alfanumerico da ORM
060800101025     C                   When      §4cRfo = 'S'
060900160209     C*
061000160209     C* Reperisco TITA4 tipo record 'M'
061100160209     C                   MOVEL     'M'           UBTA4ITRC
061200160209     C                   EXSR      RTVTA4
061300160209     C*
061400160209     C                   IF        DTA4M <> *zeros
061500101025     C     KORM          CHAIN     FNORM01L                           61
061600160209     C     *IN61         IFEQ      *OFF
061700101025     C                   MOVEL     ORMRFA        VAFRMX
061800101025     C                   ENDIF
061900160209     C                   ENDIF
062000160209     C*
062100101025      * Ritorna al cliente il campo XCO
062200111026     c**                 When      §4cRfo = 'X' and TasXco <> *Blanks
062300101025      * recupero il valore da tornare dalla tabella VUS
062400111026     c**                 Clear                   Tibs02ds
062500111026     c**                 Clear                   dvus
062600111026     c**                 Eval      T02mod = 'C'
062700111026     c**                 Eval      T02sif = knsif
062800111026     c**                 Eval      T02cod = 'VUS'
062900111026     c**                 Eval      T02ke1 = TasXco
063000111026     c**                 Call      'TIBS02R'
063100111026     c**                 Parm                    kpjba
063200111026     c**                 Parm                    Tibs02ds
063300111026     c**                 If        T02err = *blanks
063400111026     c**                 Eval      dvus = T02uni
063500111026     c**                 EndIf
063600111026     c**                 Eval      vafrmx = §vuvaf
063700101025
063800160209     C*
063900101025     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
064000101025     ***C                   When      wlb3 = 190  or wlb3 = 195 or wlb3 = 191
064100101025     c                   When      §OGntw = 'DPD'
064200101025     C                   movel     TASDCM        VAFRMX
064300101025
064400101025     c                   EndSl
064500101025     c
064600101025     c                   ENDSR
064700000124     C*-----------------------------------------------------------*
064800000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
064900000124     C*-----------------------------------------------------------*
065000941122     C     SRTA7         BEGSR
065100990817     C*
065200990823     C     KTA7          SETLL     TITA730C
065300990823     C     KTA7          READE     TITA730C                               59
065400990817DO  1C     *IN59         DOWEQ     *OFF
065500990910     C                   MOVEL     TA7SVN        WSV
065600990910     C                   Z-ADD     TA7VAN        WVA
065700990910     C                   EXSR      CONTR
065800990910     C*
065900990823     C     KTA7          READE     TITA730C                               59
066000990817E   1C                   ENDDO
066100990817     C*
066200941122     C                   ENDSR
066300990910     C*--------------------------------------------------------
066400000124     C* CONTROLLO SE VARIE GIA' PRESENTI
066500000124     C*--------------------------------------------------------
066600990910     C     CONTR         BEGSR
066700000124IF  1C     WSV           IFNE      ' '
066800990910     C*
066900990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
067000990910     C     WSV           ANDNE     §$2IVA
067100990910     C                   ADD       1             A                 3 0
067200990910     C                   MOVEL     WSV           VSV(A)
067300990910     C                   Z-ADD     WVA           VVA(A)
067400990910E   4C                   ENDIF
067500000124E   1C                   ENDIF
067600000124     C                   ENDSR
067700050905
067800050905      /TITLE Scrittura record TIVAF in file TIVGD00F (file VAS generico download)
067900050905     C     wriVGD        BEGSR
068000140109     C*
068100050905     C                   clear                   tivgd000
068200080404     C                   eval      vgdDTA = %subst(TIVAFDS:1:%size(TIVAFDS))
068300050905     C                   eval      vgdTIP = 'VF'
068400050905     C                   eval      vgdKSU = curKSU
068500080404     C                   eval      vgdTSC = curTSC
068600050905     C                   eval      vgdDAT = datcor
068700140109     C                   eval      vgdPGM = 'TRTC28R'
068800140109     C*
068900140109     C*************
069000140113     C* Se specificatamente NON richiesto nel lancio ...
069100140113     C                   if        FILESEP <> 'N'
069200140113     C*
069300140109     C* Verifico la rottura di codice per KSU / DFT / NFT / FIV
069400140109     C                   eval      ds_KSU = curKSU
069500140109     C                   eval      ds_DFT = tasDFT
069600140109     C                   eval      ds_NFT = tasNFT
069700140109     C                   eval      ds_FIV = tasFIV
069800140109     C*
069900140109     C                   z-add     1             jPRG
070000140109     C     DS_PRG        lookup    skKSCU(jPRG)                           55
070100140109     C                   if        %found
070200140109     C                   eval      vgdPRG = skPRG(jPRG)
070300140109     C                   else
070400140109     C                   exsr      calprog
070500140109     C                   eval      vgdPRG = f_tis7prgf
070600140109     C                   add       1             tPRG
070700140109     C                   eval      skKSCU(tPRG) = DS_PRG
070800140109     C                   eval      skPRG(tPRG)  = vgdPRG
070900140109     C                   endif
071000140113     C*
071100140113     C                   endif
071200140109     C*************
071300050905     C                   write     tivgd000
071400050905     C*
071500050905     C                   ENDSR
071600140108
071700140108
071800140108      /TITLE Valorizzazione Progressivo Applicazione
071900140108     C     calprog       begsr
072000140108     C*
072100140108     C                   movel     *blanks       dwlisv            2
072200140108     C                   movel     *all'0'       dwlprg           10
072300140108     C                   z-add     *zeros        wrkprg            8 0
072400140108     C*
072500140108     C                   eval      dwlisv = 'VF'
072600140108     C*
072700140108     C                   open      tis7prgf
072800140108     C*
072900140108     C                   read(e)   tis7prgf
073000140108     C                   if        not %error
073100140108     C                   eval      dwlprg = f_tis7prgf
073200140108     C*
073300140108     C                   move(p)   dwlprg        wrkprg
073400140108     C                   add       1             wrkprg
073500140108     C                   move(p)   wrkprg        dwlprg
073600140108     C                   movel     dwlisv        dwlprg
073700140108     C*
073800140108     C                   eval      f_tis7prgf = dwlprg
073900140108     C                   update    tis7prg0
074000140108     C                   endif
074100140108     C*
074200140108     C                   close     tis7prgf
074300140108     C*
074400140108     C                   endsr
074500140108
074600140108
074700050906     C*--------------------------------------------------------
074800050906     C* OPNFIL    CONTROLLO ESISTENZA DEL MEMBRO (MXXXXXXX    *
074900050906     C*           DOVE XXXXXXX=COD CLI) SE NON C'E AGGIUNGO E *
075000050906     C*           APRO IL FILE                                *
075100050906     C*--------------------------------------------------------
075200050906     C     OPNFIL        BEGSR
075300101025     C                   Z-ADD     50            LUNG             15 5
075400050906     C*
075500050906     C** CONTROLLO SE ESISTE IL MEMBRO
075600050906     C                   SETOFF                                       33        TIVAF00T
075700050906     C                   MOVE      VAR           CMD1
075800050906     C                   MOVEL     *BLANKS       COMMAN           80
075900050906     C                   MOVEA     CMD1(1)       COMMAN
076000050906     C                   CALL      'QCMDEXC'                            33
076100050906     C                   PARM                    COMMAN
076200050906     C                   PARM                    LUNG
076300050906     C** ESEGUE ADDPFM SOLO SE NON ESISTE MEMBRO
076400050906     C   33              DO
076500050906     C                   MOVE      VAR           CMD2
076600050906     C                   MOVEL     *BLANKS       COMMAN
076700050906     C                   MOVEA     CMD2(1)       COMMAN
076800050906     C                   CALL      'QCMDEXC'
076900050906     C                   PARM                    COMMAN
077000050906     C                   PARM                    LUNG
077100050906     C                   END
077200050906     C** FACCIO OVRDBF DEL FILE E LO APRO
077300050906     C                   MOVE      VAR           CMD3
077400050906     C                   MOVEL     *BLANKS       COMMAN
077500050906     C                   MOVEA     CMD3(1)       COMMAN
077600050906     C                   CALL      'QCMDEXC'
077700050906     C                   PARM                    COMMAN
077800050906     C                   PARM                    LUNG
077900050906     C**
078000060203     C                   IF        not %open(TIVAF00T)
078100050906     C                   OPEN      TIVAF00T
078200060203     C                   ENDIF
078300060203     C*
078400050906     C                   ENDSR
078500050905?    C*-------------------------------------------------
078600050906?    C* esegue - esegue comandi "CL"
078700050905?    C*-------------------------------------------------
078800050905     c     esegue        begsr
078900050905     c                   call      'QCMDEXC'                            60
079000050905     c                   parm                    comman
079100050905     c                   parm                    lung
079200050905     c                   endsr
079300101025     C*----------------------------------------------------*
079400101025     c* Scrittuta TIVAF per embro revisori
079500101025     C*----------------------------------------------------*
079600101025     c     ElaREV        BEGSR
079700101026     c
079800101026     C                   EXSR      DEFCAM
079900101026
080000101025     c                   eval      $Fine='0'
080100101025     c                   clear                   ksc
080200101025     c                   eval      VAR='REVISORI)'
080300101025     c                   exsr      opnfil
080400101025     c
080500101025     c* Lettura del primo codice cliente
080600101025    1c                   dow       $Fine='0'
080700101025     c     ksc           setgt     titas31c
080800101025     c                   read      titas31c
080900101025     c
081000101025    2c                   if        %eof(titas31c)
081100101025     c                   eval      $Fine='1'
081200101025   x2c                   else
081300101025     c
081400101025     c                   eval      ksc=tasksc
081500101025     c     ktas          setll     titas31c
081600101025     c     ktas          reade     titas31c
081700101025     c
081800101025    3c                   if        not %eof(titas31c)
081900101025     c* Escludo se non si tratta di cliente codificato
082000101025     c                   move      tasksc        w0040             4 0
082100101025
082200101025    4c                   if        w0040<>8888 and tasksc<>9999
082300101025
082400101025    5c                   dow       not %eof(titas31c)
082500101025     c
082600101026     c                   if        tasfiv>990
082700101025     c                   exsr      ScriviDati
082800101025     c
082900101025     c                   write     tivaf000
083000101026     c                   endif
083100101025     c
083200101025     c     ktas          reade     titas31c
083300101025    5c                   enddo
083400101025     c
083500101025    4c                   endif
083600101025     c
083700101025    3c                   endif
083800101025    2c                   endif
083900101025    1c                   enddo
084000101025     c*
084100101025     c                   if        %open(tivaf00t)
084200101025     c                   close     tivaf00t
084300101025     c                   endif
084400101025     c
084500101025     c                   ENDSR
084600990817     C*--------------------------------------------------------
084700151016
084800151016
084900151016
085000151016     C     RTVAR5FAT     BEGSR
085100151016     C*
085200151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
085300151028     C                   SETOFF                                       7172
085400151016     C                   EVAL      kAr5Trd = 'FAT'
085500151016     C                   CLEAR                   DAR5FAT
085600151016     C     kFiar5        CHAIN     FIAR531C
085700151016     C                   IF        %found(FIAR531C)
085800151016     C                   MOVEL     AR5UNI        DAR5FAT
085900170130     C                   IF        (§AR5FPTAS = 'T' OR §AR5FPTAS = 'D') AND
086000170130     C                             §AR5PKTAS > *zeros
086100151028     C                   SETON                                        71
086200151016     C                   EVAL      VAFPKC = §AR5PKTAS
086300151016     C                   ENDIF
086400170130     C                   IF        (§AR5FVTAS = 'T' OR §AR5FVTAS = 'D') AND
086500170130     C                             §AR5VLTAS > *zeros
086600151028     C                   SETON                                        72
086700160505     C                   EVAL      VAFVLC = §AR5VLTAS
086800151016     C                   ENDIF
086900151016     C                   ENDIF
087000151016     C*
087100151016     C                   ENDSR
087200160208
087300160208
087400160208
087500160209     C     RTVTA4        BEGSR
087600160208     C*
087700160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
087800160208     C                   CLEAR                   DTA4A
087900160209     C                   CLEAR                   DTA4M
088000160208     C*
088100160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
088200160208     C                   CALL      'UBTA400R'
088300160208     C                   PARM      *blanks       UBTA4IOPZ
088400160208     C                   PARM      *blanks       UBTA4ITLA
088500160208     C                   PARM      tasAAS        UBTA4IAAS
088600160208     C                   PARM      tasLNP        UBTA4ILNP
088700160208     C                   PARM      tasNRS        UBTA4INRS
088800160208     C                   PARM      tasNSP        UBTA4INSP
088900160209     C                   PARM                    UBTA4ITRC
089000160208     C                   PARM                    UBTA4OERR
089100160208     C                   PARM                    UBTA4ODS
089200160208     C                   PARM                    UBTA4OLEN
089300160208     C                   PARM                    UBTA4ODATI
089400160208     C*
089500160208     C                   IF        UBTA4OERR = *zeros
089600160208     C                   SELECT
089700160208     C* Gestione output tipo record 'A'
089800160208     C                   WHEN      UBTA4ODS = 'DTA4A'
089900160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
090000160209     C* Gestione output tipo record 'M'
090100160209     C                   WHEN      UBTA4ODS = 'DTA4M'
090200160209     C                   EVAL      DTA4M = %subst(UBTA4ODATI:1:UBTA4OLEN)
090300160208     C                   ENDSL
090400160208     C*
090500160208     C                   ENDIF
090600160208     C*
090700160208     C                   ENDSR
090800151016     C*--------------------------------------------------------
090900991110**         CMD1
091000000124CHKOBJ OBJ(TIVAF00T) OBJTYPE(*FILE) MBR(M0000000)
091100910830**         CMD2
091200000124ADDPFM FILE(TIVAF00T)               MBR(M0000000)
091300910830**         CMD3
091400000124OVRDBF FILE(TIVAF00T)               MBR(M0000000)
091500000721**         CMD5
091600000724OVRDBF FILE(TABELXXF) TOFILE(*LIBL/TABEL00F)
091700991110**         CMD
091800991110DLTOVR FILE(*ALL)
091900030515**         VALNUM
0920000305150123456789
