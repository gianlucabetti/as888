000100960329      ***********************************************************************
000200960925      *   SCRIVO ARCHIVIO BOLLE DA WORK FILE
000300960329      ***********************************************************************
000400960329     H DECEDIT('0,') DATEDIT(*DMY.)
000500980219      *---------------------------------------------------------------------*
000600980219      *  DATA BASE                                                          *
000700980219      *---------------------------------------------------------------------*
000800090216     FEDRDE01L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
000900980219      *---------
001000090216     FEDSUM02L  UF   E           K DISK    USROPN  COMMIT(prmCmtCtl)
001100980219      *---------
001200090216     FEDiVAB1L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
001300980219      *---------
001400121102     FEDiVAD1L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
001500980219      *---------
001600121102     FEDiVAT1L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
001700040709      *---------
001800121102     FEDiVAX1L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
001900980219      *---------
002000090216     FFiVAB01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
002100980219      *---------
002200090216     FFiVAD01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
002300980219      *---------
002400090216     FFiVAT01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
002500040709      *---------
002600090216     FFiVAX01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
002700100604      *---------
002800100604     FTABEL00F  IF   E           K DISK
002900980219      *---------------------------------------------------------------------*
003000980219      * DS
003100980219      *---------------------------------------------------------------------*
003200980219      *
003300960329     D KPJBA         E DS
003400041116     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003500041116     D CNCR80        E DS
003600100604     D ds7R          E DS
003700041116     d
003800041116     D TRUL06DS      E DS
003900041116     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
004000960925     D TRTC86        E DS                  EXTNAME(TRTC86DS)
004100060126     d Fnls25ds      e ds
004200100604     d
004300100604     D KCOD            S                   LIKE(TBLcod)
004400100604     D KKEY            S                   LIKE(TBLkey)
004500980219      *---------------------------------------------------------------------*
004600980219      * Ciclo principale
004700980219      *---------------------------------------------------------------------*
004800980219      *
004900040712      * Posizionamento iniziale x CMR
005000960925     C                   Z-ADD     D86CCM        KCCM
005100040712     C                   MOVEL     D86CMR        KCMR
005200960925     C                   Z-ADD     D86CNT        KCNT
005300040712      *
005400040712      * Elaborazione file VAB (testate)
005500040712     C     KVA_1         SETLL     EDiVAB1L                               30
005600040712     C                   IF        %equal(EDiVAB1L)
005700040712     C     KVA_1         READE     EDiVAB1L                               30
005800980219      *
005900040712     C     *IN30         DOWEQ     '0'
006000041116     c* elaboro solo se fgs  della £1 del p.o. utente
006100041116     c     vabfgs        lookup    lin                                    50
006200041214     c                   if        *in50  and vabfgs>0
006300041214     c                             or D86ctl = 'N'
006400041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
006500041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
006600041214      * impostato a "N"
006700040712      *
006800040712      * Imposto la chiave completa x spedizione
006900121102     C  N30              Z-ADD     VABFGS        KFGS
007000040712     C  N30              Z-ADD     VABAAS        KAAS
007100040712     C  N30              Z-ADD     VABLNP        KLNP
007200040712     C  N30              Z-ADD     VABNRS        KNRS
007300040712     C  N30              Z-ADD     VABNSP        KNSP
007400040712      *
007500020909      *  Se devo scrivere FiVAB procedo al posizionamento
007600040712     C     D86MOD        IFEQ      'E'
007700040712     C                   WRITE     FiVAB000
007800121102     C*
007900121102     C* Eseguo operazioni x estensioni: VAD / VAT / VAX
008000121102     C                   EXSR      SUBVAD
008100121102     C                   EXSR      SUBVAT
008200121102     C                   EXSR      SUBVAX
008300121102     C*
008400040712     C                   ELSE
008500980219     C                   EXSR      SUBDEL
008600040712     C                   ENDIF
008700060126
008800060126      * se bolla con "RC" ed è stato richiesto l'annullamento devo anche cancellare
008900060126      * l'ORM legato alla bolla
009000100604     c
009100100604     c                   clear                   ds7R
009200100604     c                   If        vabGma <>*blanks
009300100604     C                   eval      KCOD = '7R'
009400100604     C                   movel(P)  vabgma        KKEY
009500100604     C     keytab        chain     TABEL
009600100604     c                   IF        %found
009700100604     C                   movel     TBLUNI        ds7R
009800100604     c                   ENDIF
009900100604     c                   ENDIF
010000100604     c                   If        §7rRC='S'   and  d86mod = 'C'
010100060126     c                   ExSr      Delorm
010200060126     c                   EndIf
010300980219      *
010400040712      * Deleto bolla testata su EDIVAB
010500020909     C                   DELETE    EDiVAB00
010600041116     c                   endif
010700041116     c
010800040712      * Proseguo la lettura del file VAB (testate)
010900040712     C     KVA_1         READE     EDiVAB1L                               30
011000040712     C                   ENDDO
011100040712     C                   ENDIF
011200980219      *
011300980219      *  Controllo come devo chiudere il pgm
011400040712     C     D86CHI        IFEQ      'RT'
011500980219      *  Se devo chiudere con RT accendo indicatore
011600960925     C                   SETON                                        RT
011700040712     C                   ELSE
011800980219      *  Se devo chiudere con LR accendo indicatore LR
011900960925     C                   SETON                                        LR
012000040712     C                   ENDIF
012100980219      *---------------------------------------------------------------------*
012200040712      * SUBDEL: CANCELLO FILE APPOGGIO
012300980219      *---------------------------------------------------------------------*
012400980219     C     SUBDEL        BEGSR
012500980219      *
012600980219      * Loop delete EDRDE
012700980219     C     KRDE          DELETE    EDRDE01L                           33
0128009802193-   C     *IN33         DOWEQ     '0'
012900980219     C     KRDE          DELETE    EDRDE01L                           33
013000040712-3   C                   ENDDO
013100980219      * Loop delete EDSUM
013200980219     C                   Z-ADD     VABCCM        KCCM
013300980219     C                   Z-ADD     0             KNFE
013400980219     C                   MOVEL     *BLANKS       KFLG
0135009802193-   C     D86DTA        IFEQ      'S'
013600980219     C     KSUM          DELETE    EDSUM02L                           33
0137009802194-   C     *IN33         DOWEQ     '0'
013800980219     C     KSUM          DELETE    EDSUM02L                           33
013900040712-4   C                   ENDDO
014000040712-3   C                   ENDIF
014100980219      *
014200980219     C                   ENDSR
014300980219      *---------------------------------------------------------------------*
014400020909      * SUBVAD: ELABORO EDiVAD
014500980219      *---------------------------------------------------------------------*
014600980219     C     SUBVAD        BEGSR
014700980219      *
014800121102     C     KVA_C         CHAIN     EDiVAD1L                           34
014900980219      *
015000040712     C     *IN34         DOWEQ     '0'
015100041116     c     vadfgs        lookup    lin                                    50
015200041116     c                   if        *in50 and vadfgs>0
015300041214     c                             or D86ctl = 'N'
015400041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
015500041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
015600041214      * impostato a "N"
015700041214      *
015800980219     C     D86MOD        IFEQ      'E'
015900040712     C                   WRITE     FiVAD000
016000040712     C                   ENDIF
016100980219      *
016200020909     C                   DELETE    EDiVAD00
016300041116     c                   endif
016400980219      *
016500121102     C     KVA_C         READE     EDiVAD1L                               34
016600040712     C                   ENDDO
016700980219      *
016800980219     C                   ENDSR
016900980219      *---------------------------------------------------------------------*
017000020909      * SUBVAT: ELABORO EDiVAT
017100980219      *---------------------------------------------------------------------*
017200980219     C     SUBVAT        BEGSR
017300980219      *
017400020909      *  Scrivo FiVAT e cancello EDiVAT
017500121102     C     KVA_C         CHAIN     EDiVAT1L                           34
017600121102      *
017700980219     C     *IN34         DOWEQ     '0'
017800041116     c     vatfgs        lookup    lin                                    50
017900041116     c                   if        *in50 and vatfgs> 0
018000041214     c                             or D86ctl = 'N'
018100041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
018200041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
018300041214      * impostato a "N"
018400980219      *
018500980219     C     D86MOD        IFEQ      'E'
018600020909     C                   WRITE     FiVAT000
018700040712     C                   ENDIF
018800980219      *
018900020909     C                   DELETE    EDiVAT00
019000041116     c                   endif
019100980219      *
019200121102     C     KVA_C         READE     EDiVAT1L                               34
019300040712     C                   ENDDO
019400040107      *
019500980219     C                   ENDSR
019600040712      *---------------------------------------------------------------------*
019700040712      * SUBVAX: ELABORO EDiVAX
019800040712      *---------------------------------------------------------------------*
019900040712     C     SUBVAX        BEGSR
020000040712      *
020100040712      *  Scrivo FiVAT e cancello EDiVAT
020200121102     C     KVA_C         CHAIN     EDiVAX1L                           34
020300121102      *
020400040712     C     *IN34         DOWEQ     '0'
020500041116     c     vaxfgs        lookup    lin                                    50
020600041116     c                   if        *in50 and vaxfgs>0
020700041214     c                             or D86ctl = 'N'
020800041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
020900041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
021000041214      * impostato a "N"
021100040712      *
021200040712     C     D86MOD        IFEQ      'E'
021300040712     C                   WRITE     FiVAX000
021400040712     C                   ENDIF
021500040712      *
021600040712     C                   DELETE    EDiVAX00
021700041116     C                   ENDIF
021800040712      *
021900121102     C     KVA_C         READE     EDiVAX1L                               34
022000040712     C                   ENDDO
022100040712      *
022200040712     C                   ENDSR
022300060126      *---------------------------------------------------------------------*
022400060126      * DELORM: Cancello l'ORM legato alla bolla RC
022500060126      *---------------------------------------------------------------------*
022600060126     c     Delorm        BegSr
022700060126
022800060126     c                   Clear                   fnls25ds
022900060126     c                   Eval      i25Ksc = vabccm
023000060126     c                   Move      vabaas        i25aas
023100060126     c                   Eval      i25spe = %editc(vablnp:'X') +
023200060126     c                                      %editc(vabnrs:'X') +
023300060126     c                                      %editc(vabnsp:'X')
023400060126     c                   Call      'FNLS25R'
023500060126     c                   Parm                    Fnls25ds
023600060126
023700060126     c                   EndSr
023800980219      *---------------------------------------------------------------------*
023900980219      * *INZSR: Operazioni iniziali
024000980219      *---------------------------------------------------------------------*
024100960925     C     *INZSR        BEGSR
024200980219      *
024300020920      *  Reperisco parametri in entrata
024400020920     C     *ENTRY        PLIST
024500020920     C                   PARM                    KPJBA
024600090216     C                   PARM                    Commit_ON         1
024700090216      *
024800090216     c                   if        %PARMS < 2
024900090216     c                   eval      prmCmtCtl = *Off
025000090216     c                   else
025100090216     c                   eval      prmCmtCtl = *On
025200090216     c                   end
025300090216      *
025400090216      * a questo punto apre i files
025500090216     c                   IF        NOT %OPEN(EDRDE01L)
025600090216     c                   open      EDRDE01L
025700090216     c                   end
025800090216      *
025900090216     c                   IF        NOT %OPEN(EDiVAB1L)
026000090216     c                   open      EDiVAB1L
026100090216     c                   end
026200090216      *
026300121102     c                   IF        NOT %OPEN(EDiVAD1L)
026400121102     c                   open      EDiVAD1L
026500090216     c                   end
026600090216      *
026700121102     c                   IF        NOT %OPEN(EDiVAT1L)
026800121102     c                   open      EDiVAT1L
026900090216     c                   end
027000090216      *
027100121102     c                   IF        NOT %OPEN(EDiVAX1L)
027200121102     c                   open      EDiVAX1L
027300090216     c                   end
027400090216      *
027500090216     c                   IF        NOT %OPEN(FiVAB01L)
027600090216     c                   open      FiVAB01L
027700090216     c                   end
027800090216      *
027900090216     c                   IF        NOT %OPEN(FiVAD01L)
028000090216     c                   open      FiVAD01L
028100090216     c                   end
028200090216      *
028300090216     c                   IF        NOT %OPEN(FiVAT01L)
028400090216     c                   open      FiVAT01L
028500090216     c                   end
028600090216      *
028700090216     c                   IF        NOT %OPEN(FiVAX01L)
028800090216     c                   open      FiVAX01L
028900090216     c                   end
029000090216      *
029100020920     C                   MOVEL     KPJBU         TRTC86
029200020920      *
029300980219      * Chiave di accesso
029400100604     C     keytab        KLIST
029500100604     C                   KFLD                    CODUT
029600100604     C                   KFLD                    KCOD
029700100604     C                   KFLD                    KKEY
029800100604      *
029900040712     C     KVA_1         KLIST
030000960925     C                   KFLD                    KCCM
030100960925     C                   KFLD                    KCMR
030200960925     C                   KFLD                    KCNT
030300121102     C     KVA_C         KLIST
030400121102     C                   KFLD                    KFGS
030500121102     C                   KFLD                    KCCM
030600121102     C                   KFLD                    KCMR
030700121102     C                   KFLD                    KCNT
030800121102     C                   KFLD                    KAAS
030900121102     C                   KFLD                    KLNP
031000121102     C                   KFLD                    KNRS
031100121102     C                   KFLD                    KNSP
031200961021     C     KRDE          KLIST
031300961021     C                   KFLD                    KAAS
031400961021     C                   KFLD                    KLNP
031500961021     C                   KFLD                    KNRS
031600961021     C                   KFLD                    KNSP
031700961021     C     KSUM          KLIST
031800961021     C                   KFLD                    KCCM
031900961021     C                   KFLD                    KNFE
032000961021     C                   KFLD                    KFLG
032100961021     C                   KFLD                    KAAS
032200961021     C                   KFLD                    KLNP
032300961021     C                   KFLD                    KNRS
032400961021     C                   KFLD                    KNSP
032500041116
032600980219      * Definizione variabili
032700121102     C     *LIKE         DEFINE    VABFGS        KFGS
032800960925     C     *LIKE         DEFINE    VABCCM        KCCM
032900960925     C     *LIKE         DEFINE    VABCMR        KCMR
033000960925     C     *LIKE         DEFINE    VABCNT        KCNT
033100960925     C     *LIKE         DEFINE    VABAAS        KAAS
033200960925     C     *LIKE         DEFINE    VABLNP        KLNP
033300960925     C     *LIKE         DEFINE    VABNRS        KNRS
033400960925     C     *LIKE         DEFINE    VABNSP        KNSP
033500961021     C     *LIKE         DEFINE    SUMNFE        KNFE
033600961021     C     *LIKE         DEFINE    SUMFLG        KFLG
033700041116     C*  Richiamo XPARUT
033800041116     C                   Z-ADD     1             CODUT             1 0
033900041116     C                   CALL      'X§PARUT'
034000041116     C                   PARM                    UT§DSE
034100041116     C                   MOVEL     REC80         CNCR80
034200041116     c
034300041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
034400041116     C                   CLEAR                   TRUL06DS
034500041116     C                   MOVE      '£1'          D06COD
034600041116     C                   MOVEL     SIMFEL        D06KEY
034700041116     C                   MOVEL     'L'           D06TLA
034800041116     C                   MOVEL     TRUL06DS      KPJBU
034900041116      *
035000041116     C                   CALL      'TRUL06R'
035100041116     C                   PARM                    KPJBA
035200041116     C                   MOVEL     KPJBU         TRUL06DS
035300041116     C*
035400980219      *
035500980219      *  Se il cliente vuole il ritorno delle date di ARRIVO
035600980219      *  APRO EDSUM
035700961105     C     D86DTA        IFEQ      'S'
035800961024     C                   OPEN      EDSUM02L
035900040712     C                   ENDIF
036000980219      *
036100960925     C                   ENDSR
