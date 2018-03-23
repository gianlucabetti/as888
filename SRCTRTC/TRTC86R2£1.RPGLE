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
001400090216     FEDiVAD2L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
001500980219      *---------
001600090216     FEDiVAT2L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
001700040709      *---------
001800090216     FEDiVAX2L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
001900980219      *---------
002000090216     FFiVAB01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
002100980219      *---------
002200090216     FFiVAD01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
002300980219      *---------
002400090216     FFiVAT01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
002500040709      *---------
002600090216     FFiVAX01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
002700980219      *---------------------------------------------------------------------*
002800980219      * DS
002900980219      *---------------------------------------------------------------------*
003000980219      *
003100960329     D KPJBA         E DS
003200041116     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003300041116     D CNCR80        E DS
003400041116     d
003500041116     D TRUL06DS      E DS
003600041116     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
003700960925     D TRTC86        E DS                  EXTNAME(TRTC86DS)
003800060126     d Fnls25ds      e ds
003900980219      *---------------------------------------------------------------------*
004000980219      * Ciclo principale
004100980219      *---------------------------------------------------------------------*
004200980219      *
004300040712      * Posizionamento iniziale x CMR
004400960925     C                   Z-ADD     D86CCM        KCCM
004500040712     C                   MOVEL     D86CMR        KCMR
004600960925     C                   Z-ADD     D86CNT        KCNT
004700040712      *
004800040712      * Elaborazione file VAB (testate)
004900040712     C     KVA_1         SETLL     EDiVAB1L                               30
005000040712     C                   IF        %equal(EDiVAB1L)
005100040712     C     KVA_1         READE     EDiVAB1L                               30
005200980219      *
005300040712     C     *IN30         DOWEQ     '0'
005400041116     c* elaboro solo se fgs  della £1 del p.o. utente
005500041116     c     vabfgs        lookup    lin                                    50
005600041214     c                   if        *in50  and vabfgs>0
005700041214     c                             or D86ctl = 'N'
005800041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
005900041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
006000041214      * impostato a "N"
006100040712      *
006200040712      * Imposto la chiave completa x spedizione
006300040712     C  N30              Z-ADD     VABAAS        KAAS
006400040712     C  N30              Z-ADD     VABLNP        KLNP
006500040712     C  N30              Z-ADD     VABNRS        KNRS
006600040712     C  N30              Z-ADD     VABNSP        KNSP
006700040712      *
006800020909      *  Se devo scrivere FiVAB procedo al posizionamento
006900040712     C     D86MOD        IFEQ      'E'
007000040712     C                   WRITE     FiVAB000
007100040712     C                   ELSE
007200980219     C                   EXSR      SUBDEL
007300040712     C                   ENDIF
007400060126
007500060126      * se bolla con "RC" ed è stato richiesto l'annullamento devo anche cancellare
007600060126      * l'ORM legato alla bolla
007700060126     c                   If        vabgma = 'RC' and d86mod = 'C'
007800060126     c                   ExSr      Delorm
007900060126     c                   EndIf
008000980219      *
008100040712      * Deleto bolla testata su EDIVAB
008200020909     C                   DELETE    EDiVAB00
008300041116     c                   endif
008400041116     c
008500040712      * Proseguo la lettura del file VAB (testate)
008600040712     C     KVA_1         READE     EDiVAB1L                               30
008700040712     C                   ENDDO
008800040712     C                   ENDIF
008900040712      *
009000040712      * Eseguo operazioni x estensione VAD
009100040712     C                   EXSR      SUBVAD
009200040712      *
009300040712      * Eseguo operazioni x estensione VAT
009400040712     C                   EXSR      SUBVAT
009500040712      *
009600040712      * Eseguo operazioni x estensione VAX
009700040712     C                   EXSR      SUBVAX
009800980219      *
009900980219      *  Controllo come devo chiudere il pgm
010000040712     C     D86CHI        IFEQ      'RT'
010100980219      *  Se devo chiudere con RT accendo indicatore
010200960925     C                   SETON                                        RT
010300040712     C                   ELSE
010400980219      *  Se devo chiudere con LR accendo indicatore LR
010500960925     C                   SETON                                        LR
010600040712     C                   ENDIF
010700980219      *---------------------------------------------------------------------*
010800040712      * SUBDEL: CANCELLO FILE APPOGGIO
010900980219      *---------------------------------------------------------------------*
011000980219     C     SUBDEL        BEGSR
011100980219      *
011200980219      * Loop delete EDRDE
011300980219     C     KRDE          DELETE    EDRDE01L                           33
0114009802193-   C     *IN33         DOWEQ     '0'
011500980219     C     KRDE          DELETE    EDRDE01L                           33
011600040712-3   C                   ENDDO
011700980219      * Loop delete EDSUM
011800980219     C                   Z-ADD     VABCCM        KCCM
011900980219     C                   Z-ADD     0             KNFE
012000980219     C                   MOVEL     *BLANKS       KFLG
0121009802193-   C     D86DTA        IFEQ      'S'
012200980219     C     KSUM          DELETE    EDSUM02L                           33
0123009802194-   C     *IN33         DOWEQ     '0'
012400980219     C     KSUM          DELETE    EDSUM02L                           33
012500040712-4   C                   ENDDO
012600040712-3   C                   ENDIF
012700980219      *
012800980219     C                   ENDSR
012900980219      *---------------------------------------------------------------------*
013000020909      * SUBVAD: ELABORO EDiVAD
013100980219      *---------------------------------------------------------------------*
013200980219     C     SUBVAD        BEGSR
013300980219      *
013400040712     C     KVA_1         CHAIN     EDiVAD2L                           34
013500980219      *
013600040712     C     *IN34         DOWEQ     '0'
013700041116     c     vadfgs        lookup    lin                                    50
013800041116     c                   if        *in50 and vadfgs>0
013900041214     c                             or D86ctl = 'N'
014000041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
014100041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
014200041214      * impostato a "N"
014300041214      *
014400980219     C     D86MOD        IFEQ      'E'
014500040712     C                   WRITE     FiVAD000
014600040712     C                   ENDIF
014700980219      *
014800020909     C                   DELETE    EDiVAD00
014900041116     c                   endif
015000980219      *
015100040712     C     KVA_1         READE     EDiVAD2L                               34
015200040712     C                   ENDDO
015300980219      *
015400980219     C                   ENDSR
015500980219      *---------------------------------------------------------------------*
015600020909      * SUBVAT: ELABORO EDiVAT
015700980219      *---------------------------------------------------------------------*
015800980219     C     SUBVAT        BEGSR
015900980219      *
016000020909      *  Scrivo FiVAT e cancello EDiVAT
016100040712     C     KVA_1         CHAIN     EDiVAT2L                           34
016200980219     C     *IN34         DOWEQ     '0'
016300041116     c     vatfgs        lookup    lin                                    50
016400041116     c                   if        *in50 and vatfgs> 0
016500041214     c                             or D86ctl = 'N'
016600041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
016700041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
016800041214      * impostato a "N"
016900980219      *
017000980219     C     D86MOD        IFEQ      'E'
017100020909     C                   WRITE     FiVAT000
017200040712     C                   ENDIF
017300980219      *
017400020909     C                   DELETE    EDiVAT00
017500041116     c                   endif
017600980219      *
017700040712     C     KVA_1         READE     EDiVAT2L                               34
017800040712     C                   ENDDO
017900040107      *
018000980219     C                   ENDSR
018100040712      *---------------------------------------------------------------------*
018200040712      * SUBVAX: ELABORO EDiVAX
018300040712      *---------------------------------------------------------------------*
018400040712     C     SUBVAX        BEGSR
018500040712      *
018600040712      *  Scrivo FiVAT e cancello EDiVAT
018700040712     C     KVA_1         CHAIN     EDiVAX2L                           34
018800040712     C     *IN34         DOWEQ     '0'
018900041116     c     vaxfgs        lookup    lin                                    50
019000041116     c                   if        *in50 and vaxfgs>0
019100041214     c                             or D86ctl = 'N'
019200041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
019300041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
019400041214      * impostato a "N"
019500040712      *
019600040712     C     D86MOD        IFEQ      'E'
019700040712     C                   WRITE     FiVAX000
019800040712     C                   ENDIF
019900040712      *
020000040712     C                   DELETE    EDiVAX00
020100041116     C                   ENDIF
020200040712      *
020300040712     C     KVA_1         READE     EDiVAX2L                               34
020400040712     C                   ENDDO
020500040712      *
020600040712     C                   ENDSR
020700060126      *---------------------------------------------------------------------*
020800060126      * DELORM: Cancello l'ORM legato alla bolla RC
020900060126      *---------------------------------------------------------------------*
021000060126     c     Delorm        BegSr
021100060126
021200060126     c                   Clear                   fnls25ds
021300060126     c                   Eval      i25Ksc = vabccm
021400060126     c                   Move      vabaas        i25aas
021500060126     c                   Eval      i25spe = %editc(vablnp:'X') +
021600060126     c                                      %editc(vabnrs:'X') +
021700060126     c                                      %editc(vabnsp:'X')
021800060126     c                   Call      'FNLS25R'
021900060126     c                   Parm                    Fnls25ds
022000060126
022100060126     c                   EndSr
022200980219      *---------------------------------------------------------------------*
022300980219      * *INZSR: Operazioni iniziali
022400980219      *---------------------------------------------------------------------*
022500960925     C     *INZSR        BEGSR
022600980219      *
022700020920      *  Reperisco parametri in entrata
022800020920     C     *ENTRY        PLIST
022900020920     C                   PARM                    KPJBA
023000090216     C                   PARM                    Commit_ON         1
023100090216      *
023200090216     c                   if        %PARMS < 2
023300090216     c                   eval      prmCmtCtl = *Off
023400090216     c                   else
023500090216     c                   eval      prmCmtCtl = *On
023600090216     c                   end
023700090216      *
023800090216      * a questo punto apre i files
023900090216     c                   IF        NOT %OPEN(EDRDE01L)
024000090216     c                   open      EDRDE01L
024100090216     c                   end
024200090216      *
024300090216     c                   IF        NOT %OPEN(EDiVAB1L)
024400090216     c                   open      EDiVAB1L
024500090216     c                   end
024600090216      *
024700090216     c                   IF        NOT %OPEN(EDiVAD2L)
024800090216     c                   open      EDiVAD2L
024900090216     c                   end
025000090216      *
025100090216     c                   IF        NOT %OPEN(EDiVAT2L)
025200090216     c                   open      EDiVAT2L
025300090216     c                   end
025400090216      *
025500090216     c                   IF        NOT %OPEN(EDiVAX2L)
025600090216     c                   open      EDiVAX2L
025700090216     c                   end
025800090216      *
025900090216     c                   IF        NOT %OPEN(FiVAB01L)
026000090216     c                   open      FiVAB01L
026100090216     c                   end
026200090216      *
026300090216     c                   IF        NOT %OPEN(FiVAD01L)
026400090216     c                   open      FiVAD01L
026500090216     c                   end
026600090216      *
026700090216     c                   IF        NOT %OPEN(FiVAT01L)
026800090216     c                   open      FiVAT01L
026900090216     c                   end
027000090216      *
027100090216     c                   IF        NOT %OPEN(FiVAX01L)
027200090216     c                   open      FiVAX01L
027300090216     c                   end
027400090216      *
027500020920     C                   MOVEL     KPJBU         TRTC86
027600020920      *
027700980219      * Chiave di accesso
027800040712     C     KVA_1         KLIST
027900960925     C                   KFLD                    KCCM
028000960925     C                   KFLD                    KCMR
028100960925     C                   KFLD                    KCNT
028200961021     C     KRDE          KLIST
028300961021     C                   KFLD                    KAAS
028400961021     C                   KFLD                    KLNP
028500961021     C                   KFLD                    KNRS
028600961021     C                   KFLD                    KNSP
028700961021     C     KSUM          KLIST
028800961021     C                   KFLD                    KCCM
028900961021     C                   KFLD                    KNFE
029000961021     C                   KFLD                    KFLG
029100961021     C                   KFLD                    KAAS
029200961021     C                   KFLD                    KLNP
029300961021     C                   KFLD                    KNRS
029400961021     C                   KFLD                    KNSP
029500041116
029600980219      * Definizione variabili
029700960925     C     *LIKE         DEFINE    VABCCM        KCCM
029800960925     C     *LIKE         DEFINE    VABCMR        KCMR
029900960925     C     *LIKE         DEFINE    VABCNT        KCNT
030000960925     C     *LIKE         DEFINE    VABAAS        KAAS
030100960925     C     *LIKE         DEFINE    VABLNP        KLNP
030200960925     C     *LIKE         DEFINE    VABNRS        KNRS
030300960925     C     *LIKE         DEFINE    VABNSP        KNSP
030400961021     C     *LIKE         DEFINE    SUMNFE        KNFE
030500961021     C     *LIKE         DEFINE    SUMFLG        KFLG
030600041116     C*  Richiamo XPARUT
030700041116     C                   Z-ADD     1             CODUT             1 0
030800041116     C                   CALL      'X§PARUT'
030900041116     C                   PARM                    UT§DSE
031000041116     C                   MOVEL     REC80         CNCR80
031100041116     c
031200041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
031300041116     C                   CLEAR                   TRUL06DS
031400041116     C                   MOVE      '£1'          D06COD
031500041116     C                   MOVEL     SIMFEL        D06KEY
031600041116     C                   MOVEL     'L'           D06TLA
031700041116     C                   MOVEL     TRUL06DS      KPJBU
031800041116      *
031900041116     C                   CALL      'TRUL06R'
032000041116     C                   PARM                    KPJBA
032100041116     C                   MOVEL     KPJBU         TRUL06DS
032200041116     C*
032300980219      *
032400980219      *  Se il cliente vuole il ritorno delle date di ARRIVO
032500980219      *  APRO EDSUM
032600961105     C     D86DTA        IFEQ      'S'
032700961024     C                   OPEN      EDSUM02L
032800040712     C                   ENDIF
032900980219      *
033000960925     C                   ENDSR
