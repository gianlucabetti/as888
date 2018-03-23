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
006900040712     C  N30              Z-ADD     VABAAS        KAAS
007000040712     C  N30              Z-ADD     VABLNP        KLNP
007100040712     C  N30              Z-ADD     VABNRS        KNRS
007200040712     C  N30              Z-ADD     VABNSP        KNSP
007300040712      *
007400020909      *  Se devo scrivere FiVAB procedo al posizionamento
007500040712     C     D86MOD        IFEQ      'E'
007600040712     C                   WRITE     FiVAB000
007700040712     C                   ELSE
007800980219     C                   EXSR      SUBDEL
007900040712     C                   ENDIF
008000060126
008100060126      * se bolla con "RC" ed è stato richiesto l'annullamento devo anche cancellare
008200060126      * l'ORM legato alla bolla
008300100604     c
008400100604     c                   clear                   ds7R
008500100604     c                   If        vabGma <>*blanks
008600100604     C                   eval      KCOD = '7R'
008700100604     C                   movel(P)  vabgma        KKEY
008800100604     C     keytab        chain     TABEL
008900100604     c                   IF        %found
009000100604     C                   movel     TBLUNI        ds7R
009100100604     c                   ENDIF
009200100604     c                   ENDIF
009300100604     c                   If        §7rRC='S'   and  d86mod = 'C'
009400060126     c                   ExSr      Delorm
009500060126     c                   EndIf
009600980219      *
009700040712      * Deleto bolla testata su EDIVAB
009800020909     C                   DELETE    EDiVAB00
009900041116     c                   endif
010000041116     c
010100040712      * Proseguo la lettura del file VAB (testate)
010200040712     C     KVA_1         READE     EDiVAB1L                               30
010300040712     C                   ENDDO
010400040712     C                   ENDIF
010500040712      *
010600040712      * Eseguo operazioni x estensione VAD
010700040712     C                   EXSR      SUBVAD
010800040712      *
010900040712      * Eseguo operazioni x estensione VAT
011000040712     C                   EXSR      SUBVAT
011100040712      *
011200040712      * Eseguo operazioni x estensione VAX
011300040712     C                   EXSR      SUBVAX
011400980219      *
011500980219      *  Controllo come devo chiudere il pgm
011600040712     C     D86CHI        IFEQ      'RT'
011700980219      *  Se devo chiudere con RT accendo indicatore
011800960925     C                   SETON                                        RT
011900040712     C                   ELSE
012000980219      *  Se devo chiudere con LR accendo indicatore LR
012100960925     C                   SETON                                        LR
012200040712     C                   ENDIF
012300980219      *---------------------------------------------------------------------*
012400040712      * SUBDEL: CANCELLO FILE APPOGGIO
012500980219      *---------------------------------------------------------------------*
012600980219     C     SUBDEL        BEGSR
012700980219      *
012800980219      * Loop delete EDRDE
012900980219     C     KRDE          DELETE    EDRDE01L                           33
0130009802193-   C     *IN33         DOWEQ     '0'
013100980219     C     KRDE          DELETE    EDRDE01L                           33
013200040712-3   C                   ENDDO
013300980219      * Loop delete EDSUM
013400980219     C                   Z-ADD     VABCCM        KCCM
013500980219     C                   Z-ADD     0             KNFE
013600980219     C                   MOVEL     *BLANKS       KFLG
0137009802193-   C     D86DTA        IFEQ      'S'
013800980219     C     KSUM          DELETE    EDSUM02L                           33
0139009802194-   C     *IN33         DOWEQ     '0'
014000980219     C     KSUM          DELETE    EDSUM02L                           33
014100040712-4   C                   ENDDO
014200040712-3   C                   ENDIF
014300980219      *
014400980219     C                   ENDSR
014500980219      *---------------------------------------------------------------------*
014600020909      * SUBVAD: ELABORO EDiVAD
014700980219      *---------------------------------------------------------------------*
014800980219     C     SUBVAD        BEGSR
014900980219      *
015000040712     C     KVA_1         CHAIN     EDiVAD2L                           34
015100980219      *
015200040712     C     *IN34         DOWEQ     '0'
015300041116     c     vadfgs        lookup    lin                                    50
015400041116     c                   if        *in50 and vadfgs>0
015500041214     c                             or D86ctl = 'N'
015600041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
015700041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
015800041214      * impostato a "N"
015900041214      *
016000980219     C     D86MOD        IFEQ      'E'
016100040712     C                   WRITE     FiVAD000
016200040712     C                   ENDIF
016300980219      *
016400020909     C                   DELETE    EDiVAD00
016500041116     c                   endif
016600980219      *
016700040712     C     KVA_1         READE     EDiVAD2L                               34
016800040712     C                   ENDDO
016900980219      *
017000980219     C                   ENDSR
017100980219      *---------------------------------------------------------------------*
017200020909      * SUBVAT: ELABORO EDiVAT
017300980219      *---------------------------------------------------------------------*
017400980219     C     SUBVAT        BEGSR
017500980219      *
017600020909      *  Scrivo FiVAT e cancello EDiVAT
017700040712     C     KVA_1         CHAIN     EDiVAT2L                           34
017800980219     C     *IN34         DOWEQ     '0'
017900041116     c     vatfgs        lookup    lin                                    50
018000041116     c                   if        *in50 and vatfgs> 0
018100041214     c                             or D86ctl = 'N'
018200041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
018300041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
018400041214      * impostato a "N"
018500980219      *
018600980219     C     D86MOD        IFEQ      'E'
018700020909     C                   WRITE     FiVAT000
018800040712     C                   ENDIF
018900980219      *
019000020909     C                   DELETE    EDiVAT00
019100041116     c                   endif
019200980219      *
019300040712     C     KVA_1         READE     EDiVAT2L                               34
019400040712     C                   ENDDO
019500040107      *
019600980219     C                   ENDSR
019700040712      *---------------------------------------------------------------------*
019800040712      * SUBVAX: ELABORO EDiVAX
019900040712      *---------------------------------------------------------------------*
020000040712     C     SUBVAX        BEGSR
020100040712      *
020200040712      *  Scrivo FiVAT e cancello EDiVAT
020300040712     C     KVA_1         CHAIN     EDiVAX2L                           34
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
021900040712     C     KVA_1         READE     EDiVAX2L                               34
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
026300090216     c                   IF        NOT %OPEN(EDiVAD2L)
026400090216     c                   open      EDiVAD2L
026500090216     c                   end
026600090216      *
026700090216     c                   IF        NOT %OPEN(EDiVAT2L)
026800090216     c                   open      EDiVAT2L
026900090216     c                   end
027000090216      *
027100090216     c                   IF        NOT %OPEN(EDiVAX2L)
027200090216     c                   open      EDiVAX2L
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
030300961021     C     KRDE          KLIST
030400961021     C                   KFLD                    KAAS
030500961021     C                   KFLD                    KLNP
030600961021     C                   KFLD                    KNRS
030700961021     C                   KFLD                    KNSP
030800961021     C     KSUM          KLIST
030900961021     C                   KFLD                    KCCM
031000961021     C                   KFLD                    KNFE
031100961021     C                   KFLD                    KFLG
031200961021     C                   KFLD                    KAAS
031300961021     C                   KFLD                    KLNP
031400961021     C                   KFLD                    KNRS
031500961021     C                   KFLD                    KNSP
031600041116
031700980219      * Definizione variabili
031800960925     C     *LIKE         DEFINE    VABCCM        KCCM
031900960925     C     *LIKE         DEFINE    VABCMR        KCMR
032000960925     C     *LIKE         DEFINE    VABCNT        KCNT
032100960925     C     *LIKE         DEFINE    VABAAS        KAAS
032200960925     C     *LIKE         DEFINE    VABLNP        KLNP
032300960925     C     *LIKE         DEFINE    VABNRS        KNRS
032400960925     C     *LIKE         DEFINE    VABNSP        KNSP
032500961021     C     *LIKE         DEFINE    SUMNFE        KNFE
032600961021     C     *LIKE         DEFINE    SUMFLG        KFLG
032700041116     C*  Richiamo XPARUT
032800041116     C                   Z-ADD     1             CODUT             1 0
032900041116     C                   CALL      'X§PARUT'
033000041116     C                   PARM                    UT§DSE
033100041116     C                   MOVEL     REC80         CNCR80
033200041116     c
033300041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
033400041116     C                   CLEAR                   TRUL06DS
033500041116     C                   MOVE      '£1'          D06COD
033600041116     C                   MOVEL     SIMFEL        D06KEY
033700041116     C                   MOVEL     'L'           D06TLA
033800041116     C                   MOVEL     TRUL06DS      KPJBU
033900041116      *
034000041116     C                   CALL      'TRUL06R'
034100041116     C                   PARM                    KPJBA
034200041116     C                   MOVEL     KPJBU         TRUL06DS
034300041116     C*
034400980219      *
034500980219      *  Se il cliente vuole il ritorno delle date di ARRIVO
034600980219      *  APRO EDSUM
034700961105     C     D86DTA        IFEQ      'S'
034800961024     C                   OPEN      EDSUM02L
034900040712     C                   ENDIF
035000980219      *
035100960925     C                   ENDSR
