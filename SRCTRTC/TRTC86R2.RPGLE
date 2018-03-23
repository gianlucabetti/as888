000100960329      ***********************************************************************
000200160520      *   Scrivo ARCHIVIO BOLLE DA WORK FILE
000300160520      *   oppure
000400160520      *   Cancello WORK FILE
000500160520      *
000600160520      * questo pgm viene richiamato per 2 motivi:
000700160520      * - D86MOD = E --> scrittura file bolle leggendo work file con chiave passata
000800160520      *                  e cancellazione rcd letto su work file
000900160520      * - D86MOD = C --> cancellazione rcd su work file con chiave passata
001000160520      * D86MOD può valere anche R mna vengono chiamati altri pgm perché è il raggruppamento
001100160520      *
001200960329      ***********************************************************************
001300960329     H DECEDIT('0,') DATEDIT(*DMY.)
001400980219      *---------------------------------------------------------------------*
001500980219      *  DATA BASE                                                          *
001600980219      *---------------------------------------------------------------------*
001700090216     FEDRDE01L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
001800980219      *---------
001900090216     FEDSUM02L  UF   E           K DISK    USROPN  COMMIT(prmCmtCtl)
002000980219      *---------
002100090216     FEDiVAB1L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
002200980219      *---------
002300121102     FEDiVAD1L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
002301171219      *---------
002302171219     FEDiVAD11L UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
002400980219      *---------
002500121102     FEDiVAT1L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
002600040709      *---------
002700121102     FEDiVAX1L  UF   E           K DISK    usropn  COMMIT(prmCmtCtl)
002800980219      *---------
002900090216     FFiVAB01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
003000980219      *---------
003100090216     FFiVAD01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
003101171219      *---------
003102171219     FFiVAD12L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
003200980219      *---------
003300090216     FFiVAT01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
003400040709      *---------
003500090216     FFiVAX01L  UF A E           K DISK    usropn  COMMIT(prmCmtCtl)
003600100604      *---------
003700100604     FTABEL00F  IF   E           K DISK
003800980219      *---------------------------------------------------------------------*
003900980219      * DS
004000980219      *---------------------------------------------------------------------*
004100980219      *
004200960329     D KPJBA         E DS
004300041116     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400041116     D CNCR80        E DS
004500100604     D ds7R          E DS
004600041116     d
004700041116     D TRUL06DS      E DS
004800041116     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
004900960925     D TRTC86        E DS                  EXTNAME(TRTC86DS)
005000060126     d Fnls25ds      e ds
005100100604     d
005200100604     D KCOD            S                   LIKE(TBLcod)
005300100604     D KKEY            S                   LIKE(TBLkey)
005400980219      *---------------------------------------------------------------------*
005500980219      * Ciclo principale
005600980219      *---------------------------------------------------------------------*
005700980219      *
005800040712      * Posizionamento iniziale x CMR
005900960925     C                   Z-ADD     D86CCM        KCCM
006000040712     C                   MOVEL     D86CMR        KCMR
006100960925     C                   Z-ADD     D86CNT        KCNT
006200160520     C*
006300160520     C* agisco a seconda della modalità richiesta
006400160520     C                   SELECT
006500160520     C                   WHEN      D86MOD = 'E'
006600160520     C                   EXSR      WriFIVA
006700160520     C                   WHEN      D86MOD = 'C'
006800160520     C                   EXSR      DltEDIVA
006900160520     C                   OTHER
007000160520     C* non è possibile essere richiamati con altra modalità
007100160520     C                   ENDSL
007200160520      *
007300160520      *  Controllo come devo chiudere il pgm
007400160520     C     D86CHI        IFEQ      'RT'
007500160520      *  Se devo chiudere con RT accendo indicatore
007600160520     C                   SETON                                        RT
007700160520     C                   ELSE
007800160520      *  Se devo chiudere con LR accendo indicatore LR
007900160520     C                   SETON                                        LR
008000160520     C                   ENDIF
008100160520      *---------------------------------------------------------------------*
008200160520      * WRIFIVA: Scrivo FIVA* (file bolle) leggendo EDIVA* (work file)
008300160520      *---------------------------------------------------------------------*
008400160520     C     WriFIVA       BEGSR
008500160520      *
008600040712      *
008700040712      * Elaborazione file VAB (testate)
008800040712     C     KVA_1         SETLL     EDiVAB1L                               30
008900040712     C                   IF        %equal(EDiVAB1L)
009000040712     C     KVA_1         READE     EDiVAB1L                               30
009100980219      *
009200040712     C     *IN30         DOWEQ     '0'
009300041116     c* elaboro solo se fgs  della £1 del p.o. utente
009400041116     c     vabfgs        lookup    lin                                    50
009500041214     c                   if        *in50  and vabfgs>0
009600041214     c                             or D86ctl = 'N'
009700041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
009800041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
009900041214      * impostato a "N"
010000040712      *
010100040712      * Imposto la chiave completa x spedizione
010200121102     C  N30              Z-ADD     VABFGS        KFGS
010300040712     C  N30              Z-ADD     VABAAS        KAAS
010400040712     C  N30              Z-ADD     VABLNP        KLNP
010500040712     C  N30              Z-ADD     VABNRS        KNRS
010600040712     C  N30              Z-ADD     VABNSP        KNSP
010700040712      *
010800040712     C                   WRITE     FiVAB000
010900121102     C*
011000121102     C* Eseguo operazioni x estensioni: VAD / VAT / VAX
011100121102     C                   EXSR      SUBVAD
011101171219     C                   EXSR      SUBVAD10
011200121102     C                   EXSR      SUBVAT
011300121102     C                   EXSR      SUBVAX
011400980219      *
011500040712      * Deleto bolla testata su EDIVAB
011600020909     C                   DELETE    EDiVAB00
011700041116     c                   endif
011800041116     c
011900040712      * Proseguo la lettura del file VAB (testate)
012000040712     C     KVA_1         READE     EDiVAB1L                               30
012100040712     C                   ENDDO
012200040712     C                   ENDIF
012300160520      *
012400160520     C                   ENDSR
012500980219      *---------------------------------------------------------------------*
012600020909      * SUBVAD: ELABORO EDiVAD
012700980219      *---------------------------------------------------------------------*
012800980219     C     SUBVAD        BEGSR
012900980219      *
013000121102     C     KVA_C         CHAIN     EDiVAD1L                           34
013100980219      *
013200040712     C     *IN34         DOWEQ     '0'
013300041116     c     vadfgs        lookup    lin                                    50
013400041116     c                   if        *in50 and vadfgs>0
013500041214     c                             or D86ctl = 'N'
013600041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
013700041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
013800041214      * impostato a "N"
013900041214      *
014000980219     C     D86MOD        IFEQ      'E'
014100040712     C                   WRITE     FiVAD000
014200040712     C                   ENDIF
014300980219      *
014400020909     C                   DELETE    EDiVAD00
014500041116     c                   endif
014600980219      *
014700121102     C     KVA_C         READE     EDiVAD1L                               34
014800040712     C                   ENDDO
014900980219      *
015000980219     C                   ENDSR
015001171219      *---------------------------------------------------------------------*
015002171219      * SUBVAD10: ELABORO EDiVAD10F
015003171219      *---------------------------------------------------------------------*
015004171219     C     SUBVAD10      BEGSR
015005171219      *
015006171219     C     KVA_C         CHAIN     EDiVAD11L                          34
015007171219      *
015008171219     C     *IN34         DOWEQ     '0'
015009171219     c     vadfgs        lookup    lin                                    50
015010171219     c                   if        *in50 and vadfgs>0
015011171219     c                             or D86ctl = 'N'
015012171219      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
015013171219      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
015014171219      * impostato a "N"
015015171219      *
015016171219     C     D86MOD        IFEQ      'E'
015017171219     C                   WRITE     FiVAD100
015018171219     C                   ENDIF
015019171219      *
015020171219     C                   DELETE    EDiVAD100
015021171219     c                   endif
015022171219      *
015023171219     C     KVA_C         READE     EDiVAD11L                              34
015024171219     C                   ENDDO
015025171219      *
015026171219     C                   ENDSR
015100980219      *---------------------------------------------------------------------*
015200020909      * SUBVAT: ELABORO EDiVAT
015300980219      *---------------------------------------------------------------------*
015400980219     C     SUBVAT        BEGSR
015500980219      *
015600020909      *  Scrivo FiVAT e cancello EDiVAT
015700121102     C     KVA_C         CHAIN     EDiVAT1L                           34
015800121102      *
015900980219     C     *IN34         DOWEQ     '0'
016000041116     c     vatfgs        lookup    lin                                    50
016100041116     c                   if        *in50 and vatfgs> 0
016200041214     c                             or D86ctl = 'N'
016300041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
016400041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
016500041214      * impostato a "N"
016600980219      *
016700980219     C     D86MOD        IFEQ      'E'
016800020909     C                   WRITE     FiVAT000
016900040712     C                   ENDIF
017000980219      *
017100020909     C                   DELETE    EDiVAT00
017200041116     c                   endif
017300980219      *
017400121102     C     KVA_C         READE     EDiVAT1L                               34
017500040712     C                   ENDDO
017600040107      *
017700980219     C                   ENDSR
017800040712      *---------------------------------------------------------------------*
017900040712      * SUBVAX: ELABORO EDiVAX
018000040712      *---------------------------------------------------------------------*
018100040712     C     SUBVAX        BEGSR
018200040712      *
018300040712      *  Scrivo FiVAT e cancello EDiVAT
018400121102     C     KVA_C         CHAIN     EDiVAX1L                           34
018500121102      *
018600040712     C     *IN34         DOWEQ     '0'
018700041116     c     vaxfgs        lookup    lin                                    50
018800041116     c                   if        *in50 and vaxfgs>0
018900041214     c                             or D86ctl = 'N'
019000041214      * se richiamato dal TRTC80R non deve eseguire il controllo e deve sempre
019100041214      * tradurre i dati dall'EDIVAB0F al FIVAB00F quindi riceve il nuovo parametro
019200041214      * impostato a "N"
019300040712      *
019400040712     C     D86MOD        IFEQ      'E'
019500040712     C                   WRITE     FiVAX000
019600040712     C                   ENDIF
019700040712      *
019800040712     C                   DELETE    EDiVAX00
019900041116     C                   ENDIF
020000040712      *
020100121102     C     KVA_C         READE     EDiVAX1L                               34
020200040712     C                   ENDDO
020300040712      *
020400040712     C                   ENDSR
020500160520      *---------------------------------------------------------------------*
020600160520      * DltEDIVA: Cancello EDIVA* (work file)
020700160520      *---------------------------------------------------------------------*
020800160520     C     DltEDIVA      BEGSR
020900160520      *
021000160520      * Elaborazione file VAB (testate)
021100160520     C     KVA_1         SETLL     EDiVAB1L                               30
021200160520 1   C                   IF        %equal(EDiVAB1L)
021300160520     C     KVA_1         READE     EDiVAB1L                               30
021400160520      *
021500160520 2   C     *IN30         DOWEQ     '0'
021600160520     c* elaboro solo se fgs  della £1 del p.o. utente
021700160520     c     vabfgs        lookup    lin                                    50
021800160520 3   c                   if        *in50  and vabfgs>0
021900160520     c                             or D86ctl = 'N'
022000160520      *
022100160520      * Imposto la chiave completa x spedizione
022200160520     C  N30              Z-ADD     VABFGS        KFGS
022300160520     C  N30              Z-ADD     VABAAS        KAAS
022400160520     C  N30              Z-ADD     VABLNP        KLNP
022500160520     C  N30              Z-ADD     VABNRS        KNRS
022600160520     C  N30              Z-ADD     VABNSP        KNSP
022700160520     C*
022800160520     C* Eseguo operazioni x estensioni: VAD / VAT / VAX
022900160520     C                   EXSR      DltEDIVAD
022901171219     C                   EXSR      DltEDIVAD10
023000160520     C                   EXSR      DltEDIVAT
023100160520     C                   EXSR      DltEDIVAX
023200160520      *
023300160520     C                   EXSR      SUBDEL
023400160520
023500160520      * se bolla con "RC" ed è stato richiesto l'annullamento devo anche cancellare
023600160520      * l'ORM legato alla bolla
023700160520     c
023800160520     c                   clear                   ds7R
023900160520 4   c                   If        vabGma <>*blanks
024000160520     C                   eval      KCOD = '7R'
024100160520     C                   movel(P)  vabgma        KKEY
024200160520     C     keytab        chain     TABEL
024300160520 5   c                   IF        %found
024400160520     C                   movel     TBLUNI        ds7R
024500160520 5-  c                   ENDIF
024600160520 4-  c                   ENDIF
024700160520 4   c                   If        §7rRC='S'   and  d86mod = 'C'
024800160520     c                   ExSr      Delorm
024900160520 4-  c                   EndIf
025000160520      *
025100160520      * Deleto bolla testata su EDIVAB
025200160520     C                   DELETE    EDiVAB00
025300160520 3-  c                   endif
025400160520     c
025500160520      * Proseguo la lettura del file VAB (testate)
025600160520     C     KVA_1         READE     EDiVAB1L                               30
025700160520 2-  C                   ENDDO
025800160520 1-  C                   ENDIF
025900160520     C*
026000160520     C                   ENDSR
026100160520      *---------------------------------------------------------------------*
026200160520      * SUBDEL: CANCELLO FILE APPOGGIO
026300160520      *---------------------------------------------------------------------*
026400160520     C     SUBDEL        BEGSR
026500160520      *
026600160520      * Loop delete EDRDE
026700160520     C     KRDE          DELETE    EDRDE01L                           33
0268001605203-   C     *IN33         DOWEQ     '0'
026900160520     C     KRDE          DELETE    EDRDE01L                           33
027000160520-3   C                   ENDDO
027100160520      * Loop delete EDSUM
027200160520     C                   Z-ADD     VABCCM        KCCM
027300160520     C                   Z-ADD     0             KNFE
027400160520     C                   MOVEL     *BLANKS       KFLG
0275001605203-   C     D86DTA        IFEQ      'S'
027600160520     C     KSUM          DELETE    EDSUM02L                           33
0277001605204-   C     *IN33         DOWEQ     '0'
027800160520     C     KSUM          DELETE    EDSUM02L                           33
027900160520-4   C                   ENDDO
028000160520-3   C                   ENDIF
028100160520      *
028200160520     C                   ENDSR
028300160520      *---------------------------------------------------------------------*
028400160520      * DltEDIVAD: cancello EDiVAD
028500160520      *---------------------------------------------------------------------*
028600160520     C     DltEDIVAD     BEGSR
028700160520      *
028800160520     C     KVA_C         CHAIN     EDiVAD1L                           34
028900160520      *
029000160520     C     *IN34         DOWEQ     '0'
029100160520     c     vadfgs        lookup    lin                                    50
029200160520     c                   if        *in50 and vadfgs>0
029300160520     c                             or D86ctl = 'N'
029400160520     C                   DELETE    EDiVAD00
029500160520     c                   endif
029600160520      *
029700160520     C     KVA_C         READE     EDiVAD1L                               34
029800160520     C                   ENDDO
029900160520      *
030000160520     C                   ENDSR
030001171219      *---------------------------------------------------------------------*
030002171219      * DltEDIVAD10: cancello EDiVAD10
030003171219      *---------------------------------------------------------------------*
030004171219     C     DltEDIVAD10   BEGSR
030005171219      *
030006171219     C     KVA_C         CHAIN     EDiVAD11L                          34
030007171219      *
030008171219     C     *IN34         DOWEQ     '0'
030009171219     c     vadfgs        lookup    lin                                    50
030010171219     c                   if        *in50 and vadfgs>0
030011171219     c                             or D86ctl = 'N'
030012171219     C                   DELETE    EDiVAD100
030013171219     c                   endif
030014171219      *
030015171219     C     KVA_C         READE     EDiVAD11L                              34
030016171219     C                   ENDDO
030017171219      *
030018171219     C                   ENDSR
030100160520      *---------------------------------------------------------------------*
030200160520      * DltEDIVAT: cancello EDiVAT
030300160520      *---------------------------------------------------------------------*
030400160520     C     DltEDIVAT     BEGSR
030500160520      *
030600160520      *  Scrivo FiVAT e cancello EDiVAT
030700160520     C     KVA_C         CHAIN     EDiVAT1L                           34
030800160520      *
030900160520     C     *IN34         DOWEQ     '0'
031000160520     c     vatfgs        lookup    lin                                    50
031100160520     c                   if        *in50 and vatfgs> 0
031200160520     c                             or D86ctl = 'N'
031300160520     C                   DELETE    EDiVAT00
031400160520     c                   endif
031500160520      *
031600160520     C     KVA_C         READE     EDiVAT1L                               34
031700160520     C                   ENDDO
031800160520      *
031900160520     C                   ENDSR
032000160520      *---------------------------------------------------------------------*
032100160520      * DltEDIVAX: cancello EDiVAX
032200160520      *---------------------------------------------------------------------*
032300160520     C     DltEDIVAX     BEGSR
032400160520      *
032500160520      *  Scrivo FiVAT e cancello EDiVAT
032600160520     C     KVA_C         CHAIN     EDiVAX1L                           34
032700160520      *
032800160520     C     *IN34         DOWEQ     '0'
032900160520     c     vaxfgs        lookup    lin                                    50
033000160520     c                   if        *in50 and vaxfgs>0
033100160520     c                             or D86ctl = 'N'
033200160520      *
033300160520     C                   DELETE    EDiVAX00
033400160520     C                   ENDIF
033500160520      *
033600160520     C     KVA_C         READE     EDiVAX1L                               34
033700160520     C                   ENDDO
033800160520      *
033900160520     C                   ENDSR
034000160520      *---------------------------------------------------------------------*
034100160520      * DELORM: Cancello l'ORM legato alla bolla RC
034200160520      *---------------------------------------------------------------------*
034300160520     c     Delorm        BegSr
034400160520
034500160520     c                   Clear                   fnls25ds
034600160520     c                   Eval      i25Ksc = vabccm
034700160520     c                   Move      vabaas        i25aas
034800160520     c                   Eval      i25spe = %editc(vablnp:'X') +
034900160520     c                                      %editc(vabnrs:'X') +
035000160520     c                                      %editc(vabnsp:'X')
035100160520     c                   Call      'FNLS25R'
035200160520     c                   Parm                    Fnls25ds
035300160520
035400160520     c                   EndSr
035500980219      *---------------------------------------------------------------------*
035600980219      * *INZSR: Operazioni iniziali
035700980219      *---------------------------------------------------------------------*
035800960925     C     *INZSR        BEGSR
035900980219      *
036000020920      *  Reperisco parametri in entrata
036100020920     C     *ENTRY        PLIST
036200020920     C                   PARM                    KPJBA
036300090216     C                   PARM                    Commit_ON         1
036400090216      *
036500090216     c                   if        %PARMS < 2
036600090216     c                   eval      prmCmtCtl = *Off
036700090216     c                   else
036800090216     c                   eval      prmCmtCtl = *On
036900090216     c                   end
037000090216      *
037100090216      * a questo punto apre i files
037200090216     c                   IF        NOT %OPEN(EDRDE01L)
037300090216     c                   open      EDRDE01L
037400090216     c                   end
037500090216      *
037600090216     c                   IF        NOT %OPEN(EDiVAB1L)
037700090216     c                   open      EDiVAB1L
037800090216     c                   end
037900090216      *
038000121102     c                   IF        NOT %OPEN(EDiVAD1L)
038100121102     c                   open      EDiVAD1L
038200090216     c                   end
038201171219      *
038202171219     c                   IF        NOT %OPEN(EDiVAD11L)
038203171219     c                   open      EDiVAD11L
038204171219     c                   end
038300090216      *
038400121102     c                   IF        NOT %OPEN(EDiVAT1L)
038500121102     c                   open      EDiVAT1L
038600090216     c                   end
038700090216      *
038800121102     c                   IF        NOT %OPEN(EDiVAX1L)
038900121102     c                   open      EDiVAX1L
039000090216     c                   end
039100090216      *
039200090216     c                   IF        NOT %OPEN(FiVAB01L)
039300090216     c                   open      FiVAB01L
039400090216     c                   end
039500090216      *
039600090216     c                   IF        NOT %OPEN(FiVAD01L)
039700090216     c                   open      FiVAD01L
039800090216     c                   end
039801171219      *
039802171219     c                   IF        NOT %OPEN(FiVAD12L)
039803171219     c                   open      FiVAD12L
039804171219     c                   end
039900090216      *
040000090216     c                   IF        NOT %OPEN(FiVAT01L)
040100090216     c                   open      FiVAT01L
040200090216     c                   end
040300090216      *
040400090216     c                   IF        NOT %OPEN(FiVAX01L)
040500090216     c                   open      FiVAX01L
040600090216     c                   end
040700090216      *
040800020920     C                   MOVEL     KPJBU         TRTC86
040900020920      *
041000980219      * Chiave di accesso
041100100604     C     keytab        KLIST
041200100604     C                   KFLD                    CODUT
041300100604     C                   KFLD                    KCOD
041400100604     C                   KFLD                    KKEY
041500100604      *
041600040712     C     KVA_1         KLIST
041700960925     C                   KFLD                    KCCM
041800960925     C                   KFLD                    KCMR
041900960925     C                   KFLD                    KCNT
042000121102     C     KVA_C         KLIST
042100121102     C                   KFLD                    KFGS
042200121102     C                   KFLD                    KCCM
042300121102     C                   KFLD                    KCMR
042400121102     C                   KFLD                    KCNT
042500121102     C                   KFLD                    KAAS
042600121102     C                   KFLD                    KLNP
042700121102     C                   KFLD                    KNRS
042800121102     C                   KFLD                    KNSP
042900961021     C     KRDE          KLIST
043000961021     C                   KFLD                    KAAS
043100961021     C                   KFLD                    KLNP
043200961021     C                   KFLD                    KNRS
043300961021     C                   KFLD                    KNSP
043400961021     C     KSUM          KLIST
043500961021     C                   KFLD                    KCCM
043600961021     C                   KFLD                    KNFE
043700961021     C                   KFLD                    KFLG
043800961021     C                   KFLD                    KAAS
043900961021     C                   KFLD                    KLNP
044000961021     C                   KFLD                    KNRS
044100961021     C                   KFLD                    KNSP
044200041116
044300980219      * Definizione variabili
044400121102     C     *LIKE         DEFINE    VABFGS        KFGS
044500960925     C     *LIKE         DEFINE    VABCCM        KCCM
044600960925     C     *LIKE         DEFINE    VABCMR        KCMR
044700960925     C     *LIKE         DEFINE    VABCNT        KCNT
044800960925     C     *LIKE         DEFINE    VABAAS        KAAS
044900960925     C     *LIKE         DEFINE    VABLNP        KLNP
045000960925     C     *LIKE         DEFINE    VABNRS        KNRS
045100960925     C     *LIKE         DEFINE    VABNSP        KNSP
045200961021     C     *LIKE         DEFINE    SUMNFE        KNFE
045300961021     C     *LIKE         DEFINE    SUMFLG        KFLG
045400041116     C*  Richiamo XPARUT
045500041116     C                   Z-ADD     1             CODUT             1 0
045600041116     C                   CALL      'X§PARUT'
045700041116     C                   PARM                    UT§DSE
045800041116     C                   MOVEL     REC80         CNCR80
045900041116     c
046000041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
046100041116     C                   CLEAR                   TRUL06DS
046200041116     C                   MOVE      '£1'          D06COD
046300041116     C                   MOVEL     SIMFEL        D06KEY
046400041116     C                   MOVEL     'L'           D06TLA
046500041116     C                   MOVEL     TRUL06DS      KPJBU
046600041116      *
046700041116     C                   CALL      'TRUL06R'
046800041116     C                   PARM                    KPJBA
046900041116     C                   MOVEL     KPJBU         TRUL06DS
047000041116     C*
047100980219      *
047200980219      *  Se il cliente vuole il ritorno delle date di ARRIVO
047300980219      *  APRO EDSUM
047400961105     C     D86DTA        IFEQ      'S'
047500961024     C                   OPEN      EDSUM02L
047600040712     C                   ENDIF
047700980219      *
047800960925     C                   ENDSR
