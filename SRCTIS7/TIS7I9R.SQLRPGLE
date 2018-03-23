000100970526     H*------------------------------------------------------------------------*
000200991230     H* Cancellazione log Servizi Strategi
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800991230     Ftivlt03l  UF   E           K DISK
000900010226     Ftivlr03l  UF   E           K DISK
001000021127     Ftivin00s  UF   E           K DISK
001100040607     Ftivinsto  O  A E             DISK    RENAME(tivinsss:tivinVAS)
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500991230     D*---
001600991230     D* Variabili temporali
001700991230     D*---
001800991230     D                 DS
001900991230     D findata                         D   DATFMT(*ISO) INZ(D'1999-01-01')
002000991230     D*---
002100991230     D* DS ESTERNA PER REPERIMENTO PARAMETRI
002200991230     D*---
002300000612     D PARAM         E DS                  EXTNAME(DSGGLOG)
002400000331     D*---
002500970526     C*------------------------------------------------------------------------*
002600970526     C* MAIN LINES
002700970526     C*------------------------------------------------------------------------*
002800170119     C*
002900170119     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
003000170119     C
003100170119     C/EXEC SQL
003200170119     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
003300170119     C/END-EXEC
003400970526     C*
003500010226     C                   IF        §GGUPL <>999 AND
003600010226     C                             §GGUPLF<>999 AND
003700021127     C                             §GGUPLS<>999
003800010226     C                   EXSR      LETUPL                                       *pulizia log upload
003900010226     C                   ENDIF
004000000612     C                   IF        §GGDWL <>999 AND
004100021127     C                             §GGDWLF<>999
004200991230     C                   EXSR      LETDWL
004300991230     C                   ENDIF
004400970526     C* FINE PGM
004500970526     C                   SETON                                        LR
004600010226     C*------------------------------------------------------------------------*
004700010226     C* LETUPL - OPERAZIONI LOG UPLOAD (TIVLR00F)
004800010226     C*------------------------------------------------------------------------*
004900010226     C     LETUPL        BEGSR
005000010226     C*
005100010226     C* POSIZIONAMENTO E LETTURA/CANCELLAZIONE RECORD
005200010226     C                   Z-ADD     findata7      KLRdat
005300010226     C                   MOVEL     *hival        KLRora
005400010226     C                   MOVEL     *hival        KLRksc
005500010226     C     KEYvlr        SETLL     tivlr03l                           97
005600010226     C*
005700010226     C* LEGGE FINO A:
005800010226     C*     - FINE FILE
005900010226     C*     - TROVATO RECORD VALIDO
006000010226     C     *IN97         DOWEQ     *OFF
006100010226     C*
006200010226     C                   READ      tivlr03l                               97
006300010226     C*
006400010226     C     *IN97         IFEQ      *OFF                                         *FINE FILE
006500010226     C                   Z-ADD     vlrdat        depdat            8 0
006600010226     C                   Z-ADD     KLRdat        findat            8 0
006700010226     C                   MOVEL     vlrflg        depsts            1
006800010226     C*
006900010226     C     depdat        IFLE      findat
007000010226     C                   MOVEL     *blanks       POPZ
007100010226     C                   MOVEL     *blanks       PLIB
007200010226     C                   MOVEL     *blanks       POBJ
007300010226     C                   MOVEL     *blanks       PMBR
007400010226     C*
007500010226     C* 1° giro x file TRADOTTO
007600010226     C                   MOVEL     ' 4'          POPZ
007700010226     C                   MOVEL     vlrfou        POBJ
007800010226     C                   EVAL      PMBR='M'+%subst(vlrhdl:2:9)
007900060509     C                   CALL(e)   'TIS7I9C1'
008000010226     C                   PARM                    POPZ              2
008100010226     C                   PARM                    PLIB             10
008200010226     C                   PARM                    POBJ             10
008300010226     C                   PARM                    PMBR             10
008400010226     C*
008500021127     C* 2° giro x file STORICO
008600060509     C*                  MOVEL     ' 4'          POPZ
008700060509     C*                  MOVEL     vlrfis        POBJ
008800060509     C*                  EVAL      PMBR='M'+%subst(vlrhdl:2:9)
008900060509     C*                  CALL(e)   'TIS7I9C1'
009000060509     C*                  PARM                    POPZ              2
009100060509     C*                  PARM                    PLIB             10
009200060509     C*                  PARM                    POBJ             10
009300060509     C*                  PARM                    PMBR             10
009400010226     C                   IF        depdat<findata3
009500120521     C                   MOVEL     'FIL'         wISV              5
009600120521     C                   MOVE      vlrISV        wISV
009700120521     C
009800090827     C* Cancello anche file estensione ad esso correlati
009900090827     C* - TIVLRE => Tipo record "FILIE"
010000090827     C/EXEC SQL
010100090827     C+ DELETE FROM TIVLRE00F WHERE
010200120521     C+ VLRETRC = :wISV AND VLREPRG = :vlrPRG
010300090827     C/END-EXEC
010400090827     C*
010500090827     C                   DELETE    tivlr000
010600010226     C                   ENDIF
010700010226     C                   ENDIF
010800010226     C                   ENDIF
010900010226     C*
011000010226     C                   ENDDO
011100010226     C*
011200021016     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TIVIN00R
011300021016     C* rimuovendo i membri x data di creazione
011400021016     C                   MOVEL(P)  findata7      §DATA
011500021017     C                   CALL(e)   'TRULVINC'
011600021016     C                   PARM                    §DATA             8
011700021016     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
011800021202     C                   PARM      'TIVIN00R'    §FILE            10
011900021127     C*
012000021127     C* Come ultima pulizia x l'upload pulisco il file di storicizzazione TIVIN00S
012100021127     C                   READ      tivin00s
012200021127     C                   DOW       not %eof(tivin00s)
012300021127     C                   IF        vindat < findata9
012400040607     C                   WRITE     tivinVAS
012500040607     C                   DELETE    tivin00s
012600021127     C                   ENDIF
012700021127     C                   READ      tivin00s
012800021127     C                   ENDDO
012900021016     C*
013000010226     C                   ENDSR
013100000103     C*------------------------------------------------------------------------*
013200000103     C* LETDWL - OPERAZIONI LOG DOWNLOAD (TIVLT00F)
013300000103     C*------------------------------------------------------------------------*
013400000103     C     LETDWL        BEGSR
013500000103     C*
013600000103     C* POSIZIONAMENTO E LETTURA/CANCELLAZIONE RECORD
013700001115     C                   Z-ADD     findata2      KLTdat
013800000103     C                   MOVEL     *hival        KLTora
013900000103     C                   MOVEL     *hival        KLTksc
014000000103     C     KEYvlt        SETLL     tivlt03l                           98
014100000103     C*
014200000103     C* LEGGE FINO A:
014300000103     C*     - FINE FILE
014400000103     C*     - TROVATO RECORD VALIDO
014500000103     C     *IN98         DOWEQ     *OFF
014600000103     C*
014700000103     C                   READ      tivlt03l                               98
014800000103     C*
014900000103     C     *IN98         IFEQ      *OFF                                         *FINE FILE
015000010326     C*
015100050922     C*                  IF        vltsts='5' AND vltdat<findata2
015200050922     C*                  MOVEL     *blanks       POPZ
015300050922     C*                  MOVEL     *blanks       PLIB
015400050922     C*                  MOVEL     *blanks       POBJ
015500050922     C*                  MOVEL     *blanks       PMBR
015600000331     C*
015700010226     C* 1° giro x file TRADOTTO
015800010326     C*                  MOVEL     ' 4'          POPZ
015900010326     C*                  MOVEL     vltfld        POBJ
016000010326     C*                  MOVEL     vltmbd        PMBR
016100060509     C*                  CALL(e)   'TIS7I9C1'
016200010326     C*                  PARM                    POPZ              2
016300010326     C*                  PARM                    PLIB             10
016400010326     C*                  PARM                    POBJ             10
016500010326     C*                  PARM                    PMBR             10
016600010226     C*
016700010226     C* 2° giro x file STORICO
016800050922     C*                  MOVEL     ' 4'          POPZ
016900050922     C*                  MOVEL     vltfls        POBJ
017000050922     C*                  MOVEL     vltmbs        PMBR
017100060509     C*                  CALL(e)   'TIS7I9C1'
017200050922     C*                  PARM                    POPZ              2
017300050922     C*                  PARM                    PLIB             10
017400050922     C*                  PARM                    POBJ             10
017500050922     C*                  PARM                    PMBR             10
017600050922     C*                  ENDIF
017700010326     C*
017800010226     C                   IF        vltdat<findata8
017900000612     C                   MOVEL     *blanks       POPZ
018000000612     C                   MOVEL     *blanks       PLIB
018100000612     C                   MOVEL     *blanks       POBJ
018200000612     C                   MOVEL     *blanks       PMBR
018300000612     C*
018400010226     C* 1° giro x file TRADOTTO
018500010326     C*                  MOVEL     ' 4'          POPZ
018600010326     C*                  MOVEL     vltfld        POBJ
018700010326     C*                  MOVEL     vltmbd        PMBR
018800060509     C*                  CALL(e)   'TIS7I9C1'
018900010326     C*                  PARM                    POPZ              2
019000010326     C*                  PARM                    PLIB             10
019100010326     C*                  PARM                    POBJ             10
019200010326     C*                  PARM                    PMBR             10
019300010226     C*
019400010226     C* 2° giro x file STORICO
019500010226     C                   MOVEL     ' 4'          POPZ
019600010226     C                   MOVEL     vltfls        POBJ
019700010226     C                   MOVEL     vltmbs        PMBR
019800060509     C                   CALL(e)   'TIS7I9C1'
019900010226     C                   PARM                    POPZ              2
020000010226     C                   PARM                    PLIB             10
020100010226     C                   PARM                    POBJ             10
020200010226     C                   PARM                    PMBR             10
020300050922     C*
020400101007     C* Cancello anche file estensione ad esso correlati
020500101007     C* - TIVLTE => Tipo record "NAMEF"
020600101007     C/EXEC SQL
020700101007     C+ DELETE FROM TIVLTE00F WHERE
020800101007     C+ VLTETRC = 'NAMEF' AND VLTEPRG = :vltPRG
020900101007     C/END-EXEC
021000050922     C                   ENDIF
021100050922     C
021200050922     C                   IF        vltdat<findata2
021300000103     C                   DELETE    tivlt000
021400000103     C                   ENDIF
021500050922     C                   ENDIF
021600000103     C*
021700000103     C                   ENDDO
021800060509     C*
021900060509     C* 3° giro x file generico download (TIVGD00F)
022000060509     C/EXEC SQL
022100060509     C+ DELETE FROM TIVGD00F WHERE
022200060509     C+ VGDDAT < :findata8
022300060509     C/END-EXEC
022400160822     C*
022500160822     C* 5° giro fuori ciclo x file generico estensioni di dettaglio varie
022600160822     C/EXEC SQL
022700160822     C+ DELETE FROM TIVLRE10F WHERE
022800160822     C+ dec(concat(substr(VLRE1MOMNT, 1, 4),
022900160822     C+     concat(substr(VLRE1MOMNT, 6, 2),
023000160822     C+            substr(VLRE1MOMNT, 9, 2))), 8, 0) < :findata10
023100160822     C/END-EXEC
023200060509     C*
023300060509     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAC00S
023400060509     C* rimuovendo i membri x data di creazione
023500060509     C                   MOVEL(P)  findata7      §DATA
023600060509     C                   CALL(e)   'TRULVINC'
023700060509     C                   PARM                    §DATA             8
023800060509     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
023900060509     C                   PARM      'FNVAC00S'    §FILE            10
024000060509     C*
024100060509     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAB00S
024200060509     C* rimuovendo i membri x data di creazione
024300060509     C                   MOVEL(P)  findata7      §DATA
024400060509     C                   CALL(e)   'TRULVINC'
024500060509     C                   PARM                    §DATA             8
024600060509     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
024700060509     C                   PARM      'FNVAB00S'    §FILE            10
024800160822     C*
024900160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAG00S
025000160822     C* rimuovendo i membri x data di creazione
025100160822     C                   MOVEL(P)  findata7      §DATA
025200160822     C                   CALL(e)   'TRULVINC'
025300160822     C                   PARM                    §DATA             8
025400160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
025500160822     C                   PARM      'FNVAG00S'    §FILE            10
025600160822     C*
025700160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAP00S
025800160822     C* rimuovendo i membri x data di creazione
025900160822     C                   MOVEL(P)  findata7      §DATA
026000160822     C                   CALL(e)   'TRULVINC'
026100160822     C                   PARM                    §DATA             8
026200160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
026300160822     C                   PARM      'FNVAP00S'    §FILE            10
026400160822     C*
026500160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TIVAF00S
026600160822     C* rimuovendo i membri x data di creazione
026700160822     C                   MOVEL(P)  findata7      §DATA
026800160822     C                   CALL(e)   'TRULVINC'
026900160822     C                   PARM                    §DATA             8
027000160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
027100160822     C                   PARM      'TIVAF00S'    §FILE            10
027200160822     C*
027300160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TNVAS00S
027400160822     C* rimuovendo i membri x data di creazione
027500160822     C                   MOVEL(P)  findata7      §DATA
027600160822     C                   CALL(e)   'TRULVINC'
027700160822     C                   PARM                    §DATA             8
027800160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
027900160822     C                   PARM      'TNVAS00S'    §FILE            10
028000160822     C*
028100160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TIVAFWWT
028200160822     C* rimuovendo i membri x data di creazione
028300160822     C                   MOVEL(P)  findata7      §DATA
028400160822     C                   CALL(e)   'TRULVINC'
028500160822     C                   PARM                    §DATA             8
028600160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
028700160822     C                   PARM      'TIVAFWWT'    §FILE            10
028800160822     C*
028900160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TNVASWWT
029000160822     C* rimuovendo i membri x data di creazione
029100160822     C                   MOVEL(P)  findata7      §DATA
029200160822     C                   CALL(e)   'TRULVINC'
029300160822     C                   PARM                    §DATA             8
029400160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
029500160822     C                   PARM      'TNVASWWT'    §FILE            10
029600160822     C*
029700160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TIVAWWWT
029800160822     C* rimuovendo i membri x data di creazione
029900160822     C                   MOVEL(P)  findata7      §DATA
030000160822     C                   CALL(e)   'TRULVINC'
030100160822     C                   PARM                    §DATA             8
030200160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
030300160822     C                   PARM      'TIVAWWWT'    §FILE            10
030400160822     C*
030500160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAPWWT
030600160822     C* rimuovendo i membri x data di creazione
030700160822     C                   MOVEL(P)  findata7      §DATA
030800160822     C                   CALL(e)   'TRULVINC'
030900160822     C                   PARM                    §DATA             8
031000160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
031100160822     C                   PARM      'FNVAPWWT'    §FILE            10
031200160822     C*
031300160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAGWWT
031400160822     C* rimuovendo i membri x data di creazione
031500160822     C                   MOVEL(P)  findata7      §DATA
031600160822     C                   CALL(e)   'TRULVINC'
031700160822     C                   PARM                    §DATA             8
031800160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
031900160822     C                   PARM      'FNVAGWWT'    §FILE            10
032000160822     C*
032100160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVACWWT
032200160822     C* rimuovendo i membri x data di creazione
032300160822     C                   MOVEL(P)  findata7      §DATA
032400160822     C                   CALL(e)   'TRULVINC'
032500160822     C                   PARM                    §DATA             8
032600160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
032700160822     C                   PARM      'FNVACWWT'    §FILE            10
032800160822     C*
032900160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVAIWWT
033000160822     C* rimuovendo i membri x data di creazione
033100160822     C                   MOVEL(P)  findata7      §DATA
033200160822     C                   CALL(e)   'TRULVINC'
033300160822     C                   PARM                    §DATA             8
033400160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
033500160822     C                   PARM      'FIVAIWWT'    §FILE            10
033600160822     C*
033700160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVABWWR
033800160822     C* rimuovendo i membri x data di creazione
033900160822     C                   MOVEL(P)  findata7      §DATA
034000160822     C                   CALL(e)   'TRULVINC'
034100160822     C                   PARM                    §DATA             8
034200160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
034300160822     C                   PARM      'FIVABWWR'    §FILE            10
034400160822     C*
034500160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVADWWR
034600160822     C* rimuovendo i membri x data di creazione
034700160822     C                   MOVEL(P)  findata7      §DATA
034800160822     C                   CALL(e)   'TRULVINC'
034900160822     C                   PARM                    §DATA             8
035000160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
035100160822     C                   PARM      'FIVADWWR'    §FILE            10
035200160822     C*
035300160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVATWWR
035400160822     C* rimuovendo i membri x data di creazione
035500160822     C                   MOVEL(P)  findata7      §DATA
035600160822     C                   CALL(e)   'TRULVINC'
035700160822     C                   PARM                    §DATA             8
035800160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
035900160822     C                   PARM      'FIVATWWR'    §FILE            10
036000160822     C*
036100160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVAXWWR
036200160822     C* rimuovendo i membri x data di creazione
036300160822     C                   MOVEL(P)  findata7      §DATA
036400160822     C                   CALL(e)   'TRULVINC'
036500160822     C                   PARM                    §DATA             8
036600160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
036700160822     C                   PARM      'FIVAXWWR'    §FILE            10
036800160822     C*
036900160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVABWWR
037000160822     C* rimuovendo i membri x data di creazione
037100160822     C                   MOVEL(P)  findata7      §DATA
037200160822     C                   CALL(e)   'TRULVINC'
037300160822     C                   PARM                    §DATA             8
037400160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
037500160822     C                   PARM      'FNVABWWR'    §FILE            10
037600160822     C*
037700160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVADWWR
037800160822     C* rimuovendo i membri x data di creazione
037900160822     C                   MOVEL(P)  findata7      §DATA
038000160822     C                   CALL(e)   'TRULVINC'
038100160822     C                   PARM                    §DATA             8
038200160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
038300160822     C                   PARM      'FNVADWWR'    §FILE            10
038400160822     C*
038500160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAOWWR
038600160822     C* rimuovendo i membri x data di creazione
038700160822     C                   MOVEL(P)  findata7      §DATA
038800160822     C                   CALL(e)   'TRULVINC'
038900160822     C                   PARM                    §DATA             8
039000160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
039100160822     C                   PARM      'FNVAOWWR'    §FILE            10
039200160822     C*
039300160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVARWWR
039400160822     C* rimuovendo i membri x data di creazione
039500160822     C                   MOVEL(P)  findata7      §DATA
039600160822     C                   CALL(e)   'TRULVINC'
039700160822     C                   PARM                    §DATA             8
039800160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
039900160822     C                   PARM      'FNVARWWR'    §FILE            10
040000160822     C*
040100160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVATWWR
040200160822     C* rimuovendo i membri x data di creazione
040300160822     C                   MOVEL(P)  findata7      §DATA
040400160822     C                   CALL(e)   'TRULVINC'
040500160822     C                   PARM                    §DATA             8
040600160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
040700160822     C                   PARM      'FNVATWWR'    §FILE            10
040800060509     C*
040900000103     C                   ENDSR
041000970526     C*------------------------------------------------------------------------*
041100970526     C* *INZSR - OPERAZIONI INIZIALI
041200970526     C*------------------------------------------------------------------------*
041300970526     C     *INZSR        BEGSR
041400991230     C*
041500991230     C*---
041600991230     C* VARIABILI RIFERITE AL DATA BASE
041700991230     C*---
041800010226     C     *LIKE         DEFINE    vlrdat        KLRdat
041900010226     C     *LIKE         DEFINE    vlrora        KLRora
042000010226     C     *LIKE         DEFINE    vlrksc        KLRksc
042100991230     C     *LIKE         DEFINE    vltdat        KLTdat
042200991230     C     *LIKE         DEFINE    vltora        KLTora
042300991230     C     *LIKE         DEFINE    vltksc        KLTksc
042400970526     C*---
042500970526     C* VARIABILI DI CONTROLLO
042600970526     C*---
042700010226     C*
042800970526     C*---
042900970526     C* CHIAVI DI LETTURA
043000970526     C*---
043100991230     C* LETTURA tivlt03L -
043200991230     C     KEYvlt        KLIST
043300991230     C                   KFLD                    KLTdat                         *PARTENZA/ARRIVO
043400991230     C                   KFLD                    KLTora                         *PARTENZA/ARRIVO
043500991230     C                   KFLD                    KLTksc                         *PARTENZA/ARRIVO
043600010226     C* LETTURA tivlrs3L -
043700010226     C     KEYvlr        KLIST
043800010226     C                   KFLD                    KLRdat                         *PARTENZA/ARRIVO
043900010226     C                   KFLD                    KLRora                         *PARTENZA/ARRIVO
044000010226     C                   KFLD                    KLRksc                         *PARTENZA/ARRIVO
044100991230     C*
044200991230     C* REPERIMENTO PARAMETRI DA DTAARA
044300991230     C     *DTAARA       DEFINE    TIS7I9A       PARAM
044400991230     C                   IN        PARAM
044500970526     C*
044600970526     C* CALCOLA LA DATA CORRENTE
044700160822     C                   Z-ADD     *zeros        DATCOR            8 0
044800160822     C                   EVAL      DATCOR = %dec(%date() : *ISO)
044900010226     C*
045000010226     C     *ISO          MOVE      DATCOR        findata
045100010226     C     findata       SUBDUR    §GGDWL:*D     findata
045200010226     C     *ISO          MOVE      findata       findata2          8 0
045300010226     C*
045400010226     C     *ISO          MOVE      DATCOR        findata
045500010226     C     findata       SUBDUR    §GGUPL:*D     findata
045600010226     C     *ISO          MOVE      findata       findata3          8 0
045700010226     C*
045800010226     C     *ISO          MOVE      DATCOR        findata
045900010226     C     findata       SUBDUR    §GGUPLF:*D    findata
046000010226     C     *ISO          MOVE      findata       findata7          8 0
046100000612     C*
046200000612     C     *ISO          MOVE      DATCOR        findata
046300000612     C     findata       SUBDUR    §GGDWLF:*D    findata
046400010226     C     *ISO          MOVE      findata       findata8          8 0
046500021127     C*
046600021127     C     *ISO          MOVE      DATCOR        findata
046700021127     C     findata       SUBDUR    §GGUPLS:*D    findata
046800021127     C     *ISO          MOVE      findata       findata9          8 0
046900160822     C*
047000160822     C     *ISO          MOVE      DATCOR        findata
047100160822     C     findata       SUBDUR    90:*D         findata
047200160822     C     *ISO          MOVE      findata       findata10         8 0
047300991230     C*
047400991230     C                   ENDSR
