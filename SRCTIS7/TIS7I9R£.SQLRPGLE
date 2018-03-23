000100050922      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300991230     H* Cancellazione log Servizi Strategi
000400970526     H*------------------------------------------------------------------------*
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900991230     Ftivlt03l  UF   E           K DISK
001000010226     Ftivlr03l  UF   E           K DISK
001100021127     Ftivin00s  UF   E           K DISK
001200040607     Ftivinsto  O  A E             DISK    RENAME(tivinsss:tivinVAS)
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600991230     D*---
001700991230     D* Variabili temporali
001800991230     D*---
001900991230     D                 DS
002000991230     D findata                         D   DATFMT(*ISO) INZ(D'1999-01-01')
002100991230     D*---
002200991230     D* DS ESTERNA PER REPERIMENTO PARAMETRI
002300991230     D*---
002400000612     D PARAM         E DS                  EXTNAME(DSGGLOG)
002500000331     D*---
002600970526     C*------------------------------------------------------------------------*
002700970526     C* MAIN LINES
002800970526     C*------------------------------------------------------------------------*
002900970526     C*
003000010226     C                   IF        §GGUPL <>999 AND
003100010226     C                             §GGUPLF<>999 AND
003200021127     C                             §GGUPLS<>999
003300010226     C                   EXSR      LETUPL                                       *pulizia log upload
003400010226     C                   ENDIF
003500000612     C                   IF        §GGDWL <>999 AND
003600021127     C                             §GGDWLF<>999
003700991230     C                   EXSR      LETDWL
003800991230     C                   ENDIF
003900970526     C* FINE PGM
004000970526     C                   SETON                                        LR
004100010226     C*------------------------------------------------------------------------*
004200010226     C* LETUPL - OPERAZIONI LOG UPLOAD (TIVLR00F)
004300010226     C*------------------------------------------------------------------------*
004400010226     C     LETUPL        BEGSR
004500010226     C*
004600010226     C* POSIZIONAMENTO E LETTURA/CANCELLAZIONE RECORD
004700010226     C                   Z-ADD     findata7      KLRdat
004800010226     C                   MOVEL     *hival        KLRora
004900010226     C                   MOVEL     *hival        KLRksc
005000010226     C     KEYvlr        SETLL     tivlr03l                           97
005100010226     C*
005200010226     C* LEGGE FINO A:
005300010226     C*     - FINE FILE
005400010226     C*     - TROVATO RECORD VALIDO
005500010226     C     *IN97         DOWEQ     *OFF
005600010226     C*
005700010226     C                   READ      tivlr03l                               97
005800010226     C*
005900010226     C     *IN97         IFEQ      *OFF                                         *FINE FILE
006000010226     C                   Z-ADD     vlrdat        depdat            8 0
006100010226     C                   Z-ADD     KLRdat        findat            8 0
006200010226     C                   MOVEL     vlrflg        depsts            1
006300010226     C*
006400010226     C     depdat        IFLE      findat
006500010226     C                   MOVEL     *blanks       POPZ
006600010226     C                   MOVEL     *blanks       PLIB
006700010226     C                   MOVEL     *blanks       POBJ
006800010226     C                   MOVEL     *blanks       PMBR
006900010226     C*
007000010226     C* 1° giro x file TRADOTTO
007100010226     C                   MOVEL     ' 4'          POPZ
007200010226     C                   MOVEL     vlrfou        POBJ
007300010226     C                   EVAL      PMBR='M'+%subst(vlrhdl:2:9)
007400060509     C                   CALL(e)   'TIS7I9C1'
007500010226     C                   PARM                    POPZ              2
007600010226     C                   PARM                    PLIB             10
007700010226     C                   PARM                    POBJ             10
007800010226     C                   PARM                    PMBR             10
007900010226     C*
008000021127     C* 2° giro x file STORICO
008100060509     C*                  MOVEL     ' 4'          POPZ
008200060509     C*                  MOVEL     vlrfis        POBJ
008300060509     C*                  EVAL      PMBR='M'+%subst(vlrhdl:2:9)
008400060509     C*                  CALL(e)   'TIS7I9C1'
008500060509     C*                  PARM                    POPZ              2
008600060509     C*                  PARM                    PLIB             10
008700060509     C*                  PARM                    POBJ             10
008800060509     C*                  PARM                    PMBR             10
008900010226     C                   IF        depdat<findata3
009000120521     C                   MOVEL     'FIL'         wISV              5
009100120521     C                   MOVE      vlrISV        wISV
009200120521     C
009300090827     C* Cancello anche file estensione ad esso correlati
009400090827     C* - TIVLRE => Tipo record "FILIE"
009500090827     C/EXEC SQL
009600090827     C+ DELETE FROM TIVLRE00F WHERE
009700120521     C+ VLRETRC = :wISV AND VLREPRG = :vlrPRG
009800090827     C/END-EXEC
009900090827     C*
010000090827     C                   DELETE    tivlr000
010100010226     C                   ENDIF
010200010226     C                   ENDIF
010300010226     C                   ENDIF
010400010226     C*
010500010226     C                   ENDDO
010600010226     C*
010700021016     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TIVIN00R
010800021016     C* rimuovendo i membri x data di creazione
010900021016     C                   MOVEL(P)  findata7      §DATA
011000021017     C                   CALL(e)   'TRULVINC'
011100021016     C                   PARM                    §DATA             8
011200021016     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
011300021202     C                   PARM      'TIVIN00R'    §FILE            10
011400021127     C*
011500021127     C* Come ultima pulizia x l'upload pulisco il file di storicizzazione TIVIN00S
011600021127     C                   READ      tivin00s
011700021127     C                   DOW       not %eof(tivin00s)
011800021127     C                   IF        vindat < findata9
011900040607     C                   WRITE     tivinVAS
012000040607     C                   DELETE    tivin00s
012100021127     C                   ENDIF
012200021127     C                   READ      tivin00s
012300021127     C                   ENDDO
012400021016     C*
012500010226     C                   ENDSR
012600000103     C*------------------------------------------------------------------------*
012700000103     C* LETDWL - OPERAZIONI LOG DOWNLOAD (TIVLT00F)
012800000103     C*------------------------------------------------------------------------*
012900000103     C     LETDWL        BEGSR
013000000103     C*
013100000103     C* POSIZIONAMENTO E LETTURA/CANCELLAZIONE RECORD
013200001115     C                   Z-ADD     findata2      KLTdat
013300000103     C                   MOVEL     *hival        KLTora
013400000103     C                   MOVEL     *hival        KLTksc
013500000103     C     KEYvlt        SETLL     tivlt03l                           98
013600000103     C*
013700000103     C* LEGGE FINO A:
013800000103     C*     - FINE FILE
013900000103     C*     - TROVATO RECORD VALIDO
014000000103     C     *IN98         DOWEQ     *OFF
014100000103     C*
014200000103     C                   READ      tivlt03l                               98
014300000103     C*
014400000103     C     *IN98         IFEQ      *OFF                                         *FINE FILE
014500010326     C*
014600050922     C*                  IF        vltsts='5' AND vltdat<findata2
014700050922     C*                  MOVEL     *blanks       POPZ
014800050922     C*                  MOVEL     *blanks       PLIB
014900050922     C*                  MOVEL     *blanks       POBJ
015000050922     C*                  MOVEL     *blanks       PMBR
015100000331     C*
015200010226     C* 1° giro x file TRADOTTO
015300010326     C*                  MOVEL     ' 4'          POPZ
015400010326     C*                  MOVEL     vltfld        POBJ
015500010326     C*                  MOVEL     vltmbd        PMBR
015600060509     C*                  CALL(e)   'TIS7I9C1'
015700010326     C*                  PARM                    POPZ              2
015800010326     C*                  PARM                    PLIB             10
015900010326     C*                  PARM                    POBJ             10
016000010326     C*                  PARM                    PMBR             10
016100010226     C*
016200010226     C* 2° giro x file STORICO
016300050922     C*                  MOVEL     ' 4'          POPZ
016400050922     C*                  MOVEL     vltfls        POBJ
016500050922     C*                  MOVEL     vltmbs        PMBR
016600060509     C*                  CALL(e)   'TIS7I9C1'
016700050922     C*                  PARM                    POPZ              2
016800050922     C*                  PARM                    PLIB             10
016900050922     C*                  PARM                    POBJ             10
017000050922     C*                  PARM                    PMBR             10
017100050922     C*                  ENDIF
017200010326     C*
017300010226     C                   IF        vltdat<findata8
017400000612     C                   MOVEL     *blanks       POPZ
017500000612     C                   MOVEL     *blanks       PLIB
017600000612     C                   MOVEL     *blanks       POBJ
017700000612     C                   MOVEL     *blanks       PMBR
017800000612     C*
017900010226     C* 1° giro x file TRADOTTO
018000010326     C*                  MOVEL     ' 4'          POPZ
018100010326     C*                  MOVEL     vltfld        POBJ
018200010326     C*                  MOVEL     vltmbd        PMBR
018300060509     C*                  CALL(e)   'TIS7I9C1'
018400010326     C*                  PARM                    POPZ              2
018500010326     C*                  PARM                    PLIB             10
018600010326     C*                  PARM                    POBJ             10
018700010326     C*                  PARM                    PMBR             10
018800010226     C*
018900010226     C* 2° giro x file STORICO
019000010226     C                   MOVEL     ' 4'          POPZ
019100010226     C                   MOVEL     vltfls        POBJ
019200010226     C                   MOVEL     vltmbs        PMBR
019300060509     C                   CALL(e)   'TIS7I9C1'
019400010226     C                   PARM                    POPZ              2
019500010226     C                   PARM                    PLIB             10
019600010226     C                   PARM                    POBJ             10
019700010226     C                   PARM                    PMBR             10
019800050922     C*
019900050922     C* 3° giro x file generico download (TIVGD00F)
020000060509     C*/EXEC SQL
020100060509     C*+ DELETE FROM TIVGD00F WHERE
020200060509     C*+ VGDKSU = :vltksu AND VGDTIP = :vlttip AND
020300060509     C*+ VGDPRG = :vltprg
020400060509     C*/END-EXEC
020500000612     C*
020600101007     C* Cancello anche file estensione ad esso correlati
020700101007     C* - TIVLTE => Tipo record "NAMEF"
020800101007     C/EXEC SQL
020900101007     C+ DELETE FROM TIVLTE00F WHERE
021000101007     C+ VLTETRC = 'NAMEF' AND VLTEPRG = :vltPRG
021100101007     C/END-EXEC
021200050922     C                   ENDIF
021300050922     C
021400050922     C                   IF        vltdat<findata2
021500000103     C                   DELETE    tivlt000
021600000103     C                   ENDIF
021700050922     C                   ENDIF
021800000103     C*
021900000103     C                   ENDDO
022000060509     C*
022100060509     C* 3° giro x file generico download (TIVGD00F)
022200060509     C/EXEC SQL
022300060509     C+ DELETE FROM TIVGD00F WHERE
022400060509     C+ VGDDAT < :findata8
022500060509     C/END-EXEC
022600050922     C*
022700050922     C* 4° giro fuori ciclo x file generico download (TIVGD00F) x clienti NN abilitati
022800060509     C*/EXEC SQL
022900060509     C*+ DELETE FROM TIVGD00F WHERE
023000120105     C*+ VGDSTO NOT IN (' ', '*', 'S') AND VGDDAT < :findata8
023100060509     C*/END-EXEC
023200160822     C*
023300160822     C* 5° giro fuori ciclo x file generico estensioni di dettaglio varie
023400160822     C/EXEC SQL
023500160822     C+ DELETE FROM TIVLRE10F WHERE
023600160822     C+ dec(concat(substr(VLRE1MOMNT, 1, 4),
023700160822     C+     concat(substr(VLRE1MOMNT, 6, 2),
023800160822     C+            substr(VLRE1MOMNT, 9, 2))), 8, 0) < :findata10
023900160822     C/END-EXEC
024000060509     C*
024100060509     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAC00S
024200060509     C* rimuovendo i membri x data di creazione
024300060509     C                   MOVEL(P)  findata7      §DATA
024400060509     C                   CALL(e)   'TRULVINC'
024500060509     C                   PARM                    §DATA             8
024600060509     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
024700060509     C                   PARM      'FNVAC00S'    §FILE            10
024800060509     C*
024900060509     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAB00S
025000060509     C* rimuovendo i membri x data di creazione
025100060509     C                   MOVEL(P)  findata7      §DATA
025200060509     C                   CALL(e)   'TRULVINC'
025300060509     C                   PARM                    §DATA             8
025400060509     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
025500060509     C                   PARM      'FNVAB00S'    §FILE            10
025600160822     C*
025700160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAG00S
025800160822     C* rimuovendo i membri x data di creazione
025900160822     C                   MOVEL(P)  findata7      §DATA
026000160822     C                   CALL(e)   'TRULVINC'
026100160822     C                   PARM                    §DATA             8
026200160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
026300160822     C                   PARM      'FNVAG00S'    §FILE            10
026400160822     C*
026500160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAP00S
026600160822     C* rimuovendo i membri x data di creazione
026700160822     C                   MOVEL(P)  findata7      §DATA
026800160822     C                   CALL(e)   'TRULVINC'
026900160822     C                   PARM                    §DATA             8
027000160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
027100160822     C                   PARM      'FNVAP00S'    §FILE            10
027200160822     C*
027300160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TIVAF00S
027400160822     C* rimuovendo i membri x data di creazione
027500160822     C                   MOVEL(P)  findata7      §DATA
027600160822     C                   CALL(e)   'TRULVINC'
027700160822     C                   PARM                    §DATA             8
027800160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
027900160822     C                   PARM      'TIVAF00S'    §FILE            10
028000160822     C*
028100160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TNVAS00S
028200160822     C* rimuovendo i membri x data di creazione
028300160822     C                   MOVEL(P)  findata7      §DATA
028400160822     C                   CALL(e)   'TRULVINC'
028500160822     C                   PARM                    §DATA             8
028600160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
028700160822     C                   PARM      'TNVAS00S'    §FILE            10
028800160822     C*
028900160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TIVAFWWT
029000160822     C* rimuovendo i membri x data di creazione
029100160822     C                   MOVEL(P)  findata7      §DATA
029200160822     C                   CALL(e)   'TRULVINC'
029300160822     C                   PARM                    §DATA             8
029400160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
029500160822     C                   PARM      'TIVAFWWT'    §FILE            10
029600160822     C*
029700160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TNVASWWT
029800160822     C* rimuovendo i membri x data di creazione
029900160822     C                   MOVEL(P)  findata7      §DATA
030000160822     C                   CALL(e)   'TRULVINC'
030100160822     C                   PARM                    §DATA             8
030200160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
030300160822     C                   PARM      'TNVASWWT'    §FILE            10
030400160822     C*
030500160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file TIVAWWWT
030600160822     C* rimuovendo i membri x data di creazione
030700160822     C                   MOVEL(P)  findata7      §DATA
030800160822     C                   CALL(e)   'TRULVINC'
030900160822     C                   PARM                    §DATA             8
031000160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
031100160822     C                   PARM      'TIVAWWWT'    §FILE            10
031200160822     C*
031300160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAPWWT
031400160822     C* rimuovendo i membri x data di creazione
031500160822     C                   MOVEL(P)  findata7      §DATA
031600160822     C                   CALL(e)   'TRULVINC'
031700160822     C                   PARM                    §DATA             8
031800160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
031900160822     C                   PARM      'FNVAPWWT'    §FILE            10
032000160822     C*
032100160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAGWWT
032200160822     C* rimuovendo i membri x data di creazione
032300160822     C                   MOVEL(P)  findata7      §DATA
032400160822     C                   CALL(e)   'TRULVINC'
032500160822     C                   PARM                    §DATA             8
032600160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
032700160822     C                   PARM      'FNVAGWWT'    §FILE            10
032800160822     C*
032900160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVACWWT
033000160822     C* rimuovendo i membri x data di creazione
033100160822     C                   MOVEL(P)  findata7      §DATA
033200160822     C                   CALL(e)   'TRULVINC'
033300160822     C                   PARM                    §DATA             8
033400160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
033500160822     C                   PARM      'FNVACWWT'    §FILE            10
033600160822     C*
033700160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVAIWWT
033800160822     C* rimuovendo i membri x data di creazione
033900160822     C                   MOVEL(P)  findata7      §DATA
034000160822     C                   CALL(e)   'TRULVINC'
034100160822     C                   PARM                    §DATA             8
034200160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
034300160822     C                   PARM      'FIVAIWWT'    §FILE            10
034400160822     C*
034500160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVABWWR
034600160822     C* rimuovendo i membri x data di creazione
034700160822     C                   MOVEL(P)  findata7      §DATA
034800160822     C                   CALL(e)   'TRULVINC'
034900160822     C                   PARM                    §DATA             8
035000160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
035100160822     C                   PARM      'FIVABWWR'    §FILE            10
035200160822     C*
035300160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVADWWR
035400160822     C* rimuovendo i membri x data di creazione
035500160822     C                   MOVEL(P)  findata7      §DATA
035600160822     C                   CALL(e)   'TRULVINC'
035700160822     C                   PARM                    §DATA             8
035800160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
035900160822     C                   PARM      'FIVADWWR'    §FILE            10
036000160822     C*
036100160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVATWWR
036200160822     C* rimuovendo i membri x data di creazione
036300160822     C                   MOVEL(P)  findata7      §DATA
036400160822     C                   CALL(e)   'TRULVINC'
036500160822     C                   PARM                    §DATA             8
036600160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
036700160822     C                   PARM      'FIVATWWR'    §FILE            10
036800160822     C*
036900160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FIVAXWWR
037000160822     C* rimuovendo i membri x data di creazione
037100160822     C                   MOVEL(P)  findata7      §DATA
037200160822     C                   CALL(e)   'TRULVINC'
037300160822     C                   PARM                    §DATA             8
037400160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
037500160822     C                   PARM      'FIVAXWWR'    §FILE            10
037600160822     C*
037700160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVABWWR
037800160822     C* rimuovendo i membri x data di creazione
037900160822     C                   MOVEL(P)  findata7      §DATA
038000160822     C                   CALL(e)   'TRULVINC'
038100160822     C                   PARM                    §DATA             8
038200160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
038300160822     C                   PARM      'FNVABWWR'    §FILE            10
038400160822     C*
038500160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVADWWR
038600160822     C* rimuovendo i membri x data di creazione
038700160822     C                   MOVEL(P)  findata7      §DATA
038800160822     C                   CALL(e)   'TRULVINC'
038900160822     C                   PARM                    §DATA             8
039000160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
039100160822     C                   PARM      'FNVADWWR'    §FILE            10
039200160822     C*
039300160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVAOWWR
039400160822     C* rimuovendo i membri x data di creazione
039500160822     C                   MOVEL(P)  findata7      §DATA
039600160822     C                   CALL(e)   'TRULVINC'
039700160822     C                   PARM                    §DATA             8
039800160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
039900160822     C                   PARM      'FNVAOWWR'    §FILE            10
040000160822     C*
040100160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVARWWR
040200160822     C* rimuovendo i membri x data di creazione
040300160822     C                   MOVEL(P)  findata7      §DATA
040400160822     C                   CALL(e)   'TRULVINC'
040500160822     C                   PARM                    §DATA             8
040600160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
040700160822     C                   PARM      'FNVARWWR'    §FILE            10
040800160822     C*
040900160822     C* Dopo aver pulito il log eseguo la pulitura "forzata" del file FNVATWWR
041000160822     C* rimuovendo i membri x data di creazione
041100160822     C                   MOVEL(P)  findata7      §DATA
041200160822     C                   CALL(e)   'TRULVINC'
041300160822     C                   PARM                    §DATA             8
041400160822     C                   PARM      'GAITRAGRU'   §LIBRERIA        10
041500160822     C                   PARM      'FNVATWWR'    §FILE            10
041600060509     C*
041700000103     C                   ENDSR
041800970526     C*------------------------------------------------------------------------*
041900970526     C* *INZSR - OPERAZIONI INIZIALI
042000970526     C*------------------------------------------------------------------------*
042100970526     C     *INZSR        BEGSR
042200991230     C*
042300991230     C*---
042400991230     C* VARIABILI RIFERITE AL DATA BASE
042500991230     C*---
042600010226     C     *LIKE         DEFINE    vlrdat        KLRdat
042700010226     C     *LIKE         DEFINE    vlrora        KLRora
042800010226     C     *LIKE         DEFINE    vlrksc        KLRksc
042900991230     C     *LIKE         DEFINE    vltdat        KLTdat
043000991230     C     *LIKE         DEFINE    vltora        KLTora
043100991230     C     *LIKE         DEFINE    vltksc        KLTksc
043200970526     C*---
043300970526     C* VARIABILI DI CONTROLLO
043400970526     C*---
043500010226     C*
043600970526     C*---
043700970526     C* CHIAVI DI LETTURA
043800970526     C*---
043900991230     C* LETTURA tivlt03L -
044000991230     C     KEYvlt        KLIST
044100991230     C                   KFLD                    KLTdat                         *PARTENZA/ARRIVO
044200991230     C                   KFLD                    KLTora                         *PARTENZA/ARRIVO
044300991230     C                   KFLD                    KLTksc                         *PARTENZA/ARRIVO
044400010226     C* LETTURA tivlrs3L -
044500010226     C     KEYvlr        KLIST
044600010226     C                   KFLD                    KLRdat                         *PARTENZA/ARRIVO
044700010226     C                   KFLD                    KLRora                         *PARTENZA/ARRIVO
044800010226     C                   KFLD                    KLRksc                         *PARTENZA/ARRIVO
044900991230     C*
045000991230     C* REPERIMENTO PARAMETRI DA DTAARA
045100991230     C     *DTAARA       DEFINE    TIS7I9A       PARAM
045200991230     C                   IN        PARAM
045300970526     C*
045400970526     C* CALCOLA LA DATA CORRENTE
045500160822     C                   Z-ADD     *zeros        DATCOR            8 0
045600160822     C                   EVAL      DATCOR = %dec(%date() : *ISO)
045700010226     C*
045800010226     C     *ISO          MOVE      DATCOR        findata
045900010226     C     findata       SUBDUR    §GGDWL:*D     findata
046000010226     C     *ISO          MOVE      findata       findata2          8 0
046100010226     C*
046200010226     C     *ISO          MOVE      DATCOR        findata
046300010226     C     findata       SUBDUR    §GGUPL:*D     findata
046400010226     C     *ISO          MOVE      findata       findata3          8 0
046500010226     C*
046600010226     C     *ISO          MOVE      DATCOR        findata
046700010226     C     findata       SUBDUR    §GGUPLF:*D    findata
046800010226     C     *ISO          MOVE      findata       findata7          8 0
046900000612     C*
047000000612     C     *ISO          MOVE      DATCOR        findata
047100000612     C     findata       SUBDUR    §GGDWLF:*D    findata
047200010226     C     *ISO          MOVE      findata       findata8          8 0
047300021127     C*
047400021127     C     *ISO          MOVE      DATCOR        findata
047500021127     C     findata       SUBDUR    §GGUPLS:*D    findata
047600021127     C     *ISO          MOVE      findata       findata9          8 0
047700160822     C*
047800160822     C     *ISO          MOVE      DATCOR        findata
047900160822     C     findata       SUBDUR    90:*D         findata
048000160822     C     *ISO          MOVE      findata       findata10         8 0
048100991230     C*
048200991230     C                   ENDSR
