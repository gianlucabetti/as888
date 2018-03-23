000100971203     H*------------------------------------------------------------------------*
000200050110     H* RICERCA Abil.clienti gestione particolarità
000300971203     H*------------------------------------------------------------------------*
000400971203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500971203     F*------------------------------------------------------------------------*
000600971203     F* DATA BASE
000700971203     F*------------------------------------------------------------------------*
000800050110     Ftabel00f  IF   E           K DISK
000900050110     FTrtb71D1  CF   E             WORKSTN
001000971203     F                                     SFILE(SD24DSF:REC1)
001100020531     d*-------------------------------------------------------------------------
001200020531     d* Data structure
001300020531     d*-------------------------------------------------------------------------
001400050110     dDS7U           e ds
001500050110     dtibs69ds       e ds
001600050110     dcnaco          e ds                  extname(cnaco00f)
001700050110     dcnind          e ds                  extname(cnind00f)
001800050110     dcnclp          e ds                  extname(cnclp00f)
001900050110     dFncls          e ds                  extname(Fncls00f)
002000050111    >D TABELDS       E DS                  extname(TABEL00f)
002100050111      *
002200050111     D Ptr             S               *   INZ(%ADDR(TBLUNI))
002300050111     D TPV             S              2    DIM(19) BASED(Ptr)
002400050111     D TPC             S              2    DIM(19)
002500081203     D x               S              4  0
002600020603      *
002700020531     d NotUsed         S             16A
002800020531     d ReturnSize      S              9B 0
002900020531     d SizeList        S              9B 0
003000020531     d WrkSort         S              1
003100020531     d*---
003200081203     d Rec1            S              4  0
003300081203     d stmrec1         S              4  0
003400020531     d*---
003500020531     d MaxKey          C                   2
003600020531     d Ascendente      C                   1
003700020531     d Discendente     C                   2
003800020531     d Carattere       C                   6
003900020531     d Put             C                   1
004000020531     d EndPut          C                   2
004100020531     d Get             C                   3
004200020603     d Numerico        C                   8
004300050111     d*---
004400050111     d datads          DS
004500050111     d  aa                            2  0
004600050111     d  mm                            2  0
004700050111     d  gg                            2  0
004800050111     d datads1         DS
004900050111     d  gg1                           2  0
005000050111     d  mm1                           2  0
005100050111     d  aa1                           2  0
005200020531     d*---
005300020531     d SflRcd          DS
005400020531     d  Scelta
005500050110     d  cliv
005600050110     d  parv
005700050110     d  ragv
005800050110     d  tpv001
005900050112     d  dusv
006000050112     d  dusinv
006100020531     d  Selected                      1A
006200050110     d*
006300020531      /COPY QSYSINC/QRPGLESRC,QLGSORT
006400020531     d QLGKL                         16    DIM(MaxKey)
006500020531     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
006600020531     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
006700020531     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
006800020531     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
006900020531
007000020531      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
007100020531      /COPY QSYSINC/QRPGLESRC,QUSEC
007200050110     C*---------- EMISSIONE SFL :                          *
007300050110     C                   do        *hival
007400050110     C                   EXFMT     SD24DCT
007500050110     C   KL              exsr      fine
007600050110      *
007700050110     C* F07 = cliente, F08 = rag. soc., F09 = Particolarità
007800050111if  1C                   IF        *INKg or *inkh or *inki or *inkj
007900050110     C                   EXSR      F11GES
008000050110e   1C                   ENDIF
008100050110      *
008200050110     C                   EXSR      CTR
008300050110     C                   enddo
008400971203     C*------------------------------------------------------------------------*
008500050110     C     *inzsr        begsr
008600971203     C*------------------------------------------------------------------------*
008700971203     C     *ENTRY        PLIST
008800050110     C                   PARM                    KNSIF            10
008900050112     C                   PARM                    §Cli              8
009000050110     C                   PARM                    §rag             25
009100050110     c     ktab          klist
009200050110     c                   kfld                    tblkut
009300050110     c                   kfld                    tblcod
009400050110     C                   eval      tblcod='7U'
009500050110     C                   eval      tblKUT=1
009600150812     c     ktab2         klist
009700150812     c                   kfld                    tblkut
009800150812     c                   kfld                    tblcod
009900150812     C                   kfld                    §Cli
010000050110     C*---------- PULIZIA SLFCT :                          *
010100050110     C                   MOVE      *BLANKS       SCELTA
010200081203     C                   Z-ADD     0             REC1
010300900418     C                   SETON                                            70
010400971203     C                   WRITE     SD24DCT
010500900418     C                   SETOFF                                       7071
010600050110     C* posizionamento per rag. soc.
010700050110     c                   seton                                        50
010800020531     C                   Z-ADD     *zeros        Rec1
010900971203     C                   WRITE     SD24D1
011000050110      * carica sfl
011100150812     c                   if        §cli = *blank
011200050110     C     ktab          setll     tabel00f
011300150812     c                   else
011400150812     C     ktab2         setll     tabel00f
011500150812     c                   endif
011600150812      *
011700050110     c                   do        *hival
011800050110     C     ktab          reade     tabel00f
011900050110     c                   if        %eof(tabel00f)
012000050110     c                   leave
012100050110     c                   end
012200050110     c                   if        tblflg <> *blanks
012300050110     c                   iter
012400050110     c                   end
012500050506     C                   EXSR      RIESFL
012600050110     C*
012700050506     c* se la schiera delle particolarità è vuota ma la DS no scrivo un
012800050506     c* record nel sfl
012900050506     c                   eval      ds7u = tbluni
013000050506     c                   if        §7UCPA=' ' and (
013100050506     c                             §7USTB <> ' ' or
013200060307     c                             §7Usre <> ' ' or
013300060307     c                             §7Uldcr > '00')
013400050506     C     REC1          ADD       1             REC1                 71
013500050506     C                   clear                   TPV001
013600050506     C                   WRITE     SD24Z1
013700050506     C                   WRITE     SD24DSF
013800050506     c                   else
013900050110     C                   DO        19            X
014000050506     C                   IF        TPV(X) <> *BLANKS
014100050110     C     REC1          ADD       1             REC1                 71
014200050110     C                   MOVEL     TPV(X)        TPV001
014300050110     C                   WRITE     SD24Z1
014400050110     C                   WRITE     SD24DSF
014500050110     C                   END
014600050110     C                   ENDDO
014700050506     C                   END
014800050110     c                   enddo
014900050110     c*
015000020603     C                   Z-ADD     REC1          STMREC1
015100020710     C                   EVAL      RcdNbr  = 1
015200050110     c                   endsr
015300050110     C*----------------------------------------------------*
015400050110     C     fine          BEGSR
015500050110     C*----------------------------------------------------*
015600020603      * Non ho scelto nulla
015700050110     c                   if        *inkl
015800050110     C                   MOVE      *BLANKS       §cli
015900050110     C                   MOVE      *BLANKS       §rag
016000050110     c                   end
016100000000     C                   SETON                                            LR
016200050110     c                   return
016300050110     c                   endsr
016400020603      *-------------------------------------------------------------------------
016500020603      * Valorizzo campo tipo flag tipo p.o
016600020603      *-------------------------------------------------------------------------
016700020603     C     RIESFL        BEGSR
016800050110     c*
016900050110     c                   movel     tblkey        cliv
017000050110     c                   eval      parv = %subst(tblkey: 8: 1)
017100050110     C                   CLEAR                   TIBS69ds
017200050110     C                   CLEAR                   CNACO
017300050110     C                   CLEAR                   CNIND
017400050110     C                   CLEAR                   CNCLP
017500050110     C                   CLEAR                   FNCLS
017600050110     C                   MOVEL     ' '           I69TLA
017700050110     C                   MOVEL     KNSIF         I69SIF
017800050110     C                   MOVE      cliv          I69KAC
017900050110     C                   MOVE      cliv          I69Kcp
018000050110     C                   CALL      'TIBS69R'
018100050110     C                   PARM                    TIBS69ds
018200050110     C                   PARM                    CNACO
018300050110     C                   PARM                    CNIND
018400050110     C                   PARM                    CNCLP
018500050110     C                   PARM                    FNCLS
018600050110     C                   if        o69err <>*blanks
018700050110     C                   EVAL      ragv = *ALL'?'
018800050110     C                   ELSE
018900050110     C                   MOVEL     ACORAG        ragv
019000050112     c                   if        clpdus <> 0
019100050112     C                   MOVE      clpdus        dusinv
019200050112     C                   MOVE      clpdus        datads
019300050111     C                   MOVE      aa            aa1
019400050111     C                   MOVE      mm            mm1
019500050111     C                   MOVE      gg            gg1
019600050111     C                   MOVE      datads1       dusv
019700050112     c                   if        aa < 49
019800050112     c                   movel     20            dusinv
019900050112     c                   else
020000050112     c                   movel     19            dusinv
020100050112     c                   end
020200050112     c                   else
020300050112     c* forzo 1 nel dusinv altrimenti non mi carica i record a *blanks
020400050112     c* perchè non sono inferiori alla scelta
020500050112     c                   z-add     0             dusv
020600050112     c                   movel     '00000000'    dusinv
020700050112     c                   end
020800050110     C                   END
020900050110     c*
021000020603     C                   ENDSR
021100020531      *-------------------------------------------------------------------------
021200020531      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/P.O
021300020531      *-------------------------------------------------------------------------
021400020531     C     F11GES        BEGSR
021500050110     C                   setoff                                       505152
021600050111     C                   setoff                                       7253
021700050110     c                   select
021800050110     c                   when      *inkg
021900050110     C                   CLEAR                   C1Crag
022000050110     C                   CLEAR                   C1Cpar
022100050111     C                   CLEAR                   C1Cdus
022200050110     c                   when      *inkh
022300050110     C                   CLEAR                   C1Ccli
022400050110     C                   CLEAR                   C1Cpar
022500050111     C                   CLEAR                   C1Cdus
022600050110     c                   when      *inki
022700050110     C                   CLEAR                   C1Ccli
022800050110     C                   CLEAR                   C1Crag
022900050111     C                   CLEAR                   C1Cdus
023000050111     c                   when      *inkJ
023100050111     C                   CLEAR                   C1Ccli
023200050111     C                   CLEAR                   C1Crag
023300050111     C                   CLEAR                   C1Cpar
023400050110     c                   endsl
023500020709      * IMPOSTO ENTRATA AD 'A'
023600020613     C                   EVAL      rcdnbr  = 1
023700020531      *
023800020531     C                   CLEAR                   QLGSCB
023900020531     C                   CLEAR                   QLGSCB00
024000020531
024100020531     ** 1 campo chiave.
024200020531     C                   EVAL      QLGNBRK    = 1
024300020531
024400050110     c                   select
024500050110     C                   when      *inkg
024600050111     ** IL cliente è a posizione 2, numerico, 3 byte, ascending.
024700050110     C                   EVAL      QLGSP      = 2
024800050110     C                   EVAL      QLGSS      = %SIZE(cliv)
024900050110     C                   EVAL      QLGDT      = CARATTERE
025000020531     C                   EVAL      QLGSO      = Ascendente
025100020531     C                   EVAL      QLGKL(1)   = QLGSKL
025200020603     C                   seton                                        50
025300050110     ** La ragione sociale è a posizione 10, 25 byte, char, ascending.
025400050110     c                   when      *inkh
025500050110     C                   EVAL      QLGSP      = 10
025600050110     C                   EVAL      QLGSS      = %SIZE(ragv)
025700020603     C                   EVAL      QLGDT      = Carattere
025800020603     C                   EVAL      QLGSO      = Ascendente
025900020603     C                   EVAL      QLGKL(1)   = QLGSKL
026000050110     C                   seton                                        51
026100050111     ** La particolarità è a posizione 35, 2 byte, char, ascending.
026200050110     c                   when      *inkI
026300050110     C                   EVAL      QLGSP      = 35
026400050110     C                   EVAL      QLGSS      = %SIZE(TPV001)
026500050110     C                   EVAL      QLGDT      = Carattere
026600050110     C                   EVAL      QLGSO      = Ascendente
026700050110     C                   EVAL      QLGKL(1)   = QLGSKL
026800050110     C                   seton                                        52
026900050111     ** La data ult. fatturaz. è a posizione 42, 6 byte, char, ascending.
027000050111     c                   when      *inkj
027100050112     C                   EVAL      QLGSP      = 43
027200050111     C                   EVAL      QLGSS      = %SIZE(dusinv)
027300050111     C                   EVAL      QLGDT      = Carattere
027400050111     C                   EVAL      QLGSO      = Ascendente
027500050111     C                   EVAL      QLGKL(1)   = QLGSKL
027600050111     C                   seton                                        53
027700050110     C                   ENDsl
027800020531
027900020531      * Load other sort parameters.
028000020531     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
028100020531     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
028200020531     C                   EVAL      QLGRT     = 8
028300020531     C                   EVAL      QLGOKL    = 80
028400020531     C                   EVAL      QLGLKE    = 16
028500020531     C                   EVAL      QLGLSS    = 290
028600020531
028700020531      * Initialize Sort I/O API fields.
028800020531     C                   EVAL      QLGRL00  = QLGRL
028900020531     C                   EVAL      QLGRC00  = 1
029000020531     C                   CLEAR                   QUSEI
029100020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
029200020531
029300020531      * First step - Initialize the sort routine.
029400020531     C                   CALL      'QLGSORT'
029500020531     C                   PARM                    QLGSCB
029600020531     C                   PARM                    NotUsed
029700020531     C                   PARM                    NotUsed
029800020531     C                   PARM                    SizeList
029900020531     C                   PARM                    ReturnSize
030000020531     C                   PARM                    QUSEC
030100020531
030200020531      * Next step - Write records to I/O routine.
030300020531     C                   EVAL      QLGRT00 = Put
030400020531
030500050111     C                   DO        STMREC1       x
030600020531
030700020613     C     x             CHAIN     SD24DSF
030800020531
030900020531     ** Solo le righe con Selected = 'Y' sono riordinate,
031000020531     ** quindi per fare un ordinamento di tutte le righe
031100020531     ** metto 'Y' sempre.
031200020531     C                   EVAL      Selected = 'Y'
031300020531
031400020531     C                   CLEAR                   QUSEI
031500020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
031600020531     C                   CALL      'QLGSRTIO'
031700020531     C                   PARM                    QLGSCB00
031800020531     C                   PARM                    SflRcd
031900020531     C                   PARM                    NotUsed
032000020531     C                   PARM                    SizeList
032100020531     C                   PARM                    NotUsed
032200020531     C                   PARM                    QUSEC
032300020531
032400020531     C                   ENDDO
032500020531
032600020531      * Next step - Signal end of input, clear subfile for reload.
032700020531     C                   EVAL      QLGRT00 = EndPut
032800020531     C                   CLEAR                   QUSEI
032900020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
033000020531
033100020531     C                   CALL      'QLGSRTIO'
033200020531     C                   PARM                    QLGSCB00
033300020531     C                   PARM                    SflRcd
033400020531     C                   PARM                    NotUsed
033500020531     C                   PARM                    SizeList
033600020531     C                   PARM                    NotUsed
033700020531     C                   PARM                    QUSEC
033800020531
033900020531     C                   SETON                                            70
034000020531     C                   WRITE     SD24DCT
034100020603     C                   SETOFF                                       70
034200020612     C                   WRITE     SD24D1
034300020613     C                   WRITE     SD24Z1
034400020531
034500020531      * Final step - Write the records back to the subfile.
034600020531     C                   EVAL      QLGRT00 = Get
034700020531
034800020613     C                   DO        STMREC1       x
034900020531     C                   CLEAR                   QUSEI
035000020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
035100020531     C                   CALL      'QLGSRTIO'
035200020531     C                   PARM                    QLGSCB00
035300020531     C                   PARM                    NotUsed
035400020531     C                   PARM                    SflRcd
035500020531     C                   PARM                    QLGRL00
035600020531     C                   PARM                    NotUsed
035700020531     C                   PARM                    QUSEC
035800020613     c                   z-add     x             rec1
035900020531     C                   WRITE     SD24DSF
036000020531     C                   ENDDO
036100020531
036200020531     C                   EVAL      *IN70 = *off
036300020604      *
036400020531     C                   ENDSR
036500020606      *-------------------------------------------------------------------------
036600020613      * CTR Subroutine per il riposizionamento
036700020606      *-------------------------------------------------------------------------
036800020606     C     CTR           BEGSR
036900020606     C*
037000050110     C                   IF        *IN71
037100020612     C*
037200050110     C                   READC     SD24DSF                                91
037300020613   1aC     Scelta        IFEQ      '1'
037400050110     C                   MOVEL     cliv          §Cli
037500050112     C                   MOVE      parv          §cli
037600050112     C                   MOVEL     ragv          §rag
037700050110     C                   EXSR      FINE
037800020613   1eC                   ELSE
037900050112     c* inverto data ultima fatt.
038000050112     c                   if        *in53 and c1cdus <> *blanks
038100050112     c                   move      c1cdus        datads
038200050112     c                   move      aa            aa1
038300050112     c                   move      mm            mm1
038400050112     c                   move      gg            gg1
038500050112     c                   move      datads1       comdus            8
038600050112     c                   if        aa < 49
038700050112     c                   movel     20            comdus
038800050112     c                   else
038900050112     c                   movel     19            comdus
039000050112     c                   end
039100050112     c                   else
039200050112     c                   clear                   comdus
039300050112     c                   end
039400020613      *
039500020613      * Controllo la scelta
039600020710     C                   seton                                        72
039700020613   2aC                   DO        *hival        X
039800020613     C     X             CHAIN     SD24DSF                            91
039900020613     C   91              LEAVE
040000020612      *
040100050110   3aC                   IF        (*in50 = *on and C1Ccli <= cliv) or
040200050110     C                             (*in51 = *on and C1Crag <= ragv) or
040300050111     C                             (*in52 = *on and C1Cpar <= TPV001) or
040400050112     C                             (*in53 = *on and comdus <= dusinv)
040500020613     C                   z-add     X             RcdNbr
040600020710     C                   setoff                                       72
040700020613   3eC                   ELSE
040800020613     C                   ITER
040900020613     C                   ENDIF
041000020710     C                   WRITE     SD24Z1
041100050110     C                   LEAVE
041200020613   2-C                   ENDDO
041300020710      * riemetto se non ho trovato records
041400020710     C   72              DO
041500020710     C                   WRITE     SD24Z1
041600050110     C                   LEAVE
041700020710     C                   ENDDO
041800020710   1-C                   ENDIF
041900020606      *
042000050110     C                   move      *blanks       C1CRAG
042100050110     C                   move      *blanks       C1CCLI
042200050110     C                   move      *blanks       C1CPAR
042300050111     C                   move      *blanks       C1Cdus
042400050110     C                   END
042500020613      *
042600020606     C                   ENDSR
