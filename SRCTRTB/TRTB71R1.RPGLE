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
002000170125     D*------------------
002100170125     D BS10DS        E DS                  EXTNAME(TIBS10DS)
002200170125     D*------------------
002300170125     D WLBDA8          DS                  INZ
002400170125     D  G08DAT                 1      8  0
002500170125     D  G08INV                 9     16  0
002600170125     D  G08ERR                17     17
002700170125     D  G08TGI                18     22  0
002800170125     D*------------------
002900050111    >D TABELDS       E DS                  extname(TABEL00f)
003000050111      *
003100050111     D Ptr             S               *   INZ(%ADDR(TBLUNI))
003200050111     D TPV             S              2    DIM(19) BASED(Ptr)
003300050111     D TPC             S              2    DIM(19)
003400081203     D x               S              4  0
003500020603      *
003600020531     d NotUsed         S             16A
003700020531     d ReturnSize      S              9B 0
003800020531     d SizeList        S              9B 0
003900020531     d WrkSort         S              1
004000020531     d*---
004100081203     d Rec1            S              4  0
004200081203     d stmrec1         S              4  0
004300020531     d*---
004400020531     d MaxKey          C                   2
004500020531     d Ascendente      C                   1
004600020531     d Discendente     C                   2
004700020531     d Carattere       C                   6
004800020531     d Put             C                   1
004900020531     d EndPut          C                   2
005000020531     d Get             C                   3
005100020603     d Numerico        C                   8
005200050111     d*---
005300050111     d datads          DS
005400050111     d  aa                            2  0
005500050111     d  mm                            2  0
005600050111     d  gg                            2  0
005700050111     d datads1         DS
005800050111     d  gg1                           2  0
005900050111     d  mm1                           2  0
006000050111     d  aa1                           2  0
006100020531     d*---
006200020531     d SflRcd          DS
006300020531     d  Scelta
006400050110     d  cliv
006500050110     d  parv
006600050110     d  ragv
006700050110     d  tpv001
006800050112     d  dusv
006900050112     d  dusinv
007000170125     d  codpad
007100170131     d  Hparv
007200020531     d  Selected                      1A
007300050110     d*
007400020531      /COPY QSYSINC/QRPGLESRC,QLGSORT
007500020531     d QLGKL                         16    DIM(MaxKey)
007600020531     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
007700020531     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
007800020531     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
007900020531     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
008000020531
008100020531      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008200020531      /COPY QSYSINC/QRPGLESRC,QUSEC
008300050110     C*---------- EMISSIONE SFL :                          *
008400170131      *
008500170131      * Inizializza il caricamento del SFL
008600170131     C                   EXSR      INZD1
008700170131      *
008800050110     C                   do        *hival
008900170131     C                   WRITE     SD24D1
009000170131     C                   WRITE     SD24Z1
009100170131     C                   WRITE     SD24Z1
009200050110     C                   EXFMT     SD24DCT
009300050110     C   KL              exsr      fine
009400050110      *
009500050110     C* F07 = cliente, F08 = rag. soc., F09 = Particolarità
009600170126if  1C                   IF        *INKg or *inkh or *inki or *inkj or *inkk
009700170131     C                   EXSR      FXXOrdina
009800050110e   1C                   ENDIF
009900050110      *
010000050110     C                   EXSR      CTR
010100050110     C                   enddo
010200971203     C*------------------------------------------------------------------------*
010300050110     C     *inzsr        begsr
010400971203     C*------------------------------------------------------------------------*
010500971203     C     *ENTRY        PLIST
010600050110     C                   PARM                    KNSIF            10
010700050112     C                   PARM                    §Cli              8
010800050110     C                   PARM                    §rag             25
010900170126     C                   PARM                    KNMUS            10
011000170126     C                   PARM                    V1CRSU           20
011100170125     C*
011200170126     C                   IF        %PARMS= 3
011300170126     C                   CLEAR                   KNMUS            10
011400170126     C                   CLEAR                   V1CRSU           20
011500170126     C                   END
011600170126     C*
011700170131     C* se richiamato solo in visualizzazione lista clienti senza SCELTA
011800170131     C                   IF        §rag = *all'V'
011900170131     C                   seton                                        01
012000170131     C                   END
012100170131     C*
012200170125     C* IMNPOSTA DATA / ORA CORRENTE
012300170125     C                   TIME                    N14              14 0
012400170125     C                   MOVE      N14           G08DAT            8 0
012500170125     C                   Z-ADD     *ZEROS        G08INV
012600170125     C                   MOVEL     '0'           G08ERR
012700170125     C                   CALL      'XSRDA8'
012800170125     C                   PARM                    WLBDA8
012900170125     C                   Z-ADD     G08INV        DATCOR            8 0
013000170125     C*
013100050110     c     ktab          klist
013200050110     c                   kfld                    tblkut
013300050110     c                   kfld                    tblcod
013400050110     C                   eval      tblcod='7U'
013500050110     C                   eval      tblKUT=1
013600150812     c     ktab2         klist
013700150812     c                   kfld                    tblkut
013800150812     c                   kfld                    tblcod
013900150812     C                   kfld                    §Cli
014000170131      *
014100170131     c                   endsr
014200170131      *-----------------------------------------------------------------------
014300170131      ** inizializza il video da emettere
014400170131      *-----------------------------------------------------------------------
014500170131     c     INZD1         begsr
014600170131      *
014700050110     C*---------- PULIZIA SLFCT :                          *
014800050110     C                   MOVE      *BLANKS       SCELTA
014900081203     C                   Z-ADD     0             REC1
015000170131      *
015100900418     C                   SETON                                            70
015200971203     C                   WRITE     SD24DCT
015300900418     C                   SETOFF                                       7071
015400170131      *
015500050110     C* posizionamento per rag. soc.
015600050110     c                   seton                                        50
015700020531     C                   Z-ADD     *zeros        Rec1
015800170131     C*****              WRITE     SD24D1
015900170131     c*****              WRITE     SD24W1
016000050110      * carica sfl
016100150812     c                   if        §cli = *blank
016200050110     C     ktab          setll     tabel00f
016300150812     c                   else
016400150812     C     ktab2         setll     tabel00f
016500150812     c                   endif
016600150812      *
016700050110     c                   do        *hival
016800050110     C     ktab          reade     tabel00f
016900050110     c                   if        %eof(tabel00f)
017000050110     c                   leave
017100050110     c                   end
017200050110     c                   if        tblflg <> *blanks
017300050110     c                   iter
017400050110     c                   end
017500050506     C                   EXSR      RIESFL
017600050110     C*
017700050506     c* se la schiera delle particolarità è vuota ma la DS no scrivo un
017800050506     c* record nel sfl
017900170201     c                   clear                   codpad
018000050506     c                   eval      ds7u = tbluni
018100050506     c                   if        §7UCPA=' ' and (
018200050506     c                             §7USTB <> ' ' or
018300060307     c                             §7Usre <> ' ' or
018400060307     c                             §7Uldcr > '00')
018500050506     C     REC1          ADD       1             REC1                 71
018600050506     C                   clear                   TPV001
018700170201     c                   eval      codPAD ='x_Resi'
018800170131     C*******            WRITE     SD24Z1
018900050506     C                   WRITE     SD24DSF
019000050506     c                   else
019100050110     C                   DO        19            X
019200050506     C                   IF        TPV(X) <> *BLANKS
019300050110     C     REC1          ADD       1             REC1                 71
019400050110     C                   MOVEL     TPV(X)        TPV001
019500170131     C******             WRITE     SD24Z1
019600050110     C                   WRITE     SD24DSF
019700050110     C                   END
019800050110     C                   ENDDO
019900050506     C                   END
020000050110     c                   enddo
020100050110     c*
020200020603     C                   Z-ADD     REC1          STMREC1
020300020710     C                   EVAL      RcdNbr  = 1
020400050110     c                   endsr
020500050110     C*----------------------------------------------------*
020600050110     C     fine          BEGSR
020700050110     C*----------------------------------------------------*
020800020603      * Non ho scelto nulla
020900050110     c                   if        *inkl
021000050110     C                   MOVE      *BLANKS       §cli
021100050110     C                   MOVE      *BLANKS       §rag
021200050110     c                   end
021300000000     C                   SETON                                            LR
021400050110     c                   return
021500050110     c                   endsr
021600020603      *-------------------------------------------------------------------------
021700020603      * Valorizzo campo tipo flag tipo p.o
021800020603      *-------------------------------------------------------------------------
021900020603     C     RIESFL        BEGSR
022000050110     c*
022100050110     c                   movel     tblkey        cliv
022200170131     c                   eval      Hparv = %subst(tblkey: 8: 1)
022300170131     c                   eval      parv  = %subst(tblkey: 8: 1)
022400170126     c                   if        parv = 'Q'
022500170126     c                   eval      parv = 'G'
022600170126     c                   elseif    parv = 'R'
022700170126     c                   eval      parv = 'C'
022800170126     c                   elseif    parv = 'S'
022900170126     c                   eval      parv = 'V'
023000170126     c                   end
023100050110     C                   CLEAR                   TIBS69ds
023200050110     C                   CLEAR                   CNACO
023300050110     C                   CLEAR                   CNIND
023400050110     C                   CLEAR                   CNCLP
023500050110     C                   CLEAR                   FNCLS
023600050110     C                   MOVEL     ' '           I69TLA
023700050110     C                   MOVEL     KNSIF         I69SIF
023800050110     C                   MOVE      cliv          I69KAC
023900050110     C                   MOVE      cliv          I69Kcp
024000050110     C                   CALL      'TIBS69R'
024100050110     C                   PARM                    TIBS69ds
024200050110     C                   PARM                    CNACO
024300050110     C                   PARM                    CNIND
024400050110     C                   PARM                    CNCLP
024500050110     C                   PARM                    FNCLS
024600050110     C                   if        o69err <>*blanks
024700050110     C                   EVAL      ragv = *ALL'?'
024800050110     C                   ELSE
024900050110     C                   MOVEL     ACORAG        ragv
025000170125      *-----
025100170125      *-  CODICE PADRE UNIFICANTE
025200170201     c                   goto      da_saltare
025300170125     C                   CLEAR                   BS10DS
025400170125     C                   Z-ADD     DATCOR        D10DRF
025500170125     C                   MOVEL     'ST'          D10TLE
025600170125     C                   MOVEL     'P'           D10PAF
025700170125     C                   MOVE      *ALL'0'       D10COD
025800170125     C                   MOVE      cliv          D10COD
025900170125     C                   CALL      'TIBS10R'
026000170125     C                   PARM                    BS10DS
026100170125     C                   Z-ADD     D10COD        UNIcli            7 0
026200170125IF  2C     D10ERR        IFEQ      *BLANKS
026300170125     C     D10COP        ANDNE     *ZEROS
026400170125     C                   Z-ADD     D10COP        UNIcli
026500170125E   2C                   ENDIF
026600170131     C*-----
026700170125     C                   MOVE      UNIcli        CODPAD
026800170201     c     da_saltare    tag
026900170125     C*-----
027000050112     c                   if        clpdus <> 0
027100050112     C                   MOVE      clpdus        dusinv
027200050112     C                   MOVE      clpdus        datads
027300050111     C                   MOVE      aa            aa1
027400050111     C                   MOVE      mm            mm1
027500050111     C                   MOVE      gg            gg1
027600050111     C                   MOVE      datads1       dusv
027700050112     c                   if        aa < 49
027800050112     c                   movel     20            dusinv
027900050112     c                   else
028000050112     c                   movel     19            dusinv
028100050112     c                   end
028200050112     c                   else
028300050112     c* forzo 1 nel dusinv altrimenti non mi carica i record a *blanks
028400050112     c* perchè non sono inferiori alla scelta
028500050112     c                   z-add     0             dusv
028600050112     c                   movel     '00000000'    dusinv
028700050112     c                   end
028800050110     C                   END
028900050110     c*
029000020603     C                   ENDSR
029100020531      *-------------------------------------------------------------------------
029200170131      * FXXOrdina TASTO FUNZIONE Fxx --> Esegue un ordinamento
029300020531      *-------------------------------------------------------------------------
029400170131     C     FXXOrdina     BEGSR
029500050110     C                   setoff                                       505152
029600170125     C                   setoff                                       725354
029700050110     c                   select
029800050110     c                   when      *inkg
029900050110     C                   CLEAR                   C1Crag
030000050110     C                   CLEAR                   C1Cpar
030100050111     C                   CLEAR                   C1Cdus
030200170125     C                   CLEAR                   C1Cpad
030300050110     c                   when      *inkh
030400050110     C                   CLEAR                   C1Ccli
030500050110     C                   CLEAR                   C1Cpar
030600050111     C                   CLEAR                   C1Cdus
030700170125     C                   CLEAR                   C1Cpad
030800050110     c                   when      *inki
030900050110     C                   CLEAR                   C1Ccli
031000050110     C                   CLEAR                   C1Crag
031100050111     C                   CLEAR                   C1Cdus
031200170125     C                   CLEAR                   C1Cpad
031300050111     c                   when      *inkJ
031400050111     C                   CLEAR                   C1Ccli
031500050111     C                   CLEAR                   C1Crag
031600050111     C                   CLEAR                   C1Cpar
031700170125     C                   CLEAR                   C1Cpad
031800170126     c                   when      *inkk
031900170125     C                   CLEAR                   C1Ccli
032000170125     C                   CLEAR                   C1Crag
032100170125     C                   CLEAR                   C1Cpar
032200170125     C                   CLEAR                   C1Cdus
032300050110     c                   endsl
032400020709      * IMPOSTO ENTRATA AD 'A'
032500020613     C                   EVAL      rcdnbr  = 1
032600020531      *
032700020531     C                   CLEAR                   QLGSCB
032800020531     C                   CLEAR                   QLGSCB00
032900020531
033000020531     ** 1 campo chiave.
033100020531     C                   EVAL      QLGNBRK    = 1
033200020531
033300050110     c                   select
033400050110     C                   when      *inkg
033500050111     ** IL cliente è a posizione 2, numerico, 3 byte, ascending.
033600050110     C                   EVAL      QLGSP      = 2
033700050110     C                   EVAL      QLGSS      = %SIZE(cliv)
033800050110     C                   EVAL      QLGDT      = CARATTERE
033900020531     C                   EVAL      QLGSO      = Ascendente
034000020531     C                   EVAL      QLGKL(1)   = QLGSKL
034100020603     C                   seton                                        50
034200050110     ** La ragione sociale è a posizione 10, 25 byte, char, ascending.
034300050110     c                   when      *inkh
034400050110     C                   EVAL      QLGSP      = 10
034500050110     C                   EVAL      QLGSS      = %SIZE(ragv)
034600020603     C                   EVAL      QLGDT      = Carattere
034700020603     C                   EVAL      QLGSO      = Ascendente
034800020603     C                   EVAL      QLGKL(1)   = QLGSKL
034900050110     C                   seton                                        51
035000050111     ** La particolarità è a posizione 35, 2 byte, char, ascending.
035100050110     c                   when      *inkI
035200050110     C                   EVAL      QLGSP      = 35
035300050110     C                   EVAL      QLGSS      = %SIZE(TPV001)
035400050110     C                   EVAL      QLGDT      = Carattere
035500050110     C                   EVAL      QLGSO      = Ascendente
035600050110     C                   EVAL      QLGKL(1)   = QLGSKL
035700050110     C                   seton                                        52
035800050111     ** La data ult. fatturaz. è a posizione 42, 6 byte, char, ascending.
035900050111     c                   when      *inkj
036000050112     C                   EVAL      QLGSP      = 43
036100050111     C                   EVAL      QLGSS      = %SIZE(dusinv)
036200050111     C                   EVAL      QLGDT      = Carattere
036300050111     C                   EVAL      QLGSO      = Ascendente
036400050111     C                   EVAL      QLGKL(1)   = QLGSKL
036500050111     C                   seton                                        53
036600170125     ** Il codice padre è a posizione 51, 7 byte, char, ascending.
036700170126     c                   when      *inkk
036800170125     C                   EVAL      QLGSP      = 51
036900170125     C                   EVAL      QLGSS      = %SIZE(codpad)
037000170125     C                   EVAL      QLGDT      = Carattere
037100170125     C                   EVAL      QLGSO      = Ascendente
037200170125     C                   EVAL      QLGKL(1)   = QLGSKL
037300170125     C                   seton                                        54
037400050110     C                   ENDsl
037500020531
037600020531      * Load other sort parameters.
037700020531     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
037800020531     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
037900020531     C                   EVAL      QLGRT     = 8
038000020531     C                   EVAL      QLGOKL    = 80
038100020531     C                   EVAL      QLGLKE    = 16
038200020531     C                   EVAL      QLGLSS    = 290
038300020531
038400020531      * Initialize Sort I/O API fields.
038500020531     C                   EVAL      QLGRL00  = QLGRL
038600020531     C                   EVAL      QLGRC00  = 1
038700020531     C                   CLEAR                   QUSEI
038800020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
038900020531
039000020531      * First step - Initialize the sort routine.
039100020531     C                   CALL      'QLGSORT'
039200020531     C                   PARM                    QLGSCB
039300020531     C                   PARM                    NotUsed
039400020531     C                   PARM                    NotUsed
039500020531     C                   PARM                    SizeList
039600020531     C                   PARM                    ReturnSize
039700020531     C                   PARM                    QUSEC
039800020531
039900020531      * Next step - Write records to I/O routine.
040000020531     C                   EVAL      QLGRT00 = Put
040100020531
040200050111     C                   DO        STMREC1       x
040300020531
040400020613     C     x             CHAIN     SD24DSF
040500020531
040600020531     ** Solo le righe con Selected = 'Y' sono riordinate,
040700020531     ** quindi per fare un ordinamento di tutte le righe
040800020531     ** metto 'Y' sempre.
040900020531     C                   EVAL      Selected = 'Y'
041000020531
041100020531     C                   CLEAR                   QUSEI
041200020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
041300020531     C                   CALL      'QLGSRTIO'
041400020531     C                   PARM                    QLGSCB00
041500020531     C                   PARM                    SflRcd
041600020531     C                   PARM                    NotUsed
041700020531     C                   PARM                    SizeList
041800020531     C                   PARM                    NotUsed
041900020531     C                   PARM                    QUSEC
042000020531
042100020531     C                   ENDDO
042200020531
042300020531      * Next step - Signal end of input, clear subfile for reload.
042400020531     C                   EVAL      QLGRT00 = EndPut
042500020531     C                   CLEAR                   QUSEI
042600020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
042700020531
042800020531     C                   CALL      'QLGSRTIO'
042900020531     C                   PARM                    QLGSCB00
043000020531     C                   PARM                    SflRcd
043100020531     C                   PARM                    NotUsed
043200020531     C                   PARM                    SizeList
043300020531     C                   PARM                    NotUsed
043400020531     C                   PARM                    QUSEC
043500020531
043600020531     C                   SETON                                            70
043700020531     C                   WRITE     SD24DCT
043800020603     C                   SETOFF                                       70
043900170131     C*****              WRITE     SD24D1
044000170131     C*****              WRITE     SD24Z1
044100020531
044200020531      * Final step - Write the records back to the subfile.
044300020531     C                   EVAL      QLGRT00 = Get
044400020531
044500020613     C                   DO        STMREC1       x
044600020531     C                   CLEAR                   QUSEI
044700020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
044800020531     C                   CALL      'QLGSRTIO'
044900020531     C                   PARM                    QLGSCB00
045000020531     C                   PARM                    NotUsed
045100020531     C                   PARM                    SflRcd
045200020531     C                   PARM                    QLGRL00
045300020531     C                   PARM                    NotUsed
045400020531     C                   PARM                    QUSEC
045500020613     c                   z-add     x             rec1
045600020531     C                   WRITE     SD24DSF
045700020531     C                   ENDDO
045800020531
045900020531     C                   EVAL      *IN70 = *off
046000020604      *
046100020531     C                   ENDSR
046200020606      *-------------------------------------------------------------------------
046300020613      * CTR Subroutine per il riposizionamento
046400020606      *-------------------------------------------------------------------------
046500020606     C     CTR           BEGSR
046600020606     C*
046700050110     C                   IF        *IN71
046800020612     C*
046900050110     C                   READC     SD24DSF                                91
047000020613   1aC     Scelta        IFEQ      '1'
047100050110     C                   MOVEL     cliv          §Cli
047200170131     C                   MOVE      Hparv         §cli
047300050112     C                   MOVEL     ragv          §rag
047400050110     C                   EXSR      FINE
047500020613   1eC                   ELSE
047600050112     c* inverto data ultima fatt.
047700050112     c                   if        *in53 and c1cdus <> *blanks
047800050112     c                   move      c1cdus        datads
047900050112     c                   move      aa            aa1
048000050112     c                   move      mm            mm1
048100050112     c                   move      gg            gg1
048200050112     c                   move      datads1       comdus            8
048300050112     c                   if        aa < 49
048400050112     c                   movel     20            comdus
048500050112     c                   else
048600050112     c                   movel     19            comdus
048700050112     c                   end
048800050112     c                   else
048900050112     c                   clear                   comdus
049000050112     c                   end
049100020613      *
049200020613      * Controllo la scelta
049300020710     C                   seton                                        72
049400020613   2aC                   DO        *hival        X
049500020613     C     X             CHAIN     SD24DSF                            91
049600020613     C   91              LEAVE
049700020612      *
049800050110   3aC                   IF        (*in50 = *on and C1Ccli <= cliv) or
049900050110     C                             (*in51 = *on and C1Crag <= ragv) or
050000050111     C                             (*in52 = *on and C1Cpar <= TPV001) or
050100170125     C                             (*in53 = *on and comdus <= dusinv) or
050200170125     C                             (*in54 = *on and c1cpad <= codpad)
050300020613     C                   z-add     X             RcdNbr
050400020710     C                   setoff                                       72
050500020613   3eC                   ELSE
050600020613     C                   ITER
050700020613     C                   ENDIF
050800170131     C*******            WRITE     SD24Z1
050900050110     C                   LEAVE
051000020613   2-C                   ENDDO
051100020710      * riemetto se non ho trovato records
051200020710     C   72              DO
051300170131     C*******            WRITE     SD24Z1
051400050110     C                   LEAVE
051500020710     C                   ENDDO
051600020710   1-C                   ENDIF
051700020606      *
051800050110     C                   move      *blanks       C1CRAG
051900050110     C                   move      *blanks       C1CCLI
052000050110     C                   move      *blanks       C1CPAR
052100050111     C                   move      *blanks       C1Cdus
052200170125     C                   move      *blanks       C1Cpad
052300050110     C                   END
052400020613      *
052500020606     C                   ENDSR
