000100971203     H*------------------------------------------------------------------------*
000200050110     H* RICERCA Abil.clienti gestione particolarità
000300971203     H*------------------------------------------------------------------------*
000400971203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500971203     F*------------------------------------------------------------------------*
000600971203     F* DATA BASE
000700971203     F*------------------------------------------------------------------------*
000800140402     Ftabel00f  uF a E           K DISK    usropn
000900170201     fTABELx0F  Uf A e           k disk    rename(tabel:tabelx) prefix(x)
001000170201     f                                     usropn
001100140401     FTrtb7uD   CF   E             WORKSTN
001200140401     F                                     SFILE(tb7uDSF:REC1)
001300140402     fsysprt    o    f  132        printer infds(stamp1)
001400020531     d*-------------------------------------------------------------------------
001500020531     d* Data structure
001600020531     d*-------------------------------------------------------------------------
001700140401     d kpjba         e ds
001800140401      * - Parametri per richiamo al pgm di controllo profilo utenti
001900140401     d TIBS34ds      e ds
002000140401      * - Ds di riferimento al file esterno AzUte00F
002100140401     d AZUTEds       e ds                  ExtName(AzUte00F)
002200140401      * - Ds per dati organigramma
002300140401     d dDatiUte      e ds
002400140403     D dute01        e ds                  inz
002500140403       // -?Caricamento filiali gestite dall'utente?
002600140403     d TRUL31ds      e ds                  inz
002700140403     d  lin            s              3    inz   dim(250)
002800140403      *
002900050110     dDS7U           e ds
003000140512     dtibs55ds       e ds
003100050110     dtibs69ds       e ds
003200050110     dcnaco          e ds                  extname(cnaco00f)
003300050110     dcnind          e ds                  extname(cnind00f)
003400050110     dcnclp          e ds                  extname(cnclp00f)
003500050110     dFncls          e ds                  extname(Fncls00f)
003600050111    >D TABELDS       E DS                  extname(TABEL00f)
003700081203     D x               S              4  0
003800140414     D exv1cclig       S                   like(v1cclig)
003900020603      *
004000020531     d NotUsed         S             16A
004100020531     d ReturnSize      S              9B 0
004200020531     d SizeList        S              9B 0
004300020531     d WrkSort         S              1
004400020531     d*---
004500081203     d Rec1            S              4  0
004600081203     d stmrec1         S              4  0
004700020531     d*---
004800020531     d MaxKey          C                   2
004900020531     d Ascendente      C                   1
005000020531     d Discendente     C                   2
005100020531     d Carattere       C                   6
005200020531     d Put             C                   1
005300020531     d EndPut          C                   2
005400020531     d Get             C                   3
005500020603     d Numerico        C                   8
005600020531     d*---
005700020531     d SflRcd          DS
005800140402     d  hin89
005900020531     d  Scelta
006000050110     d  cliv
006100050110     d  parv
006200050110     d  ragv
006300020531     d  Selected                      1A
006400140401      *
006500140402     d stamp1          ds
006600140402     d  lin1                 367    368b 0
006700140402      *
006800140401     d cli             s              7  0 dim(10)
006900140401     d clid            s             44    dim(10)
007000050110     d*
007100020531      /COPY QSYSINC/QRPGLESRC,QLGSORT
007200020531     d QLGKL                         16    DIM(MaxKey)
007300020531     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
007400020531     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
007500020531     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
007600020531     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
007700020531      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
007800020531      /COPY QSYSINC/QRPGLESRC,QUSEC
007900140512     D Ctas            C                   CONST('/TABEL00F)')
008000140512     D OVRTAb          S              1    DIM(39) CTDATA PERRCD(39)
008100170201     D OVRGRU          S              1    DIM(39) CTDATA PERRCD(39)
008200140401      *---------------------------------------------------------
008300140401     itb7ud2
008400140401     i              V1Cli0                      cli(1)
008500140401     i              V1Cli1                      cli(2)
008600140401     i              V1Cli2                      cli(3)
008700140401     i              V1Cli3                      cli(4)
008800140401     i              V1Cli4                      cli(5)
008900140401     i              V1Cli5                      cli(6)
009000140401     i              V1Cli6                      cli(7)
009100140401     i              V1Cli7                      cli(8)
009200140401     i              V1Cli8                      cli(9)
009300140401     i              V1Cli9                      cli(10)
009400140401     i              V1Cli0d                     clid(1)
009500140401     i              V1Cli1d                     clid(2)
009600140401     i              V1Cli2d                     clid(3)
009700140401     i              V1Cli3d                     clid(4)
009800140401     i              V1Cli4d                     clid(5)
009900140401     i              V1Cli5d                     clid(6)
010000140401     i              V1Cli6d                     clid(7)
010100140401     i              V1Cli7d                     clid(8)
010200140401     i              V1Cli8d                     clid(9)
010300140401     i              V1Cli9d                     clid(10)
010400140401      *
010500140401      *---------------------------------------------------------
010600140401
010700140402     c                   z-add     99            lin1
010800140408     c                   move      'S'           v1cclig
010900140408     c                   seton                                        50
011000140402     c     inizio        tag
011100140401     C                   eval      tblcod='7U'
011200140401     C                   eval      tblKUT=1
011300140401     C*---------- PULIZIA SLFCT :                          *
011400140401     C                   MOVE      *BLANKS       SCELTA
011500140401     C                   Z-ADD     0             REC1
011600140401     C                   SETON                                            70
011700140401     C                   WRITE     tb7uDCT
011800140408     C                   SETOFF                                       7071
011900140401     C* posizionamento per rag. soc.
012000140401     C                   Z-ADD     *zeros        Rec1
012100140401     C                   WRITE     tb7uD1
012200140402     c                   exsr      carica
012300140506      * forza ordinamento per ordine alfabetico
012400140506     c                   if        ordw = 'A'
012500140506     c                   seton                                        50
012600140506     c                   setoff                                       51
012700140506     c                   exsr      f11ges
012800140506     c                   endif
012900140401     C*         - EMISSIONE SFL :                          *
013000140401
013100050110     C                   do        *hival
013200140507   3aC                   IF        c1ccli <> *blank or
013300140507     C                             c1crag <> *blank
013400140507      * Controllo la scelta
013500140507     C                   seton                                        72
013600140507   2aC                   DO        *hival        X
013700140507     C     X             CHAIN     tb7uDSF                            91
013800140507     C   91              LEAVE
013900140507      *
014000140507   3aC                   IF        (*in50 = *on and C1Ccli <= cliv) or
014100140507     C                             (*in51 = *on and C1Crag <= ragv)
014200140507     C                   z-add     X             RcdNbr
014300140507     C                   setoff                                       72
014400140507   3eC                   ELSE
014500140507     C                   ITER
014600140507     C                   ENDIF
014700140507     C                   LEAVE
014800140507   2-C                   ENDDO
014900140507     C                   ENDIF
015000140507      *
015100140424     C                   WRITE     tb7uD1
015200140402     C                   WRITE     tb7uZ1
015300140401     C                   EXFMT     tb7uDCT
015400140401     C   Kc              exsr      fine
015500140402     c                   setoff                                       28
015600050110      *
015700140408     C* F07 = cliente,
015800140408if  1C                   IF        *INKg
015900050110     C                   EXSR      F11GES
016000050110e   1C                   ENDIF
016100140401      * nuovi codici
016200140402if  1C                   IF        *INKi
016300140402     C                   EXSR      stampa
016400140402     c                   eval      $msg = 'Elenco stampato '
016500140429     c                   feod      sysprt
016600140429     c                   z-add     99            lin1
016700140402     c                   seton                                        28
016800140402     c                   goto      inizio
016900140401e   1C                   ENDIF
017000140402if  1C                   IF        *INKj
017100140408     c                   clear                   cli
017200140408     c                   clear                   clid
017300140402     C                   EXSR      inserimento
017400140402     c                   clear                   cli
017500140402     c                   clear                   clid
017600140402     c                   goto      inizio
017700140402e   1C                   ENDIF
017800140414if  1C                   IF        v1cclig <> exv1cclig
017900140414     c                   goto      inizio
018000140414e   1C                   ENDIF
018100050110      *
018200050110     C                   EXSR      CTR
018300140428     c                   if        ricarica <> *blank
018400140428     c                   clear                   ricarica
018500140428     C                   goto      inizio
018600140428     c                   endif
018700050110     C                   enddo
018800140402     c*--------------------------------------------------------
018900140402     c     carica        begsr
019000140402     c*--------------------------------------------------------
019100140402      * carica sfl
019200140402     C     ktab          setll     tabel00f
019300140402     c                   do        *hival
019400140402     C     ktab          reade     tabel00f
019500140402     c                   if        %eof(tabel00f)
019600140402     c                   leave
019700140402     c                   end
019800140402     c                   if        tblflg <> *blanks
019900140402     c                   iter
020000140402     c                   end
020100140402     c                   movel     tblkey        filclia           3
020200140403     c                   move      filclia       filcli            3
020300140408     c                   movel     tbluni        ds7u
020400140402     c                   if        §7usre <> 'S'
020500140402     c                   iter
020600140402     c                   endif
020700140408     c     filcli        lookup    lin                                    89
020800140408     c                   move      *in89         hin89
020900140408     c                   if        v1cclig = 'S' and not *in89
021000140408     c                   iter
021100140408     c                   endif
021200140402     C                   EXSR      RIESFL
021300140507   3aC*                  IF        cliv  <= c1ccli  or
021400140507     C*                            ragv <= c1crag
021500140507     C*                  ITER
021600140507     C*                  ENDIF
021700140402     C     REC1          ADD       1             REC1                 71
021800140402     C                   WRITE     tb7uDSF
021900140402     c                   enddo
022000140402     c*
022100140402     C                   Z-ADD     REC1          STMREC1
022200140402     C                   EVAL      RcdNbr  = 1
022300140414     c                   eval      exv1cclig = v1cclig
022400140402     c                   endsr
022500020603      *-------------------------------------------------------------------------
022600020603      * Valorizzo campo tipo flag tipo p.o
022700020603      *-------------------------------------------------------------------------
022800020603     C     RIESFL        BEGSR
022900050110     c*
023000050110     c                   movel     tblkey        cliv
023100050110     c                   eval      parv = %subst(tblkey: 8: 1)
023200050110     C                   CLEAR                   TIBS69ds
023300050110     C                   CLEAR                   CNACO
023400050110     C                   CLEAR                   CNIND
023500050110     C                   CLEAR                   CNCLP
023600050110     C                   CLEAR                   FNCLS
023700050110     C                   MOVEL     ' '           I69TLA
023800050110     C                   MOVEL     KNSIF         I69SIF
023900050110     C                   MOVE      cliv          I69KAC
024000050110     C                   MOVE      cliv          I69Kcp
024100050110     C                   CALL      'TIBS69R'
024200050110     C                   PARM                    TIBS69ds
024300050110     C                   PARM                    CNACO
024400050110     C                   PARM                    CNIND
024500050110     C                   PARM                    CNCLP
024600050110     C                   PARM                    FNCLS
024700050110     C                   if        o69err <>*blanks
024800050110     C                   EVAL      ragv = *ALL'?'
024900050110     C                   ELSE
025000050110     C                   MOVEL     ACORAG        ragv
025100050110     C                   END
025200050110     c*
025300020603     C                   ENDSR
025400140401      *-------------------------------------------------------------------------
025500140401      *  inserimento
025600140401      *-------------------------------------------------------------------------
025700140401     C     inserimento   BEGSR
025800140401     c*
025900140401     c                   do        *hival
026000140401     C                   EXFMT     tb7ud2
026100140401     C   KL              leave
026200140401     c                   setoff                                       9028
026300140401     c                   movea     '0000000000'  *in(40)
026400140401     c     1             do        10            y                 2 0
026500140401     c                   if        cli(y) > 0
026600140401     c                   exsr      contrins
026700140401     c                   endif
026800140401     c                   enddo
026900140401     c   90              iter
027000140401     c                   if        *inkf
027100140401     c     1             do        10            y
027200140401     c                   if        cli(y) > 0
027300140401     c                   exsr      scrivi
027400140401     c                   endif
027500140401     c                   enddo
027600140402     c                   leave
027700140401     c                   endif
027800140401     c                   enddo
027900140401     c                   endsr
028000140401      *-------------------------------------------------------------------------
028100140401      * Controllo inserimento
028200140401      *-------------------------------------------------------------------------
028300140401     C     contrins      BEGSR
028400140401      * verifico se in gestione
028500140403     c                   movel     cli(y)        pocli             3
028600140401     c     pocli         lookup    lin                                    89
028700140403     c                   if        not *in89
028800140401     c                   z-add     39            xx                2 0
028900140401     c                   add       y             xx
029000140401     C                   EVAL      *in(xx) = *on
029100140401     c                   eval      $msg = 'Filiale cliente non in gestione'
029200140401     c                   seton                                        2890
029300140401     c                   leavesr
029400140401     c                   endif
029500140401     c* controllo cliente esistente
029600140402     c                   movel     cli(y)        cliv
029700140401     C                   CLEAR                   TIBS69ds
029800140401     C                   CLEAR                   CNACO
029900140401     C                   CLEAR                   CNIND
030000140401     C                   CLEAR                   CNCLP
030100140401     C                   CLEAR                   FNCLS
030200140401     C                   MOVEL     ' '           I69TLA
030300140401     C                   MOVEL     KNSIF         I69SIF
030400140401     C                   MOVE      cliv          I69KAC
030500140401     C                   MOVE      cliv          I69Kcp
030600140401     C                   CALL      'TIBS69R'
030700140401     C                   PARM                    TIBS69ds
030800140401     C                   PARM                    CNACO
030900140401     C                   PARM                    CNIND
031000140401     C                   PARM                    CNCLP
031100140401     C                   PARM                    FNCLS
031200140401     C                   if        o69err <>*blanks
031300140401     c                   z-add     39            xx                2 0
031400140401     c                   add       y             xx
031500140402     C                   EVAL      clid(y) = *ALL'?'
031600140401     C                   EVAL      *in(xx) = *on
031700140401     c                   eval      $msg = 'Codice cliente inesistente'
031800140401     c                   seton                                        2890
031900140401     c                   leavesr
032000140401     C                   ELSE
032100140402     C                   MOVEL     ACORAG        clid(y)
032200140401     C                   END
032300140401     c* controllo che non sia già in tabella
032400140402     c                   movel     cli(y)        tblkey
032500140401     c                   move      'Q'           tblkey
032600140401     c     ktabel        chain     tabel00f
032700140401     c                   if        %found(tabel00f)
032800140401     c                   movel     tbluni        ds7u
032900140401     c                   if        §7usre <> *blank
033000140401     c                   z-add     39            xx                2 0
033100140401     c                   add       y             xx
033200140401     C                   EVAL      *in(xx) = *on
033300140401     c                   eval      $msg = 'Codice Già presente in tabella'
033400140401     c                   seton                                        2890
033500140401     c                   leavesr
033600140401     c                   endif
033700140401     c                   endif
033800140401     c*
033900140401     C                   ENDSR
034000140401     c*--------------------------------------------------------------
034100140401     c     scrivi        begsr
034200140401     c*--------------------------------------------------------------
034300140402     c                   movel     cli(y)        tblkey
034400140401     c                   move      'Q'           tblkey
034500140401     c     ktabel        chain     tabel00f
034600170201      ***
034700140401      *aggiorna già presente per particolarità
034800140401     c                   if        %found(tabel00f)
034900170201      ***
035000140401     c                   movel     tbluni        ds7u
035100140401     c                   move      'S'           §7usre
035200140401     C                   MOVEL     DS7U          tbluni
035300140401     C                   MOVEL     '1'           tblftt
035400140401     C                   CLEAR                   tblftr
035500140401     C                   CLEAR                   tbldtr
035600140401     C                   UPDATE    TABEL
035700170201      ***
035800140401     c                   else
035900140401      *scrive
036000140401     c                   clear                   ds7u
036100140401     c                   move      'S'           §7usre
036200140401     C                   MOVEL     DS7U          tbluni
036300140401     C                   MOVEL     '1'           tblftt
036400140401     C                   CLEAR                   tblftr
036500140401     C                   CLEAR                   tbldtr
036600140401     C                   Z-ADD     1             tblkut
036700140401     C                   MOVEL     '7U'          tblcod
036800140402     c                   movel     cli(y)        tblkey
036900140401     c                   move      'Q'           tblkey
037000140401     C                   WRITE     TABEL
037100170201      ***
037200140401     c                   endif
037300170201      ***
037400170201     c     ktabel        chain     tabelX0F
037500170201     c                   If        %found(tabelX0F)
037600170201     c                   eval      xTBLuni = TBLuni
037700170201     C                   UPDATE    TABELx
037800170201      *
037900170201     c                   Else
038000170201      *
038100170201     C                   MOVEL     DS7U          xtbluni
038200170201     C                   MOVEL     '1'           xtblftt
038300170201     C                   CLEAR                   xtblftr
038400170201     C                   CLEAR                   xtbldtr
038500170201     C                   Z-ADD     1             xtblkut
038600170201     C                   MOVEL     '7U'          xtblcod
038700170201     c                   movel     cli(y)        xtblkey
038800170201     c                   move      'Q'           xtblkey
038900170201     c                   eval      xTBLuni = TBLuni
039000170201     C                   WRITE     TABELx
039100170201      *
039200170201     c                   End
039300170201      *
039400140401     c                   endsr
039500140402     c*--------------------------------------------------------------
039600140402     c     toglie        begsr
039700140402     c*--------------------------------------------------------------
039800140408     c                   seton                                        52
039900140424     c                   do        *hival
040000140408     c                   exfmt     tb7ud2
040100140408     c   kl              leavesr
040200140408     c                   if        *inkf
040300140408     c     1             do        10            y
040400140408     c                   movel     cli(y)        tblkey
040500140408     c                   move      'Q'           tblkey
040600140408      * se codice = 0 passo al successivo
040700140408     c                   if        cli(y) = 0
040800140408     c                   iter
040900140408     c                   endif
041000140402     c     ktabel        chain     tabel00f
041100140402      *aggiorna già presente per particolarità
041200140402     c                   if        %found(tabel00f)
041300140402     c                   movel     tbluni        ds7u
041400140402     c                   move      *blank        §7usre
041500140402     C                   MOVEL     DS7U          tbluni
041600140402     C                   MOVEL     '1'           tblftt
041700140402     C                   CLEAR                   tblftr
041800140402     C                   CLEAR                   tbldtr
041900140402     C                   UPDATE    TABEL
042000170201      *
042100170201     c     ktabel        chain     tabelX0f
042200170201     c                   If        %found(tabelX0f)
042300170201     c                   eval      xTBLuni = TBLuni
042400170201     C                   UPDATE    TABELx
042500170201     c                   End
042600170201      *
042700140428     c                   move      'X'           ricarica          1
042800140402     c                   endif
042900140408     c                   enddo
043000170201      *
043100140424     c                   leave
043200140408     c                   endif
043300140424     c                   enddo
043400140402     c                   endsr
043500020531      *-------------------------------------------------------------------------
043600020531      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/P.O
043700020531      *-------------------------------------------------------------------------
043800020531     C     F11GES        BEGSR
043900140409     C                   setoff                                         72
044000140507     c*                  select
044100140507     c*                  when      *in50
044200140507     C*                  CLEAR                   C1Crag
044300140507     c*                  when      *in51
044400140507     C*                  CLEAR                   C1Ccli
044500140507     c*                  endsl
044600020709      * IMPOSTO ENTRATA AD 'A'
044700020613     C                   EVAL      rcdnbr  = 1
044800020531      *
044900020531     C                   CLEAR                   QLGSCB
045000020531     C                   CLEAR                   QLGSCB00
045100020531
045200020531     ** 1 campo chiave.
045300020531     C                   EVAL      QLGNBRK    = 1
045400020531
045500050110     c                   select
045600140409     C                   when      *in51
045700050111     ** IL cliente è a posizione 2, numerico, 3 byte, ascending.
045800050110     C                   EVAL      QLGSP      = 2
045900050110     C                   EVAL      QLGSS      = %SIZE(cliv)
046000050110     C                   EVAL      QLGDT      = CARATTERE
046100020531     C                   EVAL      QLGSO      = Ascendente
046200020531     C                   EVAL      QLGKL(1)   = QLGSKL
046300140506     c                   clear                   ordw              1
046400140409     C                   setoff                                       51
046500140409     C                   seton                                        50
046600050110     ** La ragione sociale è a posizione 10, 25 byte, char, ascending.
046700140409     c                   when      *in50
046800050110     C                   EVAL      QLGSP      = 10
046900050110     C                   EVAL      QLGSS      = %SIZE(ragv)
047000020603     C                   EVAL      QLGDT      = Carattere
047100020603     C                   EVAL      QLGSO      = Ascendente
047200020603     C                   EVAL      QLGKL(1)   = QLGSKL
047300140506     c                   move      'A'           ordw
047400140409     C                   setoff                                       50
047500140409     C                   seton                                        51
047600050110     C                   ENDsl
047700020531
047800020531      * Load other sort parameters.
047900020531     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
048000020531     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
048100020531     C                   EVAL      QLGRT     = 8
048200020531     C                   EVAL      QLGOKL    = 80
048300020531     C                   EVAL      QLGLKE    = 16
048400020531     C                   EVAL      QLGLSS    = 290
048500020531
048600020531      * Initialize Sort I/O API fields.
048700020531     C                   EVAL      QLGRL00  = QLGRL
048800020531     C                   EVAL      QLGRC00  = 1
048900020531     C                   CLEAR                   QUSEI
049000020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
049100020531
049200020531      * First step - Initialize the sort routine.
049300020531     C                   CALL      'QLGSORT'
049400020531     C                   PARM                    QLGSCB
049500020531     C                   PARM                    NotUsed
049600020531     C                   PARM                    NotUsed
049700020531     C                   PARM                    SizeList
049800020531     C                   PARM                    ReturnSize
049900020531     C                   PARM                    QUSEC
050000020531
050100020531      * Next step - Write records to I/O routine.
050200020531     C                   EVAL      QLGRT00 = Put
050300020531
050400050111     C                   DO        STMREC1       x
050500020531
050600140401     C     x             CHAIN     tb7uDSF
050700020531
050800020531     ** Solo le righe con Selected = 'Y' sono riordinate,
050900020531     ** quindi per fare un ordinamento di tutte le righe
051000020531     ** metto 'Y' sempre.
051100020531     C                   EVAL      Selected = 'Y'
051200020531
051300020531     C                   CLEAR                   QUSEI
051400020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
051500020531     C                   CALL      'QLGSRTIO'
051600020531     C                   PARM                    QLGSCB00
051700020531     C                   PARM                    SflRcd
051800020531     C                   PARM                    NotUsed
051900020531     C                   PARM                    SizeList
052000020531     C                   PARM                    NotUsed
052100020531     C                   PARM                    QUSEC
052200020531
052300020531     C                   ENDDO
052400020531
052500020531      * Next step - Signal end of input, clear subfile for reload.
052600020531     C                   EVAL      QLGRT00 = EndPut
052700020531     C                   CLEAR                   QUSEI
052800020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
052900020531
053000020531     C                   CALL      'QLGSRTIO'
053100020531     C                   PARM                    QLGSCB00
053200020531     C                   PARM                    SflRcd
053300020531     C                   PARM                    NotUsed
053400020531     C                   PARM                    SizeList
053500020531     C                   PARM                    NotUsed
053600020531     C                   PARM                    QUSEC
053700020531
053800020531     C                   SETON                                            70
053900140401     C                   WRITE     tb7uDCT
054000020603     C                   SETOFF                                       70
054100140401     C                   WRITE     tb7uD1
054200140401     C                   WRITE     tb7uZ1
054300020531
054400020531      * Final step - Write the records back to the subfile.
054500020531     C                   EVAL      QLGRT00 = Get
054600020531
054700020613     C                   DO        STMREC1       x
054800020531     C                   CLEAR                   QUSEI
054900020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
055000020531     C                   CALL      'QLGSRTIO'
055100020531     C                   PARM                    QLGSCB00
055200020531     C                   PARM                    NotUsed
055300020531     C                   PARM                    SflRcd
055400020531     C                   PARM                    QLGRL00
055500020531     C                   PARM                    NotUsed
055600020531     C                   PARM                    QUSEC
055700140402     c                   move      hin89         *in89
055800020613     c                   z-add     x             rec1
055900140401     C                   WRITE     tb7uDSF
056000020531     C                   ENDDO
056100020531
056200020531     C                   EVAL      *IN70 = *off
056300020604      *
056400020531     C                   ENDSR
056500020606      *-------------------------------------------------------------------------
056600020613      * CTR Subroutine per il riposizionamento
056700020606      *-------------------------------------------------------------------------
056800020606     C     CTR           BEGSR
056900020606     C*
057000140408     c                   clear                   vuoto             7 0
057100140408     c                   clear                   selez             1
057200020612     C*
057300140408   2aC     1             DO        *hival        X
057400140408     C     X             CHAIN     tb7uDSF                            91
057500140408     C   91              LEAVE
057600140408   1aC                   IF        scelta = '1' and z < 10
057700140408     c                   z-add     1             z                 2 0
057800140408     c     vuoto         lookup    cli(z)                                 53
057900140408     C                   MOVEL     cliv          cli(z)
058000140408     C                   MOVEl     ragv          clid(z)
058100140408     c                   move      'X'           selez
058200140408     c                   endif
058300140408     c                   enddo
058400140408     c                   if        selez <> *blank
058500140408     C                   EXSR      toglie
058600140408     c                   setoff                                       52
058700140408     c                   clear                   cli
058800140408     c                   clear                   clid
058900140402     c                   leavesr
059000140408     c                   endif
059100020710      * riemetto se non ho trovato records
059200140408     C   72              DO
059300140408     C                   WRITE     tb7uZ1
059400140408     C                   LEAVE
059500140408     C                   ENDDO
059600020613      *
059700020606     C                   ENDSR
059800140402     c*--------------------------------------------------------
059900140402     c     stampa        begsr
060000140402     c*--------------------------------------------------------
060100140402      * rilegge per stampare elenco
060200140402     C     ktab          setll     tabel00f
060300140402     c                   do        *hival
060400140402     C     ktab          reade     tabel00f
060500140402     c                   if        %eof(tabel00f)
060600140402     c                   leave
060700140402     c                   end
060800140402     c                   if        tblflg <> *blanks
060900140402     c                   iter
061000140402     c                   end
061100140402     c                   movel     tblkey        filclia           3
061200140403     c                   move      filclia       filcli
061300140402     c     filcli        lookup    lin                                    88
061400140402     c  n88              iter
061500140402     c                   movel     tbluni        ds7u
061600140402     c                   if        §7usre <> 'S'
061700140402     c                   iter
061800140402     c                   endif
061900140402     c                   exsr      riesfl
062000140408     C                   if        lin1 > 60
062100140402     C                   except    testa
062200140402     c                   endif
062300140402     c*
062400140402     C                   except    dettaglio
062500140402     c                   enddo
062600140402     c                   endsr
062700140401     C*----------------------------------------------------*
062800140401     C     fine          BEGSR
062900140401     C*----------------------------------------------------*
063000140401      * Non ho scelto nulla
063100140401     C                   SETON                                            LR
063200140401     c                   return
063300140401     c                   endsr
063400140401     C*------------------------------------------------------------------------*
063500140401     C     *inzsr        begsr
063600140401     C*------------------------------------------------------------------------*
063700140401     C     *ENTRY        PLIST
063800140401     C                   PARM                    Kpjba
063900140401     c     ktab          klist
064000140401     c                   kfld                    tblkut
064100140401     c                   kfld                    tblcod
064200140401     c     ktabel        klist
064300140401     c                   kfld                    tblkut
064400140401     c                   kfld                    tblcod
064500140401     c                   kfld                    tblkey
064600140401     c                   exsr      sr_Datijob
064700140403      *
064800140403     c                   movel     utefaf        dute01
064900140403     c                   if        §uteabres = 'NO'
065000140403     c                   exfmt     tb7unoab
065100140403     c                   exsr      fine
065200140403     c                   endif
065300140403      * Reperimento dei P.O. gestibili dall'utente
065400140403     c                   clear                   TRUL31DS
065500140408      *se autorizzazione specifica non indicata utilizzo autorizzazione generica
065600140408     c                   if        §uteabres = *blank
065700140408     c                   eval      I31abi = uteaut
065800140408     c                   else
065900140403     c                   eval      I31abi = §uteabres
066000140408     c                   endif
066100140403     c                   eval      I31cdi = DUTdis
066200140403     c                   eval      I31car = DUTare
066300140403     c                   eval      I31cpo = DUTpou
066400140403     c                   call      'TRUL31R'
066500140403     c                   parm                    KPJBA
066600140403     c                   parm                    TRUL31DS
066700140403     c                   if        o31pog > *zeros
066800140403     c                   movea     o31pog        Lin
066900140403     c                   endif
067000140401     C*
067100140429     c                   if        dutpou <> 046
067200140512     C                   CLEAR                   tibs55ds
067300140512     c                   movel     'L'           i50tla
067400140512     c                   Movel     046           I50apo
067500140512     C                   CALL      'TIBS55R'
067600140512     C                   PARM                    tibs55ds
067700170201     C*
067800170201     c                   if        o50alg  =  o50plg
067900170201     c                   exfmt     tb7unoSEDE
068000170201     c                   exsr      fine
068100170201     c                   end
068200140512     C* overdbf
068300140512     c                   movea     o50alg        ovrtab(30)
068400140512     C                   SETOFF                                       01
068500140512     C                   CLEAR                   COMMAN           80
068600140512     C                   MOVEA     ovrtab        COMMAN
068700140512     c                   eval      comman=%trim(comman)+ctas
068800140512     C                   CALL      'QCMDEXC'
068900140512     C                   PARM                    COMMAN
069000140512     C                   PARM      50            LUNG             15 5
069100140402     C*
069200170201     C* overdbf
069300170201     C*   x tab di Sede - POICHÈ il programma GIRA in FILIALE
069400170201     c                   movea     o50plg        ovrgru(30)
069500170201     C                   CLEAR                   COMMAN           80
069600170201     C                   MOVEA     ovrgru        COMMAN
069700170201     c                   eval      comman=%trim(comman)+ctas
069800170201     C                   CALL      'QCMDEXC'
069900170201     C                   PARM                    COMMAN
070000170201     C                   PARM      50            LUNG             15 5
070100170201     c                   endif
070200170201     C*
070300170201     c                   open      tabel00f
070400170201     c                   open      tabelx0f
070500170201     C*
070600140401     c                   endsr
070700140401      *---------------------------------------------------------------*
070800140401      * Reperimento dati del job (Utente/Operativi)                   *
070900140401      *---------------------------------------------------------------*
071000140401     c     sr_DatiJob    BEGSR
071100140401      *
071200140401     c     *dtaara       define    §azute        AZUTEds
071300140401     c     *dtaara       define    §datiute      dDatiUte
071400140401      *
071500140401     c                   in(E)     *dtaara
071600140401     c                   IF        %Error or RSUT = *blanks
071700140401     c                   clear                   TIBS34ds
071800140401     c                   call      'TIBS34R'
071900140401     c                   parm                    TIBS34ds
072000140401     c                   in        *dtaara
072100140401     c                   ENDIF
072200140401      *
072300140401     c                   ENDSR
072400140402     O*------------------------------------------------------*
072500140402     Osysprt    e            TESTA             2
072600140402     o                       knsif               10
072700140402     o                       knmus               22
072800140408     o                                          105 'TRTB7UR'
072900140408     o                                          112 'Pag.'
073000140408     o                       page          z    118
073100140402     o                       *date         y    130
073200140408     O                                           95 'Stampa Elenco clienti ab-
073300140402     O                                              ilitati ai resi in -
073400140402     o                                              periodo di trazioni ridotte'
073500140402     O          e            dettaglio   2
073600140402     O                       cliv                10
073700140402     O                       ragv                60
073800140402      *-------------------------------------------------------------------------
073900140512**         OVRTAB
074000140512OVRDBF FILE(Tabel00f) TOFILE(XXXXXXXXXX
074100170201**         OVRGRU
074200170201OVRDBF FILE(TABELX0F) TOFILE(XXXXXXXXXX
