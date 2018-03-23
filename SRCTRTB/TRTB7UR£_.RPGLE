000100971203     H*------------------------------------------------------------------------*
000200050110     H* RICERCA Abil.clienti gestione particolarità
000300971203     H*------------------------------------------------------------------------*
000400971203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500971203     F*------------------------------------------------------------------------*
000600971203     F* DATA BASE
000700971203     F*------------------------------------------------------------------------*
000800140402     Ftabel00f  uF a E           K DISK    usropn
000900140401     FTrtb7uD   CF   E             WORKSTN
001000140401     F                                     SFILE(tb7uDSF:REC1)
001100140402     fsysprt    o    f  132        printer infds(stamp1)
001200020531     d*-------------------------------------------------------------------------
001300020531     d* Data structure
001400020531     d*-------------------------------------------------------------------------
001500140401     d kpjba         e ds
001600140401      * - Parametri per richiamo al pgm di controllo profilo utenti
001700140401     d TIBS34ds      e ds
001800140401      * - Ds di riferimento al file esterno AzUte00F
001900140401     d AZUTEds       e ds                  ExtName(AzUte00F)
002000140401      * - Ds per dati organigramma
002100140401     d dDatiUte      e ds
002200140403     D dute01        e ds                  inz
002300140403       // -?Caricamento filiali gestite dall'utente?
002400140403     d TRUL31ds      e ds                  inz
002500140403     d  lin            s              3    inz   dim(250)
002600140403      *
002700050110     dDS7U           e ds
002800050110     dtibs69ds       e ds
002900050110     dcnaco          e ds                  extname(cnaco00f)
003000050110     dcnind          e ds                  extname(cnind00f)
003100050110     dcnclp          e ds                  extname(cnclp00f)
003200050110     dFncls          e ds                  extname(Fncls00f)
003300050111    >D TABELDS       E DS                  extname(TABEL00f)
003400081203     D x               S              4  0
003500140414     D exv1cclig       S                   like(v1cclig)
003600020603      *
003700020531     d NotUsed         S             16A
003800020531     d ReturnSize      S              9B 0
003900020531     d SizeList        S              9B 0
004000020531     d WrkSort         S              1
004100020531     d*---
004200081203     d Rec1            S              4  0
004300081203     d stmrec1         S              4  0
004400020531     d*---
004500020531     d MaxKey          C                   2
004600020531     d Ascendente      C                   1
004700020531     d Discendente     C                   2
004800020531     d Carattere       C                   6
004900020531     d Put             C                   1
005000020531     d EndPut          C                   2
005100020531     d Get             C                   3
005200020603     d Numerico        C                   8
005300020531     d*---
005400020531     d SflRcd          DS
005500140402     d  hin89
005600020531     d  Scelta
005700050110     d  cliv
005800050110     d  parv
005900050110     d  ragv
006000020531     d  Selected                      1A
006100140401      *
006200140402     d stamp1          ds
006300140402     d  lin1                 367    368b 0
006400140402      *
006500140401     d cli             s              7  0 dim(10)
006600140401     d clid            s             44    dim(10)
006700050110     d*
006800020531      /COPY QSYSINC/QRPGLESRC,QLGSORT
006900020531     d QLGKL                         16    DIM(MaxKey)
007000020531     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
007100020531     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
007200020531     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
007300020531     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
007400020531      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
007500020531      /COPY QSYSINC/QRPGLESRC,QUSEC
007600140402     D Cmd             S             49
007700140402     D Cmd1            S             49    dim(1) ctdata perrcd(1)
007800140402     D Cmd2            S             49    dim(1) ctdata perrcd(1)
007900140401      *---------------------------------------------------------
008000140401     itb7ud2
008100140401     i              V1Cli0                      cli(1)
008200140401     i              V1Cli1                      cli(2)
008300140401     i              V1Cli2                      cli(3)
008400140401     i              V1Cli3                      cli(4)
008500140401     i              V1Cli4                      cli(5)
008600140401     i              V1Cli5                      cli(6)
008700140401     i              V1Cli6                      cli(7)
008800140401     i              V1Cli7                      cli(8)
008900140401     i              V1Cli8                      cli(9)
009000140401     i              V1Cli9                      cli(10)
009100140401     i              V1Cli0d                     clid(1)
009200140401     i              V1Cli1d                     clid(2)
009300140401     i              V1Cli2d                     clid(3)
009400140401     i              V1Cli3d                     clid(4)
009500140401     i              V1Cli4d                     clid(5)
009600140401     i              V1Cli5d                     clid(6)
009700140401     i              V1Cli6d                     clid(7)
009800140401     i              V1Cli7d                     clid(8)
009900140401     i              V1Cli8d                     clid(9)
010000140401     i              V1Cli9d                     clid(10)
010100140401      *
010200140401      *---------------------------------------------------------
010300140401
010400140402     c                   z-add     99            lin1
010500140408     c                   move      'S'           v1cclig
010600140408     c                   seton                                        50
010700140402     c     inizio        tag
010800140401     C                   eval      tblcod='7U'
010900140401     C                   eval      tblKUT=1
011000140401     C*---------- PULIZIA SLFCT :                          *
011100140401     C                   MOVE      *BLANKS       SCELTA
011200140401     C                   Z-ADD     0             REC1
011300140401     C                   SETON                                            70
011400140401     C                   WRITE     tb7uDCT
011500140408     C                   SETOFF                                       7071
011600140401     C* posizionamento per rag. soc.
011700140401     C                   Z-ADD     *zeros        Rec1
011800140401     C                   WRITE     tb7uD1
011900140402     c                   exsr      carica
012000140506      * forza ordinamento per ordine alfabetico
012100140506     c                   if        ordw = 'A'
012200140506     c                   seton                                        50
012300140506     c                   setoff                                       51
012400140506     c                   exsr      f11ges
012500140506     c                   endif
012600140401     C*         - EMISSIONE SFL :                          *
012700140401
012800050110     C                   do        *hival
012900140507   3aC                   IF        c1ccli <> *blank or
013000140507     C                             c1crag <> *blank
013100140507      * Controllo la scelta
013200140507     C                   seton                                        72
013300140507   2aC                   DO        *hival        X
013400140507     C     X             CHAIN     tb7uDSF                            91
013500140507     C   91              LEAVE
013600140507      *
013700140507   3aC                   IF        (*in50 = *on and C1Ccli <= cliv) or
013800140507     C                             (*in51 = *on and C1Crag <= ragv)
013900140507     C                   z-add     X             RcdNbr
014000140507     C                   setoff                                       72
014100140507   3eC                   ELSE
014200140507     C                   ITER
014300140507     C                   ENDIF
014400140507     C                   LEAVE
014500140507   2-C                   ENDDO
014600140507     C                   ENDIF
014700140507      *
014800140424     C                   WRITE     tb7uD1
014900140402     C                   WRITE     tb7uZ1
015000140401     C                   EXFMT     tb7uDCT
015100140401     C   Kc              exsr      fine
015200140402     c                   setoff                                       28
015300050110      *
015400140408     C* F07 = cliente,
015500140408if  1C                   IF        *INKg
015600050110     C                   EXSR      F11GES
015700050110e   1C                   ENDIF
015800140401      * nuovi codici
015900140402if  1C                   IF        *INKi
016000140402     C                   EXSR      stampa
016100140402     c                   eval      $msg = 'Elenco stampato '
016200140429     c                   feod      sysprt
016300140429     c                   z-add     99            lin1
016400140402     c                   seton                                        28
016500140402     c                   goto      inizio
016600140401e   1C                   ENDIF
016700140402if  1C                   IF        *INKj
016800140408     c                   clear                   cli
016900140408     c                   clear                   clid
017000140402     C                   EXSR      inserimento
017100140402     c                   clear                   cli
017200140402     c                   clear                   clid
017300140402     c                   goto      inizio
017400140402e   1C                   ENDIF
017500140414if  1C                   IF        v1cclig <> exv1cclig
017600140414     c                   goto      inizio
017700140414e   1C                   ENDIF
017800050110      *
017900050110     C                   EXSR      CTR
018000140428     c                   if        ricarica <> *blank
018100140428     c                   clear                   ricarica
018200140428     C                   goto      inizio
018300140428     c                   endif
018400050110     C                   enddo
018500140402     c*--------------------------------------------------------
018600140402     c     carica        begsr
018700140402     c*--------------------------------------------------------
018800140402      * carica sfl
018900140402     C     ktab          setll     tabel00f
019000140402     c                   do        *hival
019100140402     C     ktab          reade     tabel00f
019200140402     c                   if        %eof(tabel00f)
019300140402     c                   leave
019400140402     c                   end
019500140402     c                   if        tblflg <> *blanks
019600140402     c                   iter
019700140402     c                   end
019800140402     c                   movel     tblkey        filclia           3
019900140403     c                   move      filclia       filcli            3
020000140408     c                   movel     tbluni        ds7u
020100140402     c                   if        §7usre <> 'S'
020200140402     c                   iter
020300140402     c                   endif
020400140408     c     filcli        lookup    lin                                    89
020500140408     c                   move      *in89         hin89
020600140408     c                   if        v1cclig = 'S' and not *in89
020700140408     c                   iter
020800140408     c                   endif
020900140402     C                   EXSR      RIESFL
021000140507   3aC*                  IF        cliv  <= c1ccli  or
021100140507     C*                            ragv <= c1crag
021200140507     C*                  ITER
021300140507     C*                  ENDIF
021400140402     C     REC1          ADD       1             REC1                 71
021500140402     C                   WRITE     tb7uDSF
021600140402     c                   enddo
021700140402     c*
021800140402     C                   Z-ADD     REC1          STMREC1
021900140402     C                   EVAL      RcdNbr  = 1
022000140414     c                   eval      exv1cclig = v1cclig
022100140402     c                   endsr
022200020603      *-------------------------------------------------------------------------
022300020603      * Valorizzo campo tipo flag tipo p.o
022400020603      *-------------------------------------------------------------------------
022500020603     C     RIESFL        BEGSR
022600050110     c*
022700050110     c                   movel     tblkey        cliv
022800050110     c                   eval      parv = %subst(tblkey: 8: 1)
022900050110     C                   CLEAR                   TIBS69ds
023000050110     C                   CLEAR                   CNACO
023100050110     C                   CLEAR                   CNIND
023200050110     C                   CLEAR                   CNCLP
023300050110     C                   CLEAR                   FNCLS
023400050110     C                   MOVEL     ' '           I69TLA
023500050110     C                   MOVEL     KNSIF         I69SIF
023600050110     C                   MOVE      cliv          I69KAC
023700050110     C                   MOVE      cliv          I69Kcp
023800050110     C                   CALL      'TIBS69R'
023900050110     C                   PARM                    TIBS69ds
024000050110     C                   PARM                    CNACO
024100050110     C                   PARM                    CNIND
024200050110     C                   PARM                    CNCLP
024300050110     C                   PARM                    FNCLS
024400050110     C                   if        o69err <>*blanks
024500050110     C                   EVAL      ragv = *ALL'?'
024600050110     C                   ELSE
024700050110     C                   MOVEL     ACORAG        ragv
024800050110     C                   END
024900050110     c*
025000020603     C                   ENDSR
025100140401      *-------------------------------------------------------------------------
025200140401      *  inserimento
025300140401      *-------------------------------------------------------------------------
025400140401     C     inserimento   BEGSR
025500140401     c*
025600140401     c                   do        *hival
025700140401     C                   EXFMT     tb7ud2
025800140401     C   KL              leave
025900140401     c                   setoff                                       9028
026000140401     c                   movea     '0000000000'  *in(40)
026100140401     c     1             do        10            y                 2 0
026200140401     c                   if        cli(y) > 0
026300140401     c                   exsr      contrins
026400140401     c                   endif
026500140401     c                   enddo
026600140401     c   90              iter
026700140401     c                   if        *inkf
026800140401     c     1             do        10            y
026900140401     c                   if        cli(y) > 0
027000140401     c                   exsr      scrivi
027100140401     c                   endif
027200140401     c                   enddo
027300140402     c                   leave
027400140401     c                   endif
027500140401     c                   enddo
027600140401     c                   endsr
027700140401      *-------------------------------------------------------------------------
027800140401      * Controllo inserimento
027900140401      *-------------------------------------------------------------------------
028000140401     C     contrins      BEGSR
028100140401      * verifico se in gestione
028200140403     c                   movel     cli(y)        pocli             3
028300140401     c     pocli         lookup    lin                                    89
028400140403     c                   if        not *in89
028500140401     c                   z-add     39            xx                2 0
028600140401     c                   add       y             xx
028700140401     C                   EVAL      *in(xx) = *on
028800140401     c                   eval      $msg = 'Filiale cliente non in gestione'
028900140401     c                   seton                                        2890
029000140401     c                   leavesr
029100140401     c                   endif
029200140401     c* controllo cliente esistente
029300140402     c                   movel     cli(y)        cliv
029400140401     C                   CLEAR                   TIBS69ds
029500140401     C                   CLEAR                   CNACO
029600140401     C                   CLEAR                   CNIND
029700140401     C                   CLEAR                   CNCLP
029800140401     C                   CLEAR                   FNCLS
029900140401     C                   MOVEL     ' '           I69TLA
030000140401     C                   MOVEL     KNSIF         I69SIF
030100140401     C                   MOVE      cliv          I69KAC
030200140401     C                   MOVE      cliv          I69Kcp
030300140401     C                   CALL      'TIBS69R'
030400140401     C                   PARM                    TIBS69ds
030500140401     C                   PARM                    CNACO
030600140401     C                   PARM                    CNIND
030700140401     C                   PARM                    CNCLP
030800140401     C                   PARM                    FNCLS
030900140401     C                   if        o69err <>*blanks
031000140401     c                   z-add     39            xx                2 0
031100140401     c                   add       y             xx
031200140402     C                   EVAL      clid(y) = *ALL'?'
031300140401     C                   EVAL      *in(xx) = *on
031400140401     c                   eval      $msg = 'Codice cliente inesistente'
031500140401     c                   seton                                        2890
031600140401     c                   leavesr
031700140401     C                   ELSE
031800140402     C                   MOVEL     ACORAG        clid(y)
031900140401     C                   END
032000140401     c* controllo che non sia già in tabella
032100140402     c                   movel     cli(y)        tblkey
032200140401     c                   move      'Q'           tblkey
032300140401     c     ktabel        chain     tabel00f
032400140401     c                   if        %found(tabel00f)
032500140401     c                   movel     tbluni        ds7u
032600140401     c                   if        §7usre <> *blank
032700140401     c                   z-add     39            xx                2 0
032800140401     c                   add       y             xx
032900140401     C                   EVAL      *in(xx) = *on
033000140401     c                   eval      $msg = 'Codice Già presente in tabella'
033100140401     c                   seton                                        2890
033200140401     c                   leavesr
033300140401     c                   endif
033400140401     c                   endif
033500140401     c*
033600140401     C                   ENDSR
033700140401     c*--------------------------------------------------------------
033800140401     c     scrivi        begsr
033900140401     c*--------------------------------------------------------------
034000140402     c                   movel     cli(y)        tblkey
034100140401     c                   move      'Q'           tblkey
034200140401     c     ktabel        chain     tabel00f
034300140401      *aggiorna già presente per particolarità
034400140401     c                   if        %found(tabel00f)
034500140401     c                   movel     tbluni        ds7u
034600140401     c                   move      'S'           §7usre
034700140401     C                   MOVEL     DS7U          tbluni
034800140401     C                   MOVEL     '1'           tblftt
034900140401     C                   CLEAR                   tblftr
035000140401     C                   CLEAR                   tbldtr
035100140401     C                   UPDATE    TABEL
035200140401     c                   else
035300140401      *scrive
035400140401     c                   clear                   ds7u
035500140401     c                   move      'S'           §7usre
035600140401     C                   MOVEL     DS7U          tbluni
035700140401     C                   MOVEL     '1'           tblftt
035800140401     C                   CLEAR                   tblftr
035900140401     C                   CLEAR                   tbldtr
036000140401     C                   Z-ADD     1             tblkut
036100140401     C                   MOVEL     '7U'          tblcod
036200140402     c                   movel     cli(y)        tblkey
036300140401     c                   move      'Q'           tblkey
036400140401     C                   WRITE     TABEL
036500140401     c                   endif
036600140401     c                   endsr
036700140402     c*--------------------------------------------------------------
036800140402     c     toglie        begsr
036900140402     c*--------------------------------------------------------------
037000140408     c                   seton                                        52
037100140424     c                   do        *hival
037200140408     c                   exfmt     tb7ud2
037300140408     c   kl              leavesr
037400140408     c                   if        *inkf
037500140408     c     1             do        10            y
037600140408     c                   movel     cli(y)        tblkey
037700140408     c                   move      'Q'           tblkey
037800140408      * se codice = 0 passo al successivo
037900140408     c                   if        cli(y) = 0
038000140408     c                   iter
038100140408     c                   endif
038200140402     c     ktabel        chain     tabel00f
038300140402      *aggiorna già presente per particolarità
038400140402     c                   if        %found(tabel00f)
038500140402     c                   movel     tbluni        ds7u
038600140402     c                   move      *blank        §7usre
038700140402     C                   MOVEL     DS7U          tbluni
038800140402     C                   MOVEL     '1'           tblftt
038900140402     C                   CLEAR                   tblftr
039000140402     C                   CLEAR                   tbldtr
039100140402     C                   UPDATE    TABEL
039200140428     c                   move      'X'           ricarica          1
039300140402     c                   endif
039400140408     c                   enddo
039500140424     c                   leave
039600140408     c                   endif
039700140424     c                   enddo
039800140402     c                   endsr
039900020531      *-------------------------------------------------------------------------
040000020531      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/P.O
040100020531      *-------------------------------------------------------------------------
040200020531     C     F11GES        BEGSR
040300140409     C                   setoff                                         72
040400140507     c*                  select
040500140507     c*                  when      *in50
040600140507     C*                  CLEAR                   C1Crag
040700140507     c*                  when      *in51
040800140507     C*                  CLEAR                   C1Ccli
040900140507     c*                  endsl
041000020709      * IMPOSTO ENTRATA AD 'A'
041100020613     C                   EVAL      rcdnbr  = 1
041200020531      *
041300020531     C                   CLEAR                   QLGSCB
041400020531     C                   CLEAR                   QLGSCB00
041500020531
041600020531     ** 1 campo chiave.
041700020531     C                   EVAL      QLGNBRK    = 1
041800020531
041900050110     c                   select
042000140409     C                   when      *in51
042100050111     ** IL cliente è a posizione 2, numerico, 3 byte, ascending.
042200050110     C                   EVAL      QLGSP      = 2
042300050110     C                   EVAL      QLGSS      = %SIZE(cliv)
042400050110     C                   EVAL      QLGDT      = CARATTERE
042500020531     C                   EVAL      QLGSO      = Ascendente
042600020531     C                   EVAL      QLGKL(1)   = QLGSKL
042700140506     c                   clear                   ordw              1
042800140409     C                   setoff                                       51
042900140409     C                   seton                                        50
043000050110     ** La ragione sociale è a posizione 10, 25 byte, char, ascending.
043100140409     c                   when      *in50
043200050110     C                   EVAL      QLGSP      = 10
043300050110     C                   EVAL      QLGSS      = %SIZE(ragv)
043400020603     C                   EVAL      QLGDT      = Carattere
043500020603     C                   EVAL      QLGSO      = Ascendente
043600020603     C                   EVAL      QLGKL(1)   = QLGSKL
043700140506     c                   move      'A'           ordw
043800140409     C                   setoff                                       50
043900140409     C                   seton                                        51
044000050110     C                   ENDsl
044100020531
044200020531      * Load other sort parameters.
044300020531     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
044400020531     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
044500020531     C                   EVAL      QLGRT     = 8
044600020531     C                   EVAL      QLGOKL    = 80
044700020531     C                   EVAL      QLGLKE    = 16
044800020531     C                   EVAL      QLGLSS    = 290
044900020531
045000020531      * Initialize Sort I/O API fields.
045100020531     C                   EVAL      QLGRL00  = QLGRL
045200020531     C                   EVAL      QLGRC00  = 1
045300020531     C                   CLEAR                   QUSEI
045400020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
045500020531
045600020531      * First step - Initialize the sort routine.
045700020531     C                   CALL      'QLGSORT'
045800020531     C                   PARM                    QLGSCB
045900020531     C                   PARM                    NotUsed
046000020531     C                   PARM                    NotUsed
046100020531     C                   PARM                    SizeList
046200020531     C                   PARM                    ReturnSize
046300020531     C                   PARM                    QUSEC
046400020531
046500020531      * Next step - Write records to I/O routine.
046600020531     C                   EVAL      QLGRT00 = Put
046700020531
046800050111     C                   DO        STMREC1       x
046900020531
047000140401     C     x             CHAIN     tb7uDSF
047100020531
047200020531     ** Solo le righe con Selected = 'Y' sono riordinate,
047300020531     ** quindi per fare un ordinamento di tutte le righe
047400020531     ** metto 'Y' sempre.
047500020531     C                   EVAL      Selected = 'Y'
047600020531
047700020531     C                   CLEAR                   QUSEI
047800020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
047900020531     C                   CALL      'QLGSRTIO'
048000020531     C                   PARM                    QLGSCB00
048100020531     C                   PARM                    SflRcd
048200020531     C                   PARM                    NotUsed
048300020531     C                   PARM                    SizeList
048400020531     C                   PARM                    NotUsed
048500020531     C                   PARM                    QUSEC
048600020531
048700020531     C                   ENDDO
048800020531
048900020531      * Next step - Signal end of input, clear subfile for reload.
049000020531     C                   EVAL      QLGRT00 = EndPut
049100020531     C                   CLEAR                   QUSEI
049200020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
049300020531
049400020531     C                   CALL      'QLGSRTIO'
049500020531     C                   PARM                    QLGSCB00
049600020531     C                   PARM                    SflRcd
049700020531     C                   PARM                    NotUsed
049800020531     C                   PARM                    SizeList
049900020531     C                   PARM                    NotUsed
050000020531     C                   PARM                    QUSEC
050100020531
050200020531     C                   SETON                                            70
050300140401     C                   WRITE     tb7uDCT
050400020603     C                   SETOFF                                       70
050500140401     C                   WRITE     tb7uD1
050600140401     C                   WRITE     tb7uZ1
050700020531
050800020531      * Final step - Write the records back to the subfile.
050900020531     C                   EVAL      QLGRT00 = Get
051000020531
051100020613     C                   DO        STMREC1       x
051200020531     C                   CLEAR                   QUSEI
051300020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
051400020531     C                   CALL      'QLGSRTIO'
051500020531     C                   PARM                    QLGSCB00
051600020531     C                   PARM                    NotUsed
051700020531     C                   PARM                    SflRcd
051800020531     C                   PARM                    QLGRL00
051900020531     C                   PARM                    NotUsed
052000020531     C                   PARM                    QUSEC
052100140402     c                   move      hin89         *in89
052200020613     c                   z-add     x             rec1
052300140401     C                   WRITE     tb7uDSF
052400020531     C                   ENDDO
052500020531
052600020531     C                   EVAL      *IN70 = *off
052700020604      *
052800020531     C                   ENDSR
052900020606      *-------------------------------------------------------------------------
053000020613      * CTR Subroutine per il riposizionamento
053100020606      *-------------------------------------------------------------------------
053200020606     C     CTR           BEGSR
053300020606     C*
053400140408     c                   clear                   vuoto             7 0
053500140408     c                   clear                   selez             1
053600020612     C*
053700140408   2aC     1             DO        *hival        X
053800140408     C     X             CHAIN     tb7uDSF                            91
053900140408     C   91              LEAVE
054000140408   1aC                   IF        scelta = '1' and z < 10
054100140408     c                   z-add     1             z                 2 0
054200140408     c     vuoto         lookup    cli(z)                                 53
054300140408     C                   MOVEL     cliv          cli(z)
054400140408     C                   MOVEl     ragv          clid(z)
054500140408     c                   move      'X'           selez
054600140408     c                   endif
054700140408     c                   enddo
054800140408     c                   if        selez <> *blank
054900140408     C                   EXSR      toglie
055000140408     c                   setoff                                       52
055100140408     c                   clear                   cli
055200140408     c                   clear                   clid
055300140402     c                   leavesr
055400140408     c                   endif
055500020710      * riemetto se non ho trovato records
055600140408     C   72              DO
055700140408     C                   WRITE     tb7uZ1
055800140408     C                   LEAVE
055900140408     C                   ENDDO
056000020613      *
056100020606     C                   ENDSR
056200140402     c*--------------------------------------------------------
056300140402     c     stampa        begsr
056400140402     c*--------------------------------------------------------
056500140402      * rilegge per stampare elenco
056600140402     C     ktab          setll     tabel00f
056700140402     c                   do        *hival
056800140402     C     ktab          reade     tabel00f
056900140402     c                   if        %eof(tabel00f)
057000140402     c                   leave
057100140402     c                   end
057200140402     c                   if        tblflg <> *blanks
057300140402     c                   iter
057400140402     c                   end
057500140402     c                   movel     tblkey        filclia           3
057600140403     c                   move      filclia       filcli
057700140402     c     filcli        lookup    lin                                    88
057800140402     c  n88              iter
057900140402     c                   movel     tbluni        ds7u
058000140402     c                   if        §7usre <> 'S'
058100140402     c                   iter
058200140402     c                   endif
058300140402     c                   exsr      riesfl
058400140408     C                   if        lin1 > 60
058500140402     C                   except    testa
058600140402     c                   endif
058700140402     c*
058800140402     C                   except    dettaglio
058900140402     c                   enddo
059000140402     c                   endsr
059100140401     C*----------------------------------------------------*
059200140401     C     fine          BEGSR
059300140401     C*----------------------------------------------------*
059400140401      * Non ho scelto nulla
059500140401     C                   SETON                                            LR
059600140401     c                   return
059700140401     c                   endsr
059800140402      *---------------------------------------------------------------------
059900140402     C     comando       BEGSR
060000140402      *---------------------------------------------------------------------
060100140402     c                   if        %subst(knsif:7:1) = 'P'
060200140402     C                   movea     cmd1          cmd
060300140402     c                   else
060400140402     C                   movea     cmd2          cmd
060500140402     c                   endif
060600140402     C                   movel(p)  cmd           comman
060700140402     C                   call      'QCMDEXC'
060800140402     C                   parm                    comman           80
060900140402     C                   parm      49            lung             15 5
061000140402     C*
061100140402     C                   ENDSR
061200140401     C*------------------------------------------------------------------------*
061300140401     C     *inzsr        begsr
061400140401     C*------------------------------------------------------------------------*
061500140401     C     *ENTRY        PLIST
061600140401     C                   PARM                    Kpjba
061700140401     c     ktab          klist
061800140401     c                   kfld                    tblkut
061900140401     c                   kfld                    tblcod
062000140401     c     ktabel        klist
062100140401     c                   kfld                    tblkut
062200140401     c                   kfld                    tblcod
062300140401     c                   kfld                    tblkey
062400140401     c                   exsr      sr_Datijob
062500140403      *
062600140403     c                   movel     utefaf        dute01
062700140403     c                   if        §uteabres = 'NO'
062800140403     c                   exfmt     tb7unoab
062900140403     c                   exsr      fine
063000140403     c                   endif
063100140403      * Reperimento dei P.O. gestibili dall'utente
063200140403     c                   clear                   TRUL31DS
063300140408      *se autorizzazione specifica non indicata utilizzo autorizzazione generica
063400140408     c                   if        §uteabres = *blank
063500140408     c                   eval      I31abi = uteaut
063600140408     c                   else
063700140403     c                   eval      I31abi = §uteabres
063800140408     c                   endif
063900140403     c                   eval      I31cdi = DUTdis
064000140403     c                   eval      I31car = DUTare
064100140403     c                   eval      I31cpo = DUTpou
064200140403     c                   call      'TRUL31R'
064300140403     c                   parm                    KPJBA
064400140403     c                   parm                    TRUL31DS
064500140403     c                   if        o31pog > *zeros
064600140403     c                   movea     o31pog        Lin
064700140403     c                   endif
064800140401     C*
064900140429     c                   if        dutpou <> 046
065000140402     c                   exsr      comando
065100140429     c                   endif
065200140402     c                   open      tabel00f
065300140402     C*
065400140401     c                   endsr
065500140401      *---------------------------------------------------------------*
065600140401      * Reperimento dati del job (Utente/Operativi)                   *
065700140401      *---------------------------------------------------------------*
065800140401     c     sr_DatiJob    BEGSR
065900140401      *
066000140401     c     *dtaara       define    §azute        AZUTEds
066100140401     c     *dtaara       define    §datiute      dDatiUte
066200140401      *
066300140401     c                   in(E)     *dtaara
066400140401     c                   IF        %Error or RSUT = *blanks
066500140401     c                   clear                   TIBS34ds
066600140401     c                   call      'TIBS34R'
066700140401     c                   parm                    TIBS34ds
066800140401     c                   in        *dtaara
066900140401     c                   ENDIF
067000140401      *
067100140401     c                   ENDSR
067200140402     O*------------------------------------------------------*
067300140402     Osysprt    e            TESTA             2
067400140402     o                       knsif               10
067500140402     o                       knmus               22
067600140408     o                                          105 'TRTB7UR'
067700140408     o                                          112 'Pag.'
067800140408     o                       page          z    118
067900140402     o                       *date         y    130
068000140408     O                                           95 'Stampa Elenco clienti ab-
068100140402     O                                              ilitati ai resi in -
068200140402     o                                              periodo di trazioni ridotte'
068300140402     O          e            dettaglio   2
068400140402     O                       cliv                10
068500140402     O                       ragv                60
068600140402      *-------------------------------------------------------------------------
068700140402**         CMD1
068800140402OVRDBF FILE(tabel00f) TOFILE(gaitragrps/tabel00f)
068900140402**         CMD2
069000140402OVRDBF FILE(tabel00f) TOFILE(gaitragru/tabel00f)
