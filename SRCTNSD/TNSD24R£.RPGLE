000100971203     H*------------------------------------------------------------------------*
000200971203     H* RICERCA CODICI ORGANIGRAMMA
000300971203     H* - AGGIUNTA LA PARZIALIZZAZIONE SU ORGDIT ('SDI/BAR/'   '=TUTTE)
000400971203     H*------------------------------------------------------------------------*
000500971203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000600971203     F*------------------------------------------------------------------------*
000700971203     F* DATA BASE
000800971203     F*------------------------------------------------------------------------*
000900900612     FAZORG03L  IF   E           K DISK
001000910225     FAZOSE03L  IF   E           K DISK
001100910225     F                                     RENAME(AZORG:AZOSE)
001200971203     FTNSD24D   CF   E             WORKSTN
001300971203     F                                     SFILE(SD24DSF:REC1)
001400020531     d*--------------------------------------------------------------------------------------------*
001500020531     d* Data structure                                                                             *
001600020531     d*--------------------------------------------------------------------------------------------*
001700160923      ****-?Caricamento filiali abilitate
001800160923     d kpjba         e ds
001900160923     d Tibs36ds      e ds                  inz
002000160923     d TRUL31ds      e ds                  inz
002100160923     d $POg            s              3    inz   dim(250)
002200160923      *
002300020603     d OG143         E DS
002400020603      *
002500020531     d NotUsed         S             16A
002600020531     d ReturnSize      S              9B 0
002700020531     d SizeList        S              9B 0
002800020531     d WrkSort         S              1
002900020531     d*---
003000020531     d Rec1            S              3  0                                      *n° rel sf
003100020531     d stmrec1         S              3  0                                      *n° rel sf x stampa
003200020531     d*---
003300020531     d MaxKey          C                   2
003400020531     d Ascendente      C                   1
003500020531     d Discendente     C                   2
003600020531     d Carattere       C                   6
003700020531     d Put             C                   1
003800020531     d EndPut          C                   2
003900020531     d Get             C                   3
004000020603     d Numerico        C                   8
004100020531     d*---
004200020531     d SflRcd          DS
004300020531     d  Scelta
004400020603     d  OrgTip
004500020531     d  OrgFil
004600020531     d  OrgDes
004700020725     d  OrgNtw
004800020531     d  OrgFva
004900020531     d  Selected                      1A
005000020531
005100020531      /COPY QSYSINC/QRPGLESRC,QLGSORT
005200020531     d QLGKL                         16    DIM(MaxKey)
005300020531     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
005400020531     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
005500020531     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
005600020531     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
005700020531
005800020531      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
005900020531      /COPY QSYSINC/QRPGLESRC,QUSEC
006000971203     C*------------------------------------------------------------------------*
006100971203     C* MAIN LINE
006200971203     C*------------------------------------------------------------------------*
006300971203     C     *ENTRY        PLIST
006400971203     C                   PARM                    §COD
006500971203     C                   PARM                    §TIP
006600971203     C                   PARM                    §DES             25
006700971203     C                   PARM                    §PRA              3
006800160923     C                   PARM                    §abi              2
006900160923     C                   PARM                    kpjba
007000160923      * verifica per abilitazione
007100160923     c                   if        %parms>4
007200160923      *  In base all'utente prende il P.O.
007300160923     C                   CLEAR                   Tibs36Ds
007400160923     c                   EVAL      I36ute = Knmus
007500160923     c                   EVAL      I36Tla = 'L'
007600160923     C                   CALL      'TIBS36R'
007700160923     C                   PARM                    Tibs36Ds
007800160923      * Reperimento dei P.O.  gestibili dal profilo
007900160923     c                   clear                   TRUL31DS
008000160923     c                   eval      I31abi = §abi
008100160923     c                   eval      I31cpo = o36pou
008200160923     c                   call      'TRUL31R'
008300160923     c                   parm                    KPJBA
008400160923     c                   parm                    TRUL31DS
008500160923     c                   if        o31pog > *zeros
008600160923     c                   movea     o31pog        $pog
008700160923     c                   endif
008800160923     c                   endif
008900160923      *
009000910225     C     §TIP          COMP      '§'                                    88
009100900418     C     *LIKE         DEFINE    ORGFAG        §TIP
009200020604     C     *LIKE         DEFINE    C1CPOU        W1CPOU
009300020604     C     *LIKE         DEFINE    C1CDES        W1CDES
009400900418     C                   MOVE      *BLANKS       SCELTA                         *
009500900418     C                   Z-ADD     0             REC1              3 0
009600900418     C*---------- PULIZIA SLFCT :                          *
009700900418     C                   SETON                                            70    *
009800971203     C                   WRITE     SD24DCT                                      *
009900900418     C                   SETOFF                                       7071      *
010000900418     C*----------
010100021128      * se richiesto un tipo non imposto la descrizione ad 'A'
010200040518     C                   IF        §tip =  *blanks and §DES=*blanks
010300020709     C                   movel     'A'           C1cdes
010400040518     c                   else
010500040518     C                   IF        §tip =  *blanks
010600040518     c                   movel(P)  §des          c1cdes
010700021128     C                   ENDIF
010800040518     C                   ENDIF
010900040518     c
011000020710     C                   move      *on           $prima_emis       1
011100020710     C                   setoff                                       15
011200020531     C                   Z-ADD     *zeros        Rec1                           *n° record sfile
011300971203     C                   WRITE     SD24D1                                       *
011400020709      *
011500900418     C     SOPRA         TAG                                                    *
011600020604      *
011700020531      * Organigramma aziendale di sede
011800020709     C   88              READ      AZOSE                                  91
011900020604      *
012000020531      * Organigramma aziendale di azienda/filiale
012100020709     C  N88              READ      AZORG                                  91
012200020617      *
012300020617     C   91              GOTO      EMISF1                                       *
012400020606      *
012500020531      * a seconda del tipo passato in input seleziono i record
012600020531     C     §TIP          IFNE      *BLANKS
012700020531     C                   EXSR      SELTIP
012800020531     C     $NoSel        IFEQ      *ON
012900020531     C                   GOTO      SOPRA                                        *
013000020531     C                   ENDIF
013100020604     C                   ENDIF
013200020531      *
013300971203     C     §PRA          IFNE      *BLANKS
013400971203     C     §PRA          ANDNE     ORGDIT
013500971203     C                   GOTO      SOPRA                                        *
013600971203     C                   ENDIF
013700971203     C*
013800020613     C*---
013900900418     C     REC1          ADD       1             REC1                 71        *
014000020603     C                   EXSR      RIESFL
014100020613     C                   WRITE     SD24Z1                                       *
014200971203     C                   WRITE     SD24DSF                                      *
014300020603     C                   Z-ADD     REC1          STMREC1                        *n° rel sf x stampa
014400020710      *
014500020710      * alla pima emissione mi posiziono sempre a partire da 'A'
014600021128      * a meno che non abbia richiesto il tipo p.o
014700120206      *          o non abbia ricevuto un altro posizionamento
014800021128     C                   if        $prima_emis = *off or §tip <> *blanks
014900020710     C                   EVAL      RcdNbr  = 1
015000020710     C                   else
015100120206   3aC*//                IF        *in15 = *off and 'A' <= OrgDes
015200120206   3aC                   IF        *in15 = *off and C1Cdes <= OrgDes
015300020710     C                   EVAL      RcdNbr  = Rec1
015400020710     C                   seton                                        15
015500020710     C                   endif
015600020710     C                   endif
015700000000     C                   GOTO      SOPRA                                        *
015800000000     C*---------- EMISSIONE SFL :                          *
015900020710     C                   move      *off          $prima_emis
016000000000     C     EMISF1        TAG                                                    *
016100020709     C                   EXFMT     SD24DCT                                      *
016200020709     C   KL              SETON                                            90    *
016300020709     C   KL              GOTO      FINE                                         *
016400020604      *
016500020603     C* F11 = Cambia ordinamento
016600020603if  1C                   IF        *INKK
016700020603     C                   EXSR      F11GES
016800020603e   1C                   ENDIF
016900020604      *
017000020606     C                   EXSR      CTR
017100000000     C*----------------------------------------------------*
017200000000     C     FINE          TAG                                                    *
017300020603      *
017400020603      * Non ho scelto nulla
017500900418     C   90              MOVE      *BLANKS       §COD              3            *
017600900515     C   90              MOVE      *BLANKS       §DES                           *
017700020603     C   90              MOVE      *BLANKS       §TIP                           *
017800000000     C                   SETON                                            LR    *
017900020531      *--------------------------------------------------------------------------------------------*
018000020531      * Selezione Tipo P.O
018100020531      *--------------------------------------------------------------------------------------------*
018200020531     C     SELTIP        BEGSR
018300020531     c                   move      ' '           $NoSel            1
018400020531     C                   SELECT
018500160923     c                   when      %parms > 4
018600160923     c                   move      orgfil        orgfila           3
018700160923     c     orgfila       lookup    $pog                                   66
018800160923     c  n66              move      '1'           $nosel
018900020531      * Filiale
019000020531     C     §TIP          WHENEQ    'F'
019100020531     C                   if        OrgFag <>'F'
019200020531     C                   move      '1'           $NoSel
019300020531     C                   endif
019400020531      * Agenzia
019500020531     C     §TIP          WHENEQ    'A'
019600020531     C                   if        OrgFag <>'A'
019700020531     C                   move      '1'           $NoSel
019800020531     C                   endif
019900020531      * Agenzia/Filiale
020000020531     C     §TIP          WHENEQ    'E'
020100020603     C                   if        OrgFag <>'F' and OrgFag <>'A'
020200020531     C                   move      '1'           $NoSel
020300020531     C                   endif
020400020531      * Varie
020500020531     C     §TIP          WHENEQ    'V'
020600020531     C                   if        OrgFag <>'V' or OrgFl2 <>' '
020700020531     C                   move      '1'           $NoSel
020800020531     C                   endif
020900020531      * Linea per tariffe
021000020531     C     §TIP          WHENEQ    'T'
021100020531     C                   if        OrgFag <>'V' or OrgFl2 <>'S'
021200020531     C                   move      '1'           $NoSel
021300020531     C                   endif
021400020531      * Varie piu' linea per tariffe
021500020531     C     §TIP          WHENEQ    'W'
021600020531     C                   if        OrgFag <>'V' or OrgFl2 <>' '
021700020531     C                             or OrgFl2<> 'S'
021800020531     C                   move      '1'           $NoSel
021900020531     C                   endif
022000020531     C                   ENDSL
022100020531     C                   ENDSR
022200020603      *--------------------------------------------------------------------------------------------*
022300020603      * Valorizzo campo tipo flag tipo p.o
022400020603      *--------------------------------------------------------------------------------------------*
022500020603     C     RIESFL        BEGSR
022600020603      *
022700020603      * TIPO P.O.
022800020603     C                   SELECT
022900020603      * Filiale
023000020603     C                   WHEN      OrgFag = 'F'
023100020603     C                   move      'F'           OrgTip
023200020603      * Agenzia
023300020603     C                   WHEN      OrgFag = 'A'
023400020603     C                   move      'A'           OrgTip
023500020603      * Varie
023600020603     C                   WHEN      OrgFag = 'V' and OrgFl2 =' '
023700020603     C                   move      'V'           OrgTip
023800020603      * Linea per tariffa
023900020603     C                   WHEN      OrgFag = 'V' and OrgFl2 ='S'
024000020603     C                   move      'T'           OrgTip
024100020603     C                   ENDSL
024200020603      *
024300020603      * NETWORK
024400020603     C                   MOVEL     OrgDe3        OG143
024500020603     C                   MOVEL     §ogntw        ORGNTW
024600020603      *
024700020603     C                   ENDSR
024800020531      *--------------------------------------------------------------------------------------------*
024900020531      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/P.O
025000020531      *--------------------------------------------------------------------------------------------*
025100020531     C     F11GES        BEGSR
025200020710     C                   setoff                                       72
025300020613     C                   CLEAR                   C1CPou
025400020709      * IMPOSTO ENTRATA AD 'A'
025500020613     C                   EVAL      rcdnbr  = 1
025600020531      *
025700020531     C                   CLEAR                   QLGSCB
025800020531     C                   CLEAR                   QLGSCB00
025900020531
026000020531     ** 1 campo chiave.
026100020531     C                   EVAL      QLGNBRK    = 1
026200020531
026300020603     C                   IF        *in50=*off
026400020603     ** IL P.O è a posizione 3, numerico, 3 byte, ascending.
026500020603     C                   EVAL      QLGSP      = 3
026600020603     C                   EVAL      QLGSS      = %SIZE(ORGFIL)
026700020603     C                   EVAL      QLGDT      = Numerico
026800020531     C                   EVAL      QLGSO      = Ascendente
026900020531     C                   EVAL      QLGKL(1)   = QLGSKL
027000020603     C                   seton                                        50
027100020603     C                   ELSE
027200020603      *
027300020603     ** La ragione sociale è a posizione 6, 20 byte, char, ascending.
027400020603     C                   EVAL      QLGSP      = 6
027500020603     C                   EVAL      QLGSS      = %SIZE(ORGDES)
027600020603     C                   EVAL      QLGDT      = Carattere
027700020603     C                   EVAL      QLGSO      = Ascendente
027800020603     C                   EVAL      QLGKL(1)   = QLGSKL
027900020603     C                   setoff                                       50
028000020603     C                   ENDIF
028100020531
028200020531      * Load other sort parameters.
028300020531     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
028400020531     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
028500020531     C                   EVAL      QLGRT     = 8
028600020531     C                   EVAL      QLGOKL    = 80
028700020531     C                   EVAL      QLGLKE    = 16
028800020531     C                   EVAL      QLGLSS    = 290
028900020531
029000020531      * Initialize Sort I/O API fields.
029100020531     C                   EVAL      QLGRL00  = QLGRL
029200020531     C                   EVAL      QLGRC00  = 1
029300020531     C                   CLEAR                   QUSEI
029400020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
029500020531
029600020531      * First step - Initialize the sort routine.
029700020531     C                   CALL      'QLGSORT'
029800020531     C                   PARM                    QLGSCB
029900020531     C                   PARM                    NotUsed
030000020531     C                   PARM                    NotUsed
030100020531     C                   PARM                    SizeList
030200020531     C                   PARM                    ReturnSize
030300020531     C                   PARM                    QUSEC
030400020531
030500020531      * Next step - Write records to I/O routine.
030600020531     C                   EVAL      QLGRT00 = Put
030700020531
030800020613     C                   DO        STMREC1       x                 3 0
030900020531
031000020613     C     x             CHAIN     SD24DSF
031100020531
031200020531     ** Solo le righe con Selected = 'Y' sono riordinate,
031300020531     ** quindi per fare un ordinamento di tutte le righe
031400020531     ** metto 'Y' sempre.
031500020531     C                   EVAL      Selected = 'Y'
031600020531
031700020531     C                   CLEAR                   QUSEI
031800020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
031900020531     C                   CALL      'QLGSRTIO'
032000020531     C                   PARM                    QLGSCB00
032100020531     C                   PARM                    SflRcd
032200020531     C                   PARM                    NotUsed
032300020531     C                   PARM                    SizeList
032400020531     C                   PARM                    NotUsed
032500020531     C                   PARM                    QUSEC
032600020531
032700020531     C                   ENDDO
032800020531
032900020531      * Next step - Signal end of input, clear subfile for reload.
033000020531     C                   EVAL      QLGRT00 = EndPut
033100020531     C                   CLEAR                   QUSEI
033200020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
033300020531
033400020531     C                   CALL      'QLGSRTIO'
033500020531     C                   PARM                    QLGSCB00
033600020531     C                   PARM                    SflRcd
033700020531     C                   PARM                    NotUsed
033800020531     C                   PARM                    SizeList
033900020531     C                   PARM                    NotUsed
034000020531     C                   PARM                    QUSEC
034100020531
034200020531     C                   SETON                                            70    *
034300020531     C                   WRITE     SD24DCT                                      *
034400020603     C                   SETOFF                                       70        *
034500020612     C                   WRITE     SD24D1                                       *
034600020613     C                   WRITE     SD24Z1                                       *
034700020531
034800020531      * Final step - Write the records back to the subfile.
034900020531     C                   EVAL      QLGRT00 = Get
035000020531
035100020613     C                   DO        STMREC1       x
035200020531     C                   CLEAR                   QUSEI
035300020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
035400020531     C                   CALL      'QLGSRTIO'
035500020531     C                   PARM                    QLGSCB00
035600020531     C                   PARM                    NotUsed
035700020531     C                   PARM                    SflRcd
035800020531     C                   PARM                    QLGRL00
035900020531     C                   PARM                    NotUsed
036000020531     C                   PARM                    QUSEC
036100020531      *
036200020613     c                   z-add     x             rec1
036300020531     C                   WRITE     SD24DSF
036400020531     C                   ENDDO
036500020531
036600020531     C                   EVAL      *IN70 = *off
036700020604      *
036800020531     C                   ENDSR
036900020606      *--------------------------------------------------------------------------------------------*
037000020613      * CTR Subroutine per il riposizionamento
037100020606      *--------------------------------------------------------------------------------------------*
037200020606     C     CTR           BEGSR
037300020606     C*
037400020612     C  N71              GOTO      EMISF1
037500020612     C*
037600020613     C   71              READC     SD24DSF                                91    *
037700020613   1aC     Scelta        IFEQ      '1'                                          *
037800020613     C                   MOVEL     ORGFIL        §COD                           *
037900020613     C                   MOVEL     ORGTIP        §TIP                           *
038000020613     C                   MOVEL     ORGDES        §DES                           *
038100020613   1eC                   ELSE
038200020613      *
038300020613      * Controllo la scelta
038400020710     C                   seton                                        72
038500020613   2aC                   DO        *hival        X
038600020613     C     X             CHAIN     SD24DSF                            91        *
038700020613     C   91              LEAVE
038800020612      *
038900020613   3aC                   IF        (*in50 = *off and C1CDes <= OrgDes) or
039000020613     C                             (*in50 = *on  and C1CPou <= OrgFil)
039100020613     C                   z-add     X             RcdNbr
039200020710     C                   setoff                                       72
039300020613   3eC                   ELSE
039400020613     C                   ITER
039500020613     C                   ENDIF
039600020710     C                   WRITE     SD24Z1                                       *
039700020613     C                   GOTO      EMISF1
039800020613   2-C                   ENDDO
039900020710      * riemetto se non ho trovato records
040000020710     C   72              DO
040100020710     C                   WRITE     SD24Z1                                       *
040200020710     C                   GOTO      EMISF1
040300020710     C                   ENDDO
040400020710   1-C                   ENDIF
040500020606      *
040600020613     C                   move      *blanks       C1CDes
040700020613     C                   move      *blanks       C1CPou
040800020613      *
040900020606     C                   ENDSR
