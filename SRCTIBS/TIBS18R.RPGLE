000100020114    ? * TIBS18R *-----------------------------------------------------*
000200020114?     *       PROGRAMMA CREAZIONE FILE NUMERATORI DI GRUPPO           *
000300011206?     *---------------------------------------------------------------*
000400011206     H DECEDIT('0,') DATEDIT(*DMY.)
000500011206?     *---------------------------------------------------------------*
000600011206     FAZNUM01L  IF A E           K DISK
000700011206     FAZORG01L  IF   E           K DISK
000800011206     FTNTBE01L  IF   E           K DISK
000900011206?     *---------------------------------------------------------------*
001000011206     D FIL             S              3  0 DIM(500)
001100011206     D L1              S              3  0 DIM(30)
001200011206?    D*
001300011206     D PARAM           DS
001400011206     D*   Codice P.O.
001500011206     D  PARFIL                 3      5  0
001600011206     D*   Codice P.O. elaboratore di PARFIL
001700011206     D  PARFEL                 6      8  0
001800011206     D*   Codice numeratore
001900011206     D  PARCNU                 9     11
002000011206     D*   Tipo filiale Part/Arr
002100011206     D  PARFTF                12     12
002200011206     D*   Anno
002300011206     D  PARAAA                13     16  0
002400020206     D*   Società
002500020206     D  PARSOC                17     19
002600011206     D*
002700011206     D WLBDAT          DS                  INZ
002800011206     D  G02DAT                 1      8  0
002900011206     D  G02INV                 9     16  0
003000011206     D  G02ERR                17     17
003100011206     D  G02TGI                18     22  0
003200011206?    D*
003300011206     D DCNU          E DS                  INZ
003400011206     D TRUL06DS      E DS                  INZ
003500011206     D  LIN                    1     90  0 DIM(30)
003600011206?    D*
003700011206     D KPJBA         E DS
003800011206     D CNCR80        E DS
003900011206     D UT§DSE0F      E DS
004000011206?    D*
004100011206     D  KAAA           S                   like(NUMaaa)
004200011206     D  savCNU         S                   like(NUMcnu)
004300011206     D  savPO1         S                   like(NUMpo1)
004400011206     D  savPO2         S                   like(NUMpo2)
004500011206     D  DATEU          S              8  0
004600011206     D  CORaas         S              4  0
004700011206     D  z              S              3  0
004800011206     D  x              S              3  0
004900011206?     *---------------------------------------------------------------*
005000011206     C                   movel     *off          *in35
005100011206     C                   movel     'CNU'         TBEcod
005200011206IF  1C                   if        PARcnu <> *blanks
005300011206     C                   movel(p)  PARcnu        TBEke1
005400011206     C                   clear                   TBEke2
005500011206     C     K3TBE1        chain     TNTBE000                           35
005600011206X   1C                   else
005700011206     C     TBEcod        setll     TNTBE000
005800011206     C     TBEcod        reade     TNTBE000                               35
005900011206E   1C                   endif
006000011206      *
006100011206DO  1C     *IN35         DOWEQ     *OFF
006200011206      *
006300011206      * ESCLUDO ANNULLATI
006400011206IF  2C                   if        TBEatb = *blanks
006500011206     C                   movel     TBEuni        dCNU
006600011206     C                   movel     TBEke1        savCNU
006700011206      *
006800011206      * ESCLUDO QUELLI DA NON RICREARE
006900011206IF  3C     §cnuNCR       IFNE      'S'
007000011206      *
007100011206      * ESCLUDO QUELLI CON ANNO SE NON RICEVUTO
007200011206IF  4C                   if         §cnuFAA = *blanks  OR
007300011206     C                             (§cnuFAA = 'S'  and  PARAAA <> 0)
007400011206      * Se numeratore con anno imposto l'anno corrente in KEY
007500011206      * se senza anno imposto *zero
007600011206IF  5C                   if        §cnuFAA = 'S'
007700011206     C                   z-add     *YEAR         CORaas
007800011206X   5C                   else
007900011206     C                   clear                   CORaas
008000011206E   5C                   endif
008100011206      *
008200011206SEL 5C                   SELECT
008300011206      * ARRIVO
008400011206W   5C                   WHEN      PARftf = 'A'
008500011206     C                   exsr      CRTNUM_A
008600011206      * PARTENZA Controllo se numeratore solo per 1° o 2° livello
008700011206W   5C                   WHEN      (§cnuFNU = 'S' and PARfil =  PARfel) OR
008800011206     C                             (§cnuFNU = 'L' and PARfil <> PARfel) OR
008900011206     C                              §cnuFNU = *blanks
009000011206     C                   exsr      CRTNUM_P
009100011206E   5C                   ENDSL
009200011206      *
009300011206E   4C                   ENDIF
009400011206E   3C                   ENDIF
009500011206E   2C                   ENDIF
009600011206      *
009700011206      *  SE COD.NUMERATORE RICEVUTO VADO A FINE
009800011206IF  2C                   if         PARcnu <> *blanks
009900011206     C                   leave
010000011206X   2C                   else
010100011206     C     TBEcod        reade     TNTBE000                               35
010200011206E   2C                   endif
010300011206      *
010400011206E   1C                   ENDDO
010500011206      *
010600011206     C                   EVAL      *INLR = *ON
010700011206
010800011206?     *---------------------------------------------------------------*
010900011206?     *--- ROUTINE DI CREAZIONE AZNUM00F CON PARFIL COME LNP ---------*
011000011206?     *---------------------------------------------------------------*
011100011206     C     CRTNUM_P      BEGSR
011200011206      *
011300011206      ****
011400011206      ** §CNUPO1 = 'TUT'  ===>  TUTTI I P.O.
011500011206      ** §CNUPO1 = 'FIL'  ===>  P.O. DA CREARE
011600020226      **     ALTRIMENTI   ===>  QUELLO CHE C'E' (SE C'E')
011700011206      **
011800011206      ** §CNUPO2 = 'TUT'  ===>  TUTTI I P.O.
011900011206      ** §CNUPO2 = 'FIL'  ===>  P.O. DA CREARE
012000020226      **     ALTRIMENTI   ===>  QUELLO CHE C'E' (SE C'E')
012100011206      ****
012200011206      *
012300011206SEL 1C                   SELECT
012400011206      *
012500011206W   1C                   WHEN      §cnuPO1 = 'TUT'  and   §cnuPO2 = 'TUT'
012600011206     C                   eval      x = 1
012700011206DO  2C                   Dow       FIL(X) <> *zeros
012800011206     C                   z-add     FIL(X)        savPO1
012900011206     C                   exsr      TUTTI
013000011206     C                   add       1             X
013100011206E   2C                   EndDo
013200011206      *
013300011206W   1C                   WHEN      §cnuPO1 = 'TUT'
013400011206IF  2C                   If        §cnuPO2 = 'FIL'
013500011206     C                   z-add     PARFIL        savPO2
013600011206X   2C                   Else
013700020226IF  3C                   If        §cnuPO2 = *blanks
013800020226     C                   clear                   savPO2
013900020226X   3C                   else
014000020226     C                   move      §cnuPO2       savPO2
014100020226E   3C                   Endif
014200011206E   2C                   Endif
014300011206     C                   eval      x = 1
014400011206DO  2C                   Dow       FIL(X) <> *zeros
014500011206     C                   z-add     FIL(X)        savPO1
014600011206     C                   exsr      SCRIVI
014700011206     C                   add       1             X
014800011206E   2C                   EndDo
014900011206      *
015000011206W   1C                   WHEN      §cnuPO2 = 'TUT'
015100011206IF  2C                   If        §cnuPO1 = 'FIL'
015200011206     C                   z-add     PARFIL        savPO1
015300011206X   2C                   Else
015400020226IF  3C                   If        §cnuPO1 = *blanks
015500020226     C                   clear                   savPO1
015600020226X   3C                   else
015700020226     C                   move      §cnuPO1       savPO1
015800020226E   3C                   EndIf
015900011206E   2C                   EndIf
016000011206     C                   eval      z = 1
016100011206DO  2C                   Dow       FIL(Z) <> *zeros
016200011206     C                   z-add     FIL(Z)        savPO2
016300011206     C                   exsr      SCRIVI
016400011206     C                   add       1             Z
016500011206E   2C                   EndDo
016600011206      *
016700011206X   1C                   OTHER
016800011206IF  2C                   if        §cnuPO1 = 'FIL'
016900011206     C                   z-add     PARFIL        savPO1
017000011206X   2C                   else
017100020226IF  3C                   if        §cnuPO1 = *blanks
017200020226     C                   clear                   savPO1
017300020226X   3C                   else
017400020226     C                   move      §cnuPO1       savPO1
017500020226E   3C                   endif
017600011206E   2C                   endif
017700011206      *
017800011206IF  2C                   if        §cnuPO2 = 'FIL'
017900011206     C                   z-add     PARFIL        savPO2
018000011206X   2C                   else
018100020226IF  3C                   if        §cnuPO2 = *blanks
018200020226     C                   clear                   savPO2
018300020226X   3C                   else
018400020226     C                   move      §cnuPO2       savPO2
018500020226E   3C                   endif
018600011206E   2C                   endif
018700011206      *
018800011206     C                   exsr      SCRIVI
018900011206
019000011206E   1C                   ENDSL
019100011206      *
019200011206     C                   ENDSR
019300011206
019400011206?     *---------------------------------------------------------------*
019500011206?     *--- ROUTINE DI CREAZIONE AZNUM00F CON PARFIL COME LNA ---------*
019600011206?     *---------------------------------------------------------------*
019700011206     C     CRTNUM_A      BEGSR
019800020114      *
019900011206SEL 1C                   SELECT
020000020114      *
020100011206W   1C                   WHEN      §cnuPO1 = 'TUT'  and  §cnuPO2 = 'TUT'
020200011206      * Creo per il P.O. tutti i record dei P.O. esistenti
020300011206     C                   z-add     PARFIL        savPO1
020400011206     C                   exsr      TUTTI
020500011206      * Creo per tutti i record dei P.O. esistenti il P.O. immesso
020600011206     C                   z-add     PARFIL        savPO2
020700011206     C                   z-add     1             X
020800011206DO  2C                   Dow       FIL(X) <> *zeros
020900011206     C                   z-add     FIL(X)        savPO1
021000011206     C                   exsr      SCRIVI
021100011206     C                   add       1             X
021200011206E   2C                   EndDo
021300020114      *
021400011206W   1C                   WHEN      §cnuPO1 = 'TUT'  and  §cnuPO2 = 'FIL'
021500011206     C                   eval      x = 1
021600011206     C                   z-add     PARFIL        savPO1
021700011206DO  2C                   Dow       L1(X) <> *zeros
021800011206     C                   z-add     L1(X)         savPO2
021900011206      * solo se trovato il record per un'altro P.O. scrivo AZNUM
022000020206     C     K5NUM1A       setll     AZNUM000                               30
022100011206     C   30              exsr      SCRIVI
022200011206     C                   add       1             X
022300011206E   2C                   EndDo
022400020114      *
022500011206W   1C                   WHEN      §cnuPO1 = 'TUT'
022600011206     C                   z-add     PARFIL        savPO1
022700020226IF  2C                   If        §cnuPO2 = *blanks
022800020226     C                   clear                   savPO2
022900020226X   2C                   else
023000011206     C                   movel     §cnuPO2       savPO2
023100020226E   2C                   endif
023200011206     C                   exsr      SCRIVI
023300020114      *
023400011206W   1C                   WHEN      §cnuPO2 = 'TUT'  and  §cnuPO1 = 'FIL'
023500011206     C                   z-add     PARFIL        savPO2
023600011206     C                   clear                   savPO1
023700020206     C     K4NUM1        setgt     AZNUM000
023800020206     C     K3NUM1        reade     AZNUM000
023900011206DO  2C                   Dow       not %EOF
024000011206     C                   z-add     NUMpo1        savPO1
024100011206     C                   exsr      SCRIVI
024200020206     C     K4NUM1        setgt     AZNUM000
024300020206     C     K3NUM1        reade     AZNUM000
024400011206E   2C                   EndDo
024500020114      *
024600011206W   1C                   WHEN      §cnuPO2 = 'TUT'
024700011206     C                   z-add     PARFIL        savPO2
024800020226IF  2C                   If        §cnuPO1 = *blanks
024900020226     C                   clear                   savPO1
025000020226X   2C                   else
025100011206     C                   movel     §cnuPO1       savPO1
025200020226E   2C                   endif
025300011206     C                   exsr      SCRIVI
025400020114      *
025500011206E   1C                   ENDSL
025600011206      *
025700011206     C                   ENDSR
025800011206
025900011206?     *---------------------------------------------------------------*
026000011206?     *--- CREO PER TUTTE LE FILIALI UN RECORD IN AZNUM --------------*
026100011206?     *---------------------------------------------------------------*
026200011206     C     TUTTI         BEGSR
026300011206      *
026400011206     C                   eval      z = 1
026500011206DO  1C                   dow       FIL(Z) <> *zeros
026600011206     C                   z-add     FIL(Z)        savPO2
026700011206     C                   exsr      SCRIVI
026800011206     C                   add       1             Z
026900011206E   1C                   enddo
027000011206      *
027100011206     C                   ENDSR
027200011206
027300011206?     *---------------------------------------------------------------*
027400011206?     *--- ROUTINE DI SCRITTURA AZNUM00F -----------------------------*
027500011206?     *---------------------------------------------------------------*
027600011206     C     SCRIVI        BEGSR
027700011206      *
027800011206      * SE IL RECORD ESISTE GIA' NON LO CREO
027900011206      * Se numeratore con anno imposto l'anno passato in KEY
028000011206      * se senza anno imposto *zero
028100011206IF  1c                   if        §cnuFAA = 'S'
028200011206     C                   z-add     PARaaa        KAAA
028300011206X   1C                   else
028400011206     C                   clear                   KAAA
028500011206E   1C                   endif
028600011206      *
028700020206     C     K5NUM1B       setll     AZNUM000                               31
028800011206      *
028900011206IF  1C     *IN31         IFEQ      *OFF
029000011206     C                   clear                   AZNUM000
029100011206      *
029200020206     C                   movel     PARsoc        NUMsoc
029300011206     C                   z-add     KAAA          NUMaaa
029400011206     C                   z-add     savCNU        NUMcnu
029500011206     C                   z-add     savPO1        NUMpo1
029600011206     C                   z-add     savPO2        NUMpo2
029700011206     C                   z-add     DATEU         NUMdac
029800020208     C                   clear                   NUMflo
029900011206      *
030000011206     C                   z-add     §cnuRIN       NUMrin
030100011206     C                   z-add     §cnuRFI       NUMrfi
030200020226      *
030300011206      * imposto il numero iniziale
030400020226IF  2C     §cnuRIN       IFGT      0
030500020226     C     §cnuRIN       sub       §cnuINC       NUMunp
030600020226X   2C                   else
030700011206     C                   z-add     §cnuRIN       NUMunp
030800020226E   2C                   ENDIF
030900011206      *
031000011206     C                   WRITE     AZNUM000
031100011206E   1C                   ENDIF
031200011206      *
031300011206     C                   ENDSR
031400011206
031500011206?     *---------------------------------------------------------------*
031600011206?     *--- OPERAZIONI INIZIALI ---------------------------------------*
031700011206?     *---------------------------------------------------------------*
031800011206     C     *INZSR        BEGSR
031900011206      *
032000011206     C     *ENTRY        PLIST
032100011206     C                   PARM                    KPJBA
032200011206      *
032300011206     C                   movel     KPJBU         PARAM
032400011206      *
032500011206     C     K3TBE1        KLIST
032600011206     C                   KFLD                    TBEcod
032700011206     C                   KFLD                    TBEke1
032800011206     C                   KFLD                    TBEke2
032900011206      *
033000020206     C     K3NUM1        KLIST
033100020206     C                   KFLD                    PARsoc
033200011206     C                   KFLD                    CORaas
033300011206     C                   KFLD                    savCNU
033400020206     C     K4NUM1        KLIST
033500020206     C                   KFLD                    PARsoc
033600011206     C                   KFLD                    CORaas
033700011206     C                   KFLD                    savCNU
033800011206     C                   KFLD                    savPO1
033900020206     C     K5NUM1A       KLIST
034000020206     C                   KFLD                    PARsoc
034100011206     C                   KFLD                    CORaas
034200011206     C                   KFLD                    savCNU
034300011206     C                   KFLD                    FIL(1)
034400011206     C                   KFLD                    savPO2
034500020206     C     K5NUM1B       KLIST
034600020206     C                   KFLD                    PARsoc
034700011206     C                   KFLD                    KAAA
034800011206     C                   KFLD                    savCNU
034900011206     C                   KFLD                    savPO1
035000011206     C                   KFLD                    savPO2
035100011206      *
035200011206     C                   EVAL      CODUT = 1
035300011206      *
035400011206     C                   CALL      'X§PARUT'
035500011206     C                   PARM                    UT§DSE0F
035600011206     C                   MOVEL     REC80         CNCR80
035700011206      ****
035800011206      ** CARICO TABELLA FILIALI GESTITE £1
035900011206      ****
036000011206     C                   CLEAR                   TRUL06DS
036100011206     C                   MOVEL     '£1'          D06COD
036200011206     C                   MOVEL     SIMFEL        D06KEY
036300011206     C                   MOVEL     'L'           D06TLA
036400011206     C                   MOVEL     TRUL06DS      KPJBU
036500011206      *
036600011206     C                   CALL      'TRUL06R'
036700011206     C                   PARM                    KPJBA
036800011206     C                   MOVEL     KPJBU         TRUL06DS
036900011206     C                   MOVEA     LIN           L1
037000011206      ****
037100011206      ** CARICO P.O. DI AZORG00F
037200011206      ****
037300011206     C                   EVAL      X = 1
037400011206      *
037500011206     C     *LOVAL        SETLL     AZORG
037600011206     C                   READ      AZORG
037700011206      *
037800011206    1C                   DOW       not %EOF
037900011206      * ESCLUDO ANNULLATI E P.O. FITTIZZI
038000011206     c                   IF         ORGFVA = *blanks  and
038100011206     c                             (ORGFAG = 'F'      or  ORGFAG = 'A')
038200011206     C                   MOVEL     ORGFIL        FIL(X)
038300011206     C                   ADD       1             X
038400011206     C                   ENDIF
038500011206      *
038600011206     C                   READ      AZORG
038700011206    1C                   ENDDO
038800011206      ****
038900011206      ** GIRO *DATE
039000011206      ****
039100011206     C                   MOVE      *DATE         G02DAT
039200011206     C                   MOVEL     *BLANKS       G02ERR
039300011206     C                   CALL      'XSRDA8'
039400011206     C                   PARM                    WLBDAT
039500011206     C                   Z-ADD     G02INV        DATEU
039600011206      *
039700011206     C                   ENDSR
