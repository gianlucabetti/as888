000100931222     H DECEDIT('0,') DATEDIT(*DMY.)
000200010130      * FNLV06R *-----------------------------------------------------*
000300010130      *       PROGRAMMA CREAZIONE FILE NUMERATORI PER FILIALE         *
000400010130      *---------------------------------------------------------------*
000500931228     FFLNUF01L  IF A E           K DISK
000600931222     FAZORG01L  IF   E           K DISK
000700931222     FTABEL00F  IF   E           K DISK
000800010130
000900010130      *--------------------------------------------
001000010130     D FIL             S              3  0 DIM(500)
001100010131     D  L1             S              3  0 DIM(30)
001200010130
001300931222     D PARAM           DS
001400010130      *   Codice P.O.
001500010130     D  PARFIL                 3      5  0
001600010130      *   Codice P.O. elaboratore di parfil
001700931222     D  PARFEL                 6      8  0
001800010130      *   Codice numeratore
001900931228     D  PARCNU                 9     11
002000010130      *   Tipo filiale Part/Arr
002100931227     D  PARFTF                12     12
002200010130      *   Anno
002300991209     D  PARAAN                13     16  0
002400010130
002500931222     D KPJBA         E DS
002600931222     D DS1C          E DS
002700010130     D TRUL06DS      E DS
002800010130     D  LIN                    1     90  0 DIM(30)
002900971114     D CNCR80        E DS
003000010130     D UT§DSE0F      E DS
003100010130
003200010131     d  COD            s                   like(tblcod) INZ('1C')
003300010131     d  KEY            S                   like(tblkey)
003400010131     d  KAAN           S                   like(NUFaaa)
003500010131     d  SAVCNU         S                   like(NUFcnu)
003600010131     d  SAVFIL         S                   like(NUFfil)
003700010131     d  SAVFL2         S                   like(NUFfl2)
003800010131     d  dateu          S              6  0
003900010131     d  coraas         S              2  0
004000010131     d  z              S              3  0
004100010131     d  x              S              3  0
004200010130
004300010130      *****************************************************************
004400010130
004500010131     C                   if         PARCNU <> *BLANKS
004600010131     C                   MOVEL     PARCNU        KEY
004700010131     C     KTAB          CHAIN     TABEL                              35
004800010131   X1C                   else
004900931222     C     KTAB2         SETLL     TABEL
005000010131     C     KTAB2         READE     TABEL                                  35
005100010131    1C                   endif
005200010130      *
005300010131    1C     *IN35         DOWEQ     *OFF
005400010131      *
005500010130      * ESCLUDO ANNULLATI
005600010131    2C                   if        TBLFLG = *blanks
005700931228     C                   MOVEL     TBLUNI        DS1C
005800931228     C                   MOVEL     TBLKEY        SAVCNU
005900010130      *
006000010130      * ESCLUDO QUELLI DA NON RICREARE
006100931228    3C     §1CNCR        IFNE      'S'
006200010130      *
006300010131      * ESCLUDO QUELLI CON ANNO SE NON RICEVUTO
006400010131     C                   if        §1CFAA = *blanks  OR
006500010131     C                             (§1CFAA = 'S'  and  PARAAN <> 0)
006600010131      * Se numeratore con anno imposto l'anno corrente in KEY
006700010131      * se senza anno imposto *zero
006800010131     c                   if        §1CFAA = 'S'
006900010131     C                   Z-ADD     UYEAR         CORAAS
007000010131     C                   else
007100010131     C                   clear                   CORAAS
007200010131     C                   endif
007300010131      *
007400010131     c                   SELECT
007500010131      * ARRIVO
007600010131     c                   WHEN       PARftf = 'A'
007700010131     C                   exsr      CRTNU2
007800010131      * PARTENZA Controllo se numeratore solo per 1° o 2° livello
007900010131     c                   WHEN      (§1cFNU = 'S' and PARfil = PARfel)  OR
008000010131     c                             (§1cFNU = 'L' and PARfil <> PARfel) OR
008100010131     c                              §1cFNU = *blanks
008200010131     C                   exsr      CRTNUF
008300010131    5C                   ENDSL
008400010130      *
008500931228    4C                   ENDIF
008600931228    3C                   ENDIF
008700931228    2C                   ENDIF
008800010130      *
008900010131      *  SE COD.NUMERATORE RICEVUTO VADO A FINE
009000010131     C                   if         PARCNU <> *BLANKS
009100010131     C                   LEAVE
009200010131     c                   else
009300010131     C     KTAB2         READE     TABEL                                  35
009400010131     c                   endif
009500010131      *
009600931228    1C                   ENDDO
009700010130      *
009800010130     C                   EVAL      *INLR = *ON
009900010131      *---------------------------------------------------------------*
010000010131      *--- ROUTINE DI CREAZIONE FLNUF00F CON PARFIL COME LNP ---------*
010100010131      *---------------------------------------------------------------*
010200931223     C     CRTNUF        BEGSR
010300010131
010400010131      ****
010500010131      ** §1CCK1 = 'TUT'  ===>  TUTTE LE FILIALI
010600010131      ** §1CCK1 = 'FIL'  ===>  FILIALE DA CREARE
010700010131      **    ALTRIMENTI   ===>  QUELLO CHE C'E'
010800010131      **
010900010131      ** §1CCK2 = 'TUT'  ===>  TUTTE LE FILIALI
011000010131      ** §1CCK2 = 'FIL'  ===>  FILIALE DA CREARE
011100010131      **    ALTRIMENTI   ===>  QUELLO CHE C'E'
011200010131      ****
011300010131
011400931227     C                   SELECT
011500010131
011600010131     C                   WHEN      §1CCK1 = 'TUT'  and   §1CCK2 = 'TUT'
011700010131     C                   eval      x = 1
011800010131    3C                   Dow       FIL(X) <> *zeros
011900010131     C                   z-add     FIL(X)        SAVFIL
012000010131     C                   exsr      TUTTI
012100010131     C                   add       1             X
012200010131    3C                   EndDo
012300010131
012400010131     C                   WHEN      §1CCK1 = 'TUT'
012500010131      *
012600010131    3C                   If        §1CCK2 = 'FIL'
012700010131     C                   z-add     PARFIL        SAVFL2
012800010131   X3C                   Else
012900010131     C                   movel     §1CCK2        SAVFL2
013000010131    3C                   Endif
013100010131      *
013200010131     C                   eval      x = 1
013300010131    3C                   Dow       FIL(X) <> *zeros
013400010131     C                   z-add     FIL(X)        SAVFIL
013500010131     C                   exsr      SCRIVI
013600010131     C                   add       1             X
013700010131    3C                   EndDo
013800010131
013900010131     C                   WHEN      §1CCK2 =  'TUT'
014000010131      *
014100010131    3C                   If        §1CCK1 = 'FIL'
014200010131     C                   z-add     PARFIL        SAVFIL
014300010131   X3C                   Else
014400010131     C                   movel     §1CCK1        SAVFIL
014500010131    3C                   EndIf
014600010131      *
014700010131     C                   eval      z = 1
014800010131    2C                   Dow       FIL(Z) <> *zeros
014900010131     C                   z-add     FIL(Z)        SAVFL2
015000010131     C                   exsr      SCRIVI
015100010131     C                   add       1             Z
015200010131    2C                   EndDo
015300010131
015400931227     C                   OTHER
015500010131      *
015600010131    3C                   if        §1CCK1 = 'FIL'
015700931227     C                   Z-ADD     PARFIL        SAVFIL
015800010131   X3C                   else
015900931227     C                   MOVEL     §1CCK1        SAVFIL
016000010131    3C                   endif
016100010131      *
016200010131    3C                   if        §1CCK2 = 'FIL'
016300931227     C                   Z-ADD     PARFIL        SAVFL2
016400010131   X3C                   else
016500931227     C                   MOVEL     §1CCK2        SAVFL2
016600010131    3C                   endif
016700010131      *
016800010131     C                   exsr      SCRIVI
016900010131
017000931227     C                   ENDSL
017100010131      *
017200931222     C                   ENDSR
017300931227     C*
017400010131      *---------------------------------------------------------------*
017500010131      *--- ROUTINE DI CREAZIONE FLNUF00F CON PARFIL COME LNA ---------*
017600010131      *---------------------------------------------------------------*
017700931227     C     CRTNU2        BEGSR
017800010131
017900931227     C                   SELECT
018000010131
018100010131     C                   WHEN      §1CCK1 = 'TUT'  and  §1CCK2 = 'TUT'
018200010131      * Creo per il P.O. tutti i record dei P.O. esistenti
018300010131     C                   z-add     PARFIL        SAVFIL
018400010131     C                   exsr      TUTTI
018500010131      * Creo per tutti i record dei P.O. esistenti il P.O. immesso
018600010131     C                   z-add     PARFIL        SAVFL2
018700010131     C                   z-add     1             X
018800010131     C                   Dow       FIL(X) <> *zeros
018900010131     C                   z-add     FIL(X)        SAVFIL
019000010131     C                   exsr      SCRIVI
019100010131     C                   add       1             X
019200010131     C                   EndDo
019300010131
019400010131     C                   WHEN      §1CCK1 = 'TUT'  and  §1CCK2 = 'FIL'
019500010131     C                   eval      x = 1
019600010131     C                   z-add     PARFIL        SAVFIL
019700010131     C                   Dow       L1(X) <> *zeros
019800010131     C                   z-add     L1(X)         SAVFL2
019900010131      * solo se trovato il record per un'altro P.O. scrivo FLNUF
020000010131     C     KNUF3         setll     FLNUF                                  30
020100010131     C   30              exsr      SCRIVI
020200010131     C                   add       1             X
020300010131     C                   EndDo
020400010131
020500010131     C                   WHEN      §1CCK1 = 'TUT'
020600010131     C                   z-add     PARFIL        SAVFIL
020700010131     C                   movel     §1CCK2        SAVFL2
020800010131     C                   exsr      SCRIVI
020900010131
021000010131     C                   WHEN      §1CCK2 = 'TUT'  and  §1CCK1 = 'FIL'
021100010131     C                   z-add     PARFIL        SAVFL2
021200010131     C                   clear                   SAVFIL
021300010131      *
021400010131     C     KNUF2         setgt     FLNUF
021500010131     C     KNUF4         reade     FLNUF
021600010131      *
021700010131     C                   Dow       not %EOF
021800010131     C                   z-add     NUFFIL        SAVFIL
021900010131     C                   exsr      SCRIVI
022000010131     C     KNUF2         setgt     FLNUF
022100010131     C     KNUF4         reade     FLNUF
022200010131     C                   EndDo
022300010131
022400010131     C                   WHEN      §1CCK2 = 'TUT'
022500010131     C                   z-add     PARFIL        SAVFL2
022600010131     C                   movel     §1CCK1        SAVFIL
022700010131     C                   exsr      SCRIVI
022800010131
022900931228     C                   ENDSL
023000010131      *
023100931227     C                   ENDSR
023200010131      *--------------------------------------------------------------*
023300010131      * CREO PER TUTTE LE FILIALI UN RECORD IN FLNUF ----------------*
023400010131      *--------------------------------------------------------------*
023500010131     C     TUTTI         BEGSR
023600010131      *
023700010131     C                   eval      z = 1
023800010131    4C                   dow       FIL(Z) <> *zeros
023900010131     C                   Z-ADD     FIL(Z)        SAVFL2
024000010131     C                   EXSR      SCRIVI
024100010131     C                   ADD       1             Z
024200010131    4C                   enddo
024300010131      *
024400010131     C                   ENDSR
024500010131      *--------------------------------------------------------------*
024600010131     C*--- ROUTINE DI SCRITTURA FLNUF00F -----------------------------*
024700010131      *--------------------------------------------------------------*
024800010131     C     SCRIVI        BEGSR
024900010131      *
025000010131      * SE IL RECORD ESISTE GIA' NON LO CREO
025100010131      * Se numeratore con anno imposto l'anno passato in KEY
025200010131      * se senza anno imposto *zero
025300010131     c                   if        §1CFAA = 'S'
025400010131     C                   Z-ADD     PARAAN        KAAN
025500010131     C                   else
025600010131     C                   clear                   KAAN
025700010131     C                   endif
025800010131      *
025900010131     C     KNUF          SETLL     FLNUF                                  31
026000010131      *
026100010131    1C     *IN31         IFEQ      *OFF
026200010131     C                   CLEAR                   FLNUF
026300010131      *
026400010131     C                   Z-ADD     KAAN          NUFAAA
026500010131     C                   MOVEL     TBLKEY        NUFCNU
026600010131     C                   Z-ADD     SAVFIL        NUFFIL
026700010131     C                   Z-ADD     SAVFL2        NUFFL2
026800010131     C                   Z-ADD     DATEU         NUFDAT
026900010131      *
027000010131      * Test se numeratore da azzerare a nuovo anno
027100010131    2C                   if        §1CFG3 <> 'S'
027200010131     C                   Z-ADD     §1CNIN        NUFNUM
027300010131   X2C                   else
027400010131      *
027500010131     C                   Z-ADD     §1CRIN        NUFRIN
027600010131     C                   Z-ADD     §1CRFI        NUFRFI
027700010131      * imposto il numero iniziale
027800010131    3C     §1CRIN        IFGT      0
027900010131     C     §1CRIN        SUB       1             NUFNUM
028000010131   X3C                   ELSE
028100010131     C                   Z-ADD     §1CRIN        NUFNUM
028200010131    3C                   ENDIF
028300010131      *
028400010131    2C                   endif
028500010131      *
028600010131     C                   WRITE     FLNUF
028700010131    1C                   ENDIF
028800010131      *
028900010131     C                   ENDSR
029000010130      *---------------------------------------------------------------*
029100010130      *--- OPERAZIONI INIZIALI ---------------------------------------*
029200010130      *---------------------------------------------------------------*
029300010130     C     *inzsr        BEGSR
029400010130      *
029500010130     C     *ENTRY        PLIST
029600010130     C                   PARM                    KPJBA
029700010130      *
029800010130     C                   clear                   PARAM
029900010130     C                   MOVEL     KPJBU         PARAM
030000010130
030100010130      * ACCESSO TABEL00F
030200010130     C     KTAB          KLIST
030300010130     C                   KFLD                    CODUT
030400010130     C                   KFLD                    COD
030500010130     C                   KFLD                    KEY
030600010130     C     KTAB2         KLIST
030700010130     C                   KFLD                    CODUT
030800010130     C                   KFLD                    COD
030900010130     C     KNUF          KLIST
031000010130     C                   KFLD                    KAAN
031100010130     C                   KFLD                    SAVCNU
031200010130     C                   KFLD                    SAVFIL
031300010130     C                   KFLD                    SAVFL2
031400010130     C     KNUF2         KLIST
031500010130     C                   KFLD                    CORAAS
031600010130     C                   KFLD                    SAVCNU
031700010130     C                   KFLD                    SAVFIL
031800010130     C     KNUF3         KLIST
031900010131     C                   KFLD                    CORAAS
032000010130     C                   KFLD                    SAVCNU
032100010130     C                   KFLD                    FIL(1)
032200010130     C                   KFLD                    SAVFL2
032300010130     C     KNUF4         KLIST
032400010130     C                   KFLD                    CORAAS
032500010130     C                   KFLD                    SAVCNU
032600010130      *
032700010131     c                   eval      codut = 1
032800010131      *
032900010130     C                   CALL      'X§PARUT'
033000010130     C                   PARM                    UT§DSE0F
033100010130     C                   MOVEL     REC80         CNCR80
033200010130      ****
033300010130      ** CARICO TABELLA FILIALI GESTITE £1
033400010130      ****
033500010130     C                   CLEAR                   TRUL06DS
033600010130     C                   MOVE      '£1'          D06COD
033700010130     C                   MOVEL     SIMFEL        D06KEY
033800010130     C                   MOVEL     'L'           D06TLA
033900010130     C                   MOVEL     TRUL06DS      KPJBU
034000010130      *
034100010130     C                   CALL      'TRUL06R'
034200010130     C                   PARM                    KPJBA
034300010130     C                   MOVEL     KPJBU         TRUL06DS
034400010130     C                   MOVEA     LIN           L1
034500010130      ****
034600010130      ** CARICO P.O. DI AZORG00F
034700010130      ****
034800010131     C                   eval      x = 1
034900010130      *
035000010130     C     *LOVAL        SETLL     AZORG
035100010130     C                   READ      AZORG
035200010130      *
035300010130    1C                   DOW       not %EOF
035400010130      * ESCLUDO ANNULLATI E P.O. FITTIZZI
035500010130     c                   IF        ORGFVA = *blanks  and
035600010130     c                             (ORGFAG = 'F'  or  ORGFAG = 'A')
035700010130     C                   MOVEL     ORGFIL        FIL(X)
035800010130     C                   ADD       1             X
035900010130     C                   ENDIF
036000010130      *
036100010130     C                   READ      AZORG
036200010130    1C                   ENDDO
036300010130     C****
036400010130     C** GIRO UDATE
036500010130     C****
036600010131     C                   clear                   DATEU
036700010130     C                   Z-ADD     UDATE         DATEU
036800010130     C                   MOVE      UDAY          DATEU
036900010130     C                   MOVEL     UYEAR         DATEU
037000010130      *
037100010130     C                   ENDSR
