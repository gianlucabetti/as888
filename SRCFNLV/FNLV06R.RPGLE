000100931222     H DECEDIT('0,') DATEDIT(*DMY.)
000200010130      * FNLV06R *-----------------------------------------------------*
000300010130      *       PROGRAMMA CREAZIONE FILE NUMERATORI PER FILIALE         *
000400010130      *---------------------------------------------------------------*
000500061201     FFLNUF01L  IF A E           K DISK    usropn
000600931222     FAZORG01L  IF   E           K DISK
000700931222     FTABEL00F  IF   E           K DISK
000800010130
000900010130      *--------------------------------------------
001000061201     d cmdt            s             60    dim(01) ctdata perrcd(1)
001100061201     d
001200010130     D FIL             S              3  0 DIM(500)
001300010131     D  L1             S              3  0 DIM(30)
001400010130
001500931222     D PARAM           DS
001600010130      *   Codice P.O.
001700010130     D  PARFIL                 3      5  0
001800010130      *   Codice P.O. elaboratore di parfil
001900931222     D  PARFEL                 6      8  0
002000010130      *   Codice numeratore
002100931228     D  PARCNU                 9     11
002200010130      *   Tipo filiale Part/Arr
002300931227     D  PARFTF                12     12
002400010130      *   Anno
002500991209     D  PARAAN                13     16  0
002600010130
002700931222     D KPJBA         E DS
002800061201     D tibs55ds      E DS
002900931222     D DS1C          E DS
003000010130     D TRUL06DS      E DS
003100010130     D  LIN                    1     90  0 DIM(30)
003200971114     D CNCR80        E DS
003300010130     D UT§DSE0F      E DS
003400010130
003500061201     d comman          s             80
003600061201     d lenght          s             15  5 inz(80)
003700010131     d  COD            s                   like(tblcod) INZ('1C')
003800010131     d  KEY            S                   like(tblkey)
003900010131     d  KAAN           S                   like(NUFaaa)
004000010131     d  SAVCNU         S                   like(NUFcnu)
004100010131     d  SAVFIL         S                   like(NUFfil)
004200010131     d  SAVFL2         S                   like(NUFfl2)
004300010131     d  dateu          S              6  0
004400010131     d  coraas         S              2  0
004500010131     d  z              S              3  0
004600010131     d  x              S              3  0
004700010130
004800010130      *****************************************************************
004900010130
005000010131     C                   if         PARCNU <> *BLANKS
005100010131     C                   MOVEL     PARCNU        KEY
005200010131     C     KTAB          CHAIN     TABEL                              35
005300010131   X1C                   else
005400931222     C     KTAB2         SETLL     TABEL
005500010131     C     KTAB2         READE     TABEL                                  35
005600010131    1C                   endif
005700010130      *
005800010131    1C     *IN35         DOWEQ     *OFF
005900010131      *
006000010130      * ESCLUDO ANNULLATI
006100010131    2C                   if        TBLFLG = *blanks
006200931228     C                   MOVEL     TBLUNI        DS1C
006300931228     C                   MOVEL     TBLKEY        SAVCNU
006400010130      *
006500010130      * ESCLUDO QUELLI DA NON RICREARE
006600931228    3C     §1CNCR        IFNE      'S'
006700010130      *
006800010131      * ESCLUDO QUELLI CON ANNO SE NON RICEVUTO
006900010131     C                   if        §1CFAA = *blanks  OR
007000010131     C                             (§1CFAA = 'S'  and  PARAAN <> 0)
007100010131      * Se numeratore con anno imposto l'anno corrente in KEY
007200010131      * se senza anno imposto *zero
007300010131     c                   if        §1CFAA = 'S'
007400010131     C                   Z-ADD     UYEAR         CORAAS
007500010131     C                   else
007600010131     C                   clear                   CORAAS
007700010131     C                   endif
007800010131      *
007900010131     c                   SELECT
008000010131      * ARRIVO
008100010131     c                   WHEN       PARftf = 'A'
008200010131     C                   exsr      CRTNU2
008300010131      * PARTENZA Controllo se numeratore solo per 1° o 2° livello
008400010131     c                   WHEN      (§1cFNU = 'S' and PARfil = PARfel)  OR
008500010131     c                             (§1cFNU = 'L' and PARfil <> PARfel) OR
008600010131     c                              §1cFNU = *blanks
008700010131     C                   exsr      CRTNUF
008800010131    5C                   ENDSL
008900010130      *
009000931228    4C                   ENDIF
009100931228    3C                   ENDIF
009200931228    2C                   ENDIF
009300010130      *
009400010131      *  SE COD.NUMERATORE RICEVUTO VADO A FINE
009500010131     C                   if         PARCNU <> *BLANKS
009600010131     C                   LEAVE
009700010131     c                   else
009800010131     C     KTAB2         READE     TABEL                                  35
009900010131     c                   endif
010000010131      *
010100931228    1C                   ENDDO
010200010130      *
010300010130     C                   EVAL      *INLR = *ON
010400010131      *---------------------------------------------------------------*
010500010131      *--- ROUTINE DI CREAZIONE FLNUF00F CON PARFIL COME LNP ---------*
010600010131      *---------------------------------------------------------------*
010700931223     C     CRTNUF        BEGSR
010800010131
010900010131      ****
011000010131      ** §1CCK1 = 'TUT'  ===>  TUTTE LE FILIALI
011100010131      ** §1CCK1 = 'FIL'  ===>  FILIALE DA CREARE
011200010131      **    ALTRIMENTI   ===>  QUELLO CHE C'E'
011300010131      **
011400010131      ** §1CCK2 = 'TUT'  ===>  TUTTE LE FILIALI
011500010131      ** §1CCK2 = 'FIL'  ===>  FILIALE DA CREARE
011600010131      **    ALTRIMENTI   ===>  QUELLO CHE C'E'
011700010131      ****
011800010131
011900931227     C                   SELECT
012000010131
012100010131     C                   WHEN      §1CCK1 = 'TUT'  and   §1CCK2 = 'TUT'
012200010131     C                   eval      x = 1
012300010131    3C                   Dow       FIL(X) <> *zeros
012400010131     C                   z-add     FIL(X)        SAVFIL
012500010131     C                   exsr      TUTTI
012600010131     C                   add       1             X
012700010131    3C                   EndDo
012800010131
012900010131     C                   WHEN      §1CCK1 = 'TUT'
013000010131      *
013100010131    3C                   If        §1CCK2 = 'FIL'
013200010131     C                   z-add     PARFIL        SAVFL2
013300010131   X3C                   Else
013400010131     C                   movel     §1CCK2        SAVFL2
013500010131    3C                   Endif
013600010131      *
013700010131     C                   eval      x = 1
013800010131    3C                   Dow       FIL(X) <> *zeros
013900010131     C                   z-add     FIL(X)        SAVFIL
014000010131     C                   exsr      SCRIVI
014100010131     C                   add       1             X
014200010131    3C                   EndDo
014300010131
014400010131     C                   WHEN      §1CCK2 =  'TUT'
014500010131      *
014600010131    3C                   If        §1CCK1 = 'FIL'
014700010131     C                   z-add     PARFIL        SAVFIL
014800010131   X3C                   Else
014900010131     C                   movel     §1CCK1        SAVFIL
015000010131    3C                   EndIf
015100010131      *
015200010131     C                   eval      z = 1
015300010131    2C                   Dow       FIL(Z) <> *zeros
015400010131     C                   z-add     FIL(Z)        SAVFL2
015500010131     C                   exsr      SCRIVI
015600010131     C                   add       1             Z
015700010131    2C                   EndDo
015800010131
015900931227     C                   OTHER
016000010131      *
016100010131    3C                   if        §1CCK1 = 'FIL'
016200931227     C                   Z-ADD     PARFIL        SAVFIL
016300010131   X3C                   else
016400931227     C                   MOVEL     §1CCK1        SAVFIL
016500010131    3C                   endif
016600010131      *
016700010131    3C                   if        §1CCK2 = 'FIL'
016800931227     C                   Z-ADD     PARFIL        SAVFL2
016900010131   X3C                   else
017000931227     C                   MOVEL     §1CCK2        SAVFL2
017100010131    3C                   endif
017200010131      *
017300010131     C                   exsr      SCRIVI
017400010131
017500931227     C                   ENDSL
017600010131      *
017700931222     C                   ENDSR
017800931227     C*
017900010131      *---------------------------------------------------------------*
018000010131      *--- ROUTINE DI CREAZIONE FLNUF00F CON PARFIL COME LNA ---------*
018100010131      *---------------------------------------------------------------*
018200931227     C     CRTNU2        BEGSR
018300010131
018400931227     C                   SELECT
018500010131
018600010131     C                   WHEN      §1CCK1 = 'TUT'  and  §1CCK2 = 'TUT'
018700010131      * Creo per il P.O. tutti i record dei P.O. esistenti
018800010131     C                   z-add     PARFIL        SAVFIL
018900010131     C                   exsr      TUTTI
019000010131      * Creo per tutti i record dei P.O. esistenti il P.O. immesso
019100010131     C                   z-add     PARFIL        SAVFL2
019200010131     C                   z-add     1             X
019300010131     C                   Dow       FIL(X) <> *zeros
019400010131     C                   z-add     FIL(X)        SAVFIL
019500010131     C                   exsr      SCRIVI
019600010131     C                   add       1             X
019700010131     C                   EndDo
019800010131
019900010131     C                   WHEN      §1CCK1 = 'TUT'  and  §1CCK2 = 'FIL'
020000010131     C                   eval      x = 1
020100010131     C                   z-add     PARFIL        SAVFIL
020200010131     C                   Dow       L1(X) <> *zeros
020300010131     C                   z-add     L1(X)         SAVFL2
020400010131      * solo se trovato il record per un'altro P.O. scrivo FLNUF
020500010131     C     KNUF3         setll     FLNUF                                  30
020600010131     C   30              exsr      SCRIVI
020700010131     C                   add       1             X
020800010131     C                   EndDo
020900010131
021000010131     C                   WHEN      §1CCK1 = 'TUT'
021100010131     C                   z-add     PARFIL        SAVFIL
021200010131     C                   movel     §1CCK2        SAVFL2
021300010131     C                   exsr      SCRIVI
021400010131
021500010131     C                   WHEN      §1CCK2 = 'TUT'  and  §1CCK1 = 'FIL'
021600010131     C                   z-add     PARFIL        SAVFL2
021700010131     C                   clear                   SAVFIL
021800010131      *
021900010131     C     KNUF2         setgt     FLNUF
022000010131     C     KNUF4         reade     FLNUF
022100010131      *
022200010131     C                   Dow       not %EOF
022300010131     C                   z-add     NUFFIL        SAVFIL
022400010131     C                   exsr      SCRIVI
022500010131     C     KNUF2         setgt     FLNUF
022600010131     C     KNUF4         reade     FLNUF
022700010131     C                   EndDo
022800010131
022900010131     C                   WHEN      §1CCK2 = 'TUT'
023000010131     C                   z-add     PARFIL        SAVFL2
023100010131     C                   movel     §1CCK1        SAVFIL
023200010131     C                   exsr      SCRIVI
023300010131
023400931228     C                   ENDSL
023500010131      *
023600931227     C                   ENDSR
023700010131      *--------------------------------------------------------------*
023800010131      * CREO PER TUTTE LE FILIALI UN RECORD IN FLNUF ----------------*
023900010131      *--------------------------------------------------------------*
024000010131     C     TUTTI         BEGSR
024100010131      *
024200010131     C                   eval      z = 1
024300010131    4C                   dow       FIL(Z) <> *zeros
024400010131     C                   Z-ADD     FIL(Z)        SAVFL2
024500010131     C                   EXSR      SCRIVI
024600010131     C                   ADD       1             Z
024700010131    4C                   enddo
024800010131      *
024900010131     C                   ENDSR
025000010131      *--------------------------------------------------------------*
025100010131     C*--- ROUTINE DI SCRITTURA FLNUF00F -----------------------------*
025200010131      *--------------------------------------------------------------*
025300010131     C     SCRIVI        BEGSR
025400010131      *
025500010131      * SE IL RECORD ESISTE GIA' NON LO CREO
025600010131      * Se numeratore con anno imposto l'anno passato in KEY
025700010131      * se senza anno imposto *zero
025800010131     c                   if        §1CFAA = 'S'
025900010131     C                   Z-ADD     PARAAN        KAAN
026000010131     C                   else
026100010131     C                   clear                   KAAN
026200010131     C                   endif
026300010131      *
026400010131     C     KNUF          SETLL     FLNUF                                  31
026500010131      *
026600010131    1C     *IN31         IFEQ      *OFF
026700010131     C                   CLEAR                   FLNUF
026800010131      *
026900010131     C                   Z-ADD     KAAN          NUFAAA
027000010131     C                   MOVEL     TBLKEY        NUFCNU
027100010131     C                   Z-ADD     SAVFIL        NUFFIL
027200010131     C                   Z-ADD     SAVFL2        NUFFL2
027300010131     C                   Z-ADD     DATEU         NUFDAT
027400010131      *
027500010131      * Test se numeratore da azzerare a nuovo anno
027600010131    2C                   if        §1CFG3 <> 'S'
027700010131     C                   Z-ADD     §1CNIN        NUFNUM
027800010131   X2C                   else
027900010131      *
028000010131     C                   Z-ADD     §1CRIN        NUFRIN
028100010131     C                   Z-ADD     §1CRFI        NUFRFI
028200010131      * imposto il numero iniziale
028300010131    3C     §1CRIN        IFGT      0
028400010131     C     §1CRIN        SUB       1             NUFNUM
028500010131   X3C                   ELSE
028600010131     C                   Z-ADD     §1CRIN        NUFNUM
028700010131    3C                   ENDIF
028800010131      *
028900010131    2C                   endif
029000010131      *
029100010131     C                   WRITE     FLNUF
029200010131    1C                   ENDIF
029300010131      *
029400010131     C                   ENDSR
029500010130      *---------------------------------------------------------------*
029600010130      *--- OPERAZIONI INIZIALI ---------------------------------------*
029700010130      *---------------------------------------------------------------*
029800010130     C     *inzsr        BEGSR
029900010130      *
030000010130     C     *ENTRY        PLIST
030100010130     C                   PARM                    KPJBA
030200010130      *
030300010130     C                   clear                   PARAM
030400010130     C                   MOVEL     KPJBU         PARAM
030500061201     c*
030600061201     c* Reperisco S.I.
030700061201     c                   clear                   tibs55ds
030800061201     c                   movel     'L'           i50tla
030900061201     c* come p.o. di partenza un p.o. di filiale
031000061205     c                   eval      i50apo=001
031100061201     c                   call      'TIBS55R'
031200061201     c                   parm                    tibs55ds
031300061201     c
031400061201     c                   Clear                   comman
031500061201     c                   Movel(p)  cmdt(1)       comman
031600061205     c                   Eval      %Subst(comman:30:10) = o50ala
031700061201     c                   Eval      comman =
031800061201     c                             %trim(comman) + '/FLNUF01L)'
031900061201     c                   Call      'QCMDEXC'                            99
032000061201     c                   Parm                    comman
032100061201     c                   Parm                    lenght
032200061201     c
032300061201     c  n99              Open      FLNUF01L
032400061201     c
032500061201     c
032600010130      * ACCESSO TABEL00F
032700010130     C     KTAB          KLIST
032800010130     C                   KFLD                    CODUT
032900010130     C                   KFLD                    COD
033000010130     C                   KFLD                    KEY
033100010130     C     KTAB2         KLIST
033200010130     C                   KFLD                    CODUT
033300010130     C                   KFLD                    COD
033400010130     C     KNUF          KLIST
033500010130     C                   KFLD                    KAAN
033600010130     C                   KFLD                    SAVCNU
033700010130     C                   KFLD                    SAVFIL
033800010130     C                   KFLD                    SAVFL2
033900010130     C     KNUF2         KLIST
034000010130     C                   KFLD                    CORAAS
034100010130     C                   KFLD                    SAVCNU
034200010130     C                   KFLD                    SAVFIL
034300010130     C     KNUF3         KLIST
034400010131     C                   KFLD                    CORAAS
034500010130     C                   KFLD                    SAVCNU
034600010130     C                   KFLD                    FIL(1)
034700010130     C                   KFLD                    SAVFL2
034800010130     C     KNUF4         KLIST
034900010130     C                   KFLD                    CORAAS
035000010130     C                   KFLD                    SAVCNU
035100010130      *
035200010131     c                   eval      codut = 1
035300010131      *
035400010130     C                   CALL      'X§PARUT'
035500010130     C                   PARM                    UT§DSE0F
035600010130     C                   MOVEL     REC80         CNCR80
035700010130      ****
035800010130      ** CARICO TABELLA FILIALI GESTITE £1
035900010130      ****
036000010130     C                   CLEAR                   TRUL06DS
036100010130     C                   MOVE      '£1'          D06COD
036200010130     C                   MOVEL     SIMFEL        D06KEY
036300010130     C                   MOVEL     'L'           D06TLA
036400010130     C                   MOVEL     TRUL06DS      KPJBU
036500010130      *
036600010130     C                   CALL      'TRUL06R'
036700010130     C                   PARM                    KPJBA
036800010130     C                   MOVEL     KPJBU         TRUL06DS
036900010130     C                   MOVEA     LIN           L1
037000010130      ****
037100010130      ** CARICO P.O. DI AZORG00F
037200010130      ****
037300010131     C                   eval      x = 1
037400010130      *
037500010130     C     *LOVAL        SETLL     AZORG
037600010130     C                   READ      AZORG
037700010130      *
037800010130    1C                   DOW       not %EOF
037900010130      * ESCLUDO ANNULLATI E P.O. FITTIZZI
038000010130     c                   IF        ORGFVA = *blanks  and
038100010130     c                             (ORGFAG = 'F'  or  ORGFAG = 'A')
038200010130     C                   MOVEL     ORGFIL        FIL(X)
038300010130     C                   ADD       1             X
038400010130     C                   ENDIF
038500010130      *
038600010130     C                   READ      AZORG
038700010130    1C                   ENDDO
038800010130     C****
038900010130     C** GIRO UDATE
039000010130     C****
039100010131     C                   clear                   DATEU
039200010130     C                   Z-ADD     UDATE         DATEU
039300010130     C                   MOVE      UDAY          DATEU
039400010130     C                   MOVEL     UYEAR         DATEU
039500010130      *
039600010130     C                   ENDSR
039700061201     c**
039800061201** cmdt
039900061201OVRDBF FILE(FLNUF01L) TOFILE(
