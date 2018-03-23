000100020311      *---------*
000200020311      * FIDN40R *-----------------------------------------------------*
000300020311      *---------*                                                     *
000400020311      *           STATISTICA AMMINISTRATIVA DANNI         - FILTRO -  *
000500020311      *---------------------------------------------------------------*
000600020311
000700020311     H DECEDIT('0,') DATEDIT(*DMY.)
000800020311
000900020311      *---------------------------------------------------------------*
001000020311
001100020311     FFIDN40D   CF   E             WORKSTN
001200020311
001300020311      *---------------------------------------------------------------*
001400020311
001500020311      *   S C H I E R E
001600020311
001700020311     D MSG             S             78    DIM(4) CTDATA PERRCD(1)
001800020311
001900020311      *   D S   E S T E R N E / I N T E R N E
002000020311
002100020311     D KPJBA         E DS
002200020311      *
002300020311      * Parametri per elaborazione batch         - FIDN41R -
002400020311     D FIDN41DS      E DS                  INZ
002500020311      *
002600020311      * Parametri per recupero S.I. in cui sono  - X§PARUT -
002700020311     D UT§DSE0F      E DS                  INZ
002800020311      *
002900020311      * Parametri per controllo ed inversione data - XSRDA8 -
003000020311     D WLBDAT          DS
003100020311     D  G02DAT                 1      8  0
003200020311     D  G02INV                 9     16  0
003300020311     D  G02ERR                17     17
003400020311     D  G02TGI                18     22  0
003500020311      *
003600020311      * Reperimento nome PGM
003700020311     D STATUS         SDS           333
003800020311     D  DSPGM            *PROC
003900020311
004000020311      *   V A R I A B I L I
004100020311
004200020311     D wDIP            S                   LIKE(I41DIP) INZ(*loval)
004300020311     D wDFP            S                   LIKE(I41DFP) INZ(*hival)
004400020311     D wDATA0          S              8  0              INZ
004500020311     D wDATA1          S               D   DATFMT(*EUR) INZ
004600020311     D wDATA2          S               D   DATFMT(*EUR) INZ
004700020311
004800020311      *   C O S T A N T I
004900020311
005000020311
005100020311      *---------------------------------------------------------------*
005200020311
005300020311      *****************************************************************
005400020311      * RIEPILOGO INDICATORI
005500020311      *****************************************************************
005600020311      * 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
005700020311      * 41    - ERRORE: Data dal errata
005800020311      * 42    - ERRORE: Data  al errata
005900020311      *****************************************************************
006000020311      *
006100020311     C     *ENTRY        plist
006200020311     C                   parm                    KPJBA
006300020311      *
006400020311     C                   write     FI40T01
006500020311     C                   write     FI40Z01
006600020311      *
006700020311      * Pulizia dei campi videata
006800020311     C                   clear                   V1Cdip
006900020311     C                   clear                   V1Cdfp
007000020311      *
007100020311DO  1C                   do        *hival
007200020311      *
007300020311      * Emissione VIDEO1
007400020311     C                   exfmt     FI40D01
007500020311      *
007600020311      * Pulizia campo messaggio e indicatori di errore (*IN28, 41, 42)
007700020311     C                   clear                   V1Cmsg
007800020311     C                   setoff                                       28
007900020311     C                   setoff                                       4142
008000020311      *
008100020311      * F3=Fine lavoro
008200020311     C   KC              leave
008300020311      *
008400020311      * Controllo dati del formato D01
008500020311     C                   exsr      CtrD01
008600020311      * se è acceso il 28 ritorno ad emettere il formato video
008700020311     c   28              iter
008800020311      *
008900020311      * F6=Elaborazione
009000020311IF  2c                   IF        *INKF  = *ON
009100020311     C                   eval      I41dip = wDIP
009200020311     C                   eval      I41dfp = wDFP
009300020311     C                   eval      I41tca = V1Ctca
009400020311      *
009500020311     C                   movel(p)  FIDN41DS      KPJBU
009600020311IF  3C                   if        KNMUS = *all'1'
009700020313     C                   call      'FIDN41C'
009800020311     C                   parm                    KPJBA
009900020311X   3C                   else
010000020311     C                   movel     'DN41'        KCOAZ
010100020311     C***                movel     '3'           KRITB
010200020311     C***                call      'BCH09'
010300020311     C***                parm                    KPJBA
010400020311     C***                if        KRITB = '2'
010500020311     C***                iter
010600020311     C***                endif
010700020311     C                   call      'BCH10'
010800020311     C                   parm                    KPJBA
010900020311E   3C                   endif
011000020311     C                   leave
011100020311      *
011200020311X   2C                   ELSE
011300020311      *
011400020311     C                   iter
011500020311      *
011600020311E   2C                   ENDIF
011700020311      *
011800020311E   1C                   ENDDO
011900020311      *
012000020311     C                   eval      *INLR = *ON
012100020311
012200020311      *---------------------------------------------------------------*
012300020311      * CTRD01 * CONTROLLI FORMATO D01                                *
012400020311      *---------------------------------------------------------------*
012500020311     C     CTRD01        BEGSR
012600020311      *
012700020311     C                   setoff                                       28
012800020311      *
012900020311      *  MESE/ANNO DAL
013000020311IF  1C                   IF        V1Cdip > 0
013100020311     C                   clear                   WLBDAT
013200020311     C                   z-add     V1Cdip        G02dat
013300020313IF  2C                   if        V1Cdip > 9999
013400020313     C                   add       01000000      G02dat
013500020313X   2C                   else
013600020313     C                   add       00010000      G02dat
013700020313E   2C                   endif
013800020311     C                   call      'XSRDA8'
013900020311     C                   parm                    WLBDAT
014000020311IF  2C                   if        G02err = *on
014100020311     C                   movel     MSG(1)        V1Cmsg
014200020311     C                   seton                                        41  28
014300020311     C                   goto      ENDCTRD01
014400020311E   2C                   endif
014500020311     C                   z-add     G02dat        V1Cdip
014600020311     C                   eval      wDIP = G02inv / 100
014700020311     C                   ELSE
014800020311     C                   reset                   wDIP
014900020311     C                   seton                                        41  28
015000020311     C                   movel     MSG(1)        V1Cmsg
015100020311     C                   goto      ENDCTRD01
015200020311E   1C                   ENDIF
015300020311      *
015400020311      *  MESE/ANNO DAL PRECEDENTE IL 03/1999
015500020311IF  1C                   if        wDIP < 199903
015600020311     C                   movel     MSG(3)        V1Cmsg
015700020311     C                   seton                                        41  28
015800020311     C                   goto      ENDCTRD01
015900020311E   1C                   endif
016000020311      *
016100020311      *  MESE/ANNO  AL
016200020311IF  1C                   IF        V1Cdfp > 0
016300020311     C                   clear                   WLBDAT
016400020311     C                   z-add     V1Cdfp        G02dat
016500020313IF  2C                   if        V1Cdfp > 9999
016600020313     C                   add       01000000      G02dat
016700020313X   2C                   else
016800020313     C                   add       00010000      G02dat
016900020313E   2C                   endif
017000020311     C                   call      'XSRDA8'
017100020311     C                   parm                    WLBDAT
017200020311IF  2C                   if        G02err = *on
017300020311     C                   movel     MSG(1)        V1Cmsg
017400020311     C                   seton                                        42  28
017500020311     C                   goto      ENDCTRD01
017600020311E   2C                   endif
017700020311     C                   z-add     G02dat        V1Cdfp
017800020311     C                   eval      wDFP = G02inv / 100
017900020311     C                   ELSE
018000020311     C                   z-add     V1Cdip        V1Cdfp
018100020311     C                   z-add     wDIP          wDFP
018200020311E   1C                   ENDIF
018300020311      *
018400020311      *  DATA DAL MAGGIORE DI DATA AL
018500020311IF  1C                   if        wDIP > wDFP
018600020311     C                   movel     MSG(2)        V1Cmsg
018700020311     C                   seton                                        414228
018800020311     C                   goto      ENDCTRD01
018900020311E   1C                   endif
019000020311      *
019100020311      *  PERIODO DA ELABORARE SUPERIORE AI 12 MESI
019200021023IF  1C*                  if        wDFP - wDIP > 12
019300021023     C*                  z-add     V1Cdip        wDATA0
019400021023     C*                  movel     '01'          wDATA0
019500021023     C*                  move      wDATA0        wDATA1
019600021023     C*                  z-add     V1Cdfp        wDATA0
019700021023     C*                  movel     '01'          wDATA0
019800021023     C*                  move      wDATA0        wDATA2
019900021023     C*                  adddur    1:*m          wDATA2
020000021023     C*    wDATA2        subdur    wDATA1        wDATA0:*m
020100021023IF  2C*                  if        wDATA0 > 12
020200021023     C*                  movel     MSG(4)        V1Cmsg
020300021023     C*                  seton                                        414228
020400021023     C*                  goto      ENDCTRD01
020500021023E   2C*                  endif
020600021023E   1C*                  endif
020700020311      *
020800020311     C     ENDCTRD01     ENDSR
020900020311
021000020311      *---------------------------------------------------------------*
021100020311      * *INZSR * Operazioni Iniziali                                  *
021200020311      *---------------------------------------------------------------*
021300020311     C     *INZSR        BEGSR
021400020311      *
021500020311     C                   Z-ADD     1             CODUT
021600020311     C                   CALL      'X§PARUT'
021700020311     C                   PARM                    UT§DSE0F
021800020311     C                   MOVEL     RAGUT         VTCrsu
021900020311      *
022000020311      * Imposto il nome del programma
022100020311     C                   MOVEL     DSPGM         VTCpgm
022200020311      *
022300020311     C                   ENDSR
022400020311
022500020311      *---------------------------------------------------------------*
022600020311
022700020311**  MSG
022800020311Mese/Anno errati                                                              01
022900020313Il Mese/Anno iniziale non puo' essere maggiore del Mese/Anno finale           02
023000020311Il periodo iniziale non puo' essere precedente al 03/1999                     03
023100020311Il periodo da elaborare non puo' superare i 12 mesi                           04
