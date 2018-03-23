000100020219     H DECEDIT('0,') DATEDIT(*DMY.)
000200020219
000300020219      ****************************************************************
000400030404      ** TRUL28R ** Calcolo del CHECK DIGIT :                       **
000500030404      **            M???? - Segnacollo FEDEX MOD 11                 **
000600020219      ****************************************************************
000700020219
000800020219      *
000900020219      * SCHIERE
001000020219      *
001100020221     D $FED            s              1  0 dim(03) ctdata perrcd(3)
001200020219     D MSG             s             78    dim(05) ctdata perrcd(1)
001300020221      * Schiera per routine calcolo check digit
001400020221     D §SEG            s              1  0 dim(18) inz
001500020221
001600020219      *
001700020219      * DS definite ESTERNAMENTE
001800020219      *
001900020219     D TRUL28DS      e ds
002000020221
002100020219      *
002200020219      * DS definite INTERNAMENTE
002300020221      *
002400020221      * Nr.Progressivo del Segnacollo FEDEX
002500020221     D SSCFED          ds            30    inz
002600020221     D  PRGfedB                1     11s 0 inz
002700020221     D  CKDfedB               12     12s 0 inz
002800030404      *
002900030404      * DS per routine calcolo check digit
003000030404     D                 ds
003100030404     D  §18A                   1     18
003200030404     D  §18N                   1     18  0
003300030404     D                                     dim(18)
003400020221
003500020219      *
003600020219      * VARIABILI
003700020219      *
003800020221     D $Err            s              1    inz(*off)
003900020219     D wCHKDGT         s              1  0 inz
004000020219     D wPOS            s              2  0 inz
004100020219     D XX              s              2  0 inz
004200020219     D YY              s              2  0 inz(18)
004300020219     D §Max            s              3  0 inz(17)
004400020219     D §TT             s              3  0 inz
004500020219     D §TT2            s              3  0 inz
004600020219     D §CTR            s              3  0 inz
004700020219     D §RST            s              3  0 inz
004800020221
004900020219      *
005000020219      * COSTANTI
005100020219      *
005200020219     D Digits          c                   const('0123456789')
005300020219
005400020221      ****************************************************************
005500020221
005600020219      * Operazioni iniziali
005700020219     C                   exsr      OPEINI
005800020219      *
005900020219      * Controllo dati
006000020221     C                   exsr      CTRSTD
006100020219      *
006200020219      * Calcolo Check Digit
006300030404cas 1C     $ERR          caseq     *off          CHKDGT
006400030404e   1C                   endcs
006500020219      *
006600020221      * Fine
006700020219     C                   exsr      ENDPGM
006800020219
006900020219      ****************************************************************
007000020219      ** OPEINI ** Operazioni Iniziali                              **
007100020219      ****************************************************************
007200020219     C     OPEINI        BEGSR
007300020221      *
007400020221     C     *ENTRY        plist
007500030404     C                   parm                    TRUL28DS
007600020219      *
007700020219      * Inizializzazione campi di output
007800020219     C                   clear                   O28err
007900020219     C                   clear                   O28msg
008000020219     C                   clear                   O28ckd
008100020220     C                   clear                   O28cod
008200020221      *
008300020221     C                   reset                   $Err
008400020219      *
008500020219     C                   ENDSR
008600020219
008700020219      ****************************************************************
008800020221      ** CTRSTD ** Controlli standard                               **
008900020219      ****************************************************************
009000020221     C     CTRSTD        BEGSR
009100020219      *
009200020221     C                   eval      $Err = *off
009300020221     C                   clear                   §Max
009400030404      *
009500030404sel 1C                   select
009600030404      *
009700030404w   1C                   when          I28mod = *blanks
009800030404     C                             and I28cod = *blanks
009900030404     C                   movel     *on           $Err
010000030404      *
010100020221      * M???? - Segnacollo FEDEX: 12 NUMERI
010200030404w   1C                   when      I28mod = 'FED'
010300030404     C                   exsr      CtrMFedEx
010400020219      *
010500030404e   1C                   endsl
010600020221      *
010700020221if  1C                   if        O28err <> *blank
010800020221     C                   movel     *on           $Err
010900020221e   1C                   endif
011000020219      *
011100020221     C     ENDCTR        ENDSR
011200020221
011300020221      ****************************************************************
011400020221      ** CtrMFEDEX  ** Controlli standard su M????                  **
011500020221      **               (Segnacollo FEDEX)                           **
011600020221      ****************************************************************
011700020221     C     CtrMFEDEX     BEGSR
011800020221      *
011900020408      * Devono essere 11 cifre (la 12a è il check digit)
012000020221     C     Digits        check     I28cod        wPOS
012100020408if  2C                   if        wPOS <  12
012200020221     C                   movel     '2'           O28err
012300020221     C                   movel     MSG(2)        O28msg
012400020221     C                   goto      EndMFEDEX
012500020221e   2C                   endif
012600020221      * dalla 13 pos. fino alla fine devono essere tutti *BLANKS
012700020408     C     ' '           check     I28cod:12     wPOS
012800020221if  2C                   if        wPOS > *zeros
012900020221     C                   movel     '3'           O28err
013000020221     C                   movel     MSG(3)        O28msg
013100020221     C                   goto      EndMFEDEX
013200020221e   2C                   endif
013300020408      *
013400020408     C                   movel     I28cod        SSCFED
013500020221      *
013600020221      * imposto lunghezza del modulo
013700020221     C                   z-add     11            §MAX
013800020221      *
013900020221     C     EndMFEDEX     ENDSR
014000020219
014100020219      ****************************************************************
014200020221      ** CHKDGT ** Calcolo CHECK DIGIT                              **
014300020219      ****************************************************************
014400020221     C     CHKDGT        BEGSR
014500020219      *
014600020219      * controllo il check digit
014700020219     C                   clear                   §SEG
014800020221     C                   clear                   §18N
014900020221      *
015000020221     C                   movel     PRGfedB       §18A
015100020221     C                   z-add     §18N          §SEG
015200020221     C                   exsr      MFEDEX
015300020221      *
015400020221if  1C                   if        O28err = *blank
015500020221     C                   movel     wChkDgt       O28ckd
015600020221     C                   z-add     §SEG          §18N
015700020221     C                   eval      O28cod = %subst(§18A:1:§MAX+1)
015800020221e   1C                   endif
015900020219      *
016000020219     C                   ENDSR
016100020221
016200020221      ****************************************************************
016300020221      ** MFEDEX ** Calcolo cifra di controllo secondo modulo M????  **
016400020221      ****************************************************************
016500020221     C     MFEDEX        BEGSR
016600020221      *
016700020221     C                   clear                   §TT
016800020221     C                   clear                   §CTR
016900020221     C                   clear                   §RST
017000020221     C                   clear                   wCHKDGT
017100020221     C                   clear                   YY
017200020221      *
017300030404      * Il check digit del codice a barre di FedEx va calcolato molti-
017400030404      * plicando il numero del codice a barre (partendo dall'undicesimo
017500030404      * e andando a ritroso) per un valore alternato (1 - 3 - 7), e
017600030404      * moltiplicati tra loro.
017700020221      * Il totale va diviso per 11 e il resto è i check digit;
017800020221      * se il resto è 10 o 0 il check digit deve essere 0.
017900020221      * Esempio:
018000020221      * codice a barre ................:  0  1  2  3  4  5  6  7  8  9  8
018100020221      * valore alternato corrispondente:  3  1  7  3  1  7  3  1  7  3  1
018200030404      * totale (somma delle moltiplic.):  0  1 14  9  4 35 18  7 56 27  8 = 179
018300020221      * check digit ...................:  179 / 11 = 16, resto: 3
018400020221      *
018500020221      * parto dall'undicesima cifra
018600020221     C                   eval      XX  = §Max
018700020221     C                   dow       XX  > 0
018800020221      *
018900020221     C                   eval      YY  = YY  + 1
019000020221     C                   eval      §TT = §TT + (§SEG(xx) * $FED(yy))
019100020221      *
019200020221     C                   if        YY  = 3
019300020221     C                   eval      YY  = 0
019400020221     C                   endif
019500020221      *
019600020221     C                   eval      XX  = XX  - 1
019700020221      *
019800020221     C                   enddo
019900020221      *
020000020221     C     §TT           div       11            §CTR
020100020221     C                   mvr                     §RST
020200020221      *
020300020221     C                   move      §RST          wCHKDGT
020400020221      *
020500020221     C                   movel     wCHKDGT       §SEG(12)
020600020221      *
020700020221     C                   ENDSR
020800020219
020900020219      ****************************************************************
021000020219      ** ENDPGM ** Fine Programma
021100020219      ****************************************************************
021200020219     C     ENDPGM        BEGSR
021300020219      *
021400020219if  1C                   if        I28tla = *blank
021500020219     C                   movel     *on           *inrt
021600020219x   1C                   else
021700020219     C                   movel     *on           *inlr
021800020219e   1C                   endif
021900020219     C                   return
022000020219      *
022100020219     C                   ENDSR
022200020219
022300020221** $FED
022400020221137
022500020219** MSG
022600020219TRUL28R - Codice errato                                                        01
022700020221TRUL28R - Codice errato: contiene caratteri non ammessi (o blank)              02
022800020221TRUL28R - Codice errato: contiene caratteri oltre la lunghezza massima         03
022900020221TRUL28R - Caratteri iniziali del barcode errati                                04
