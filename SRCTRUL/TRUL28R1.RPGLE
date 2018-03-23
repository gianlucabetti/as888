000100020219     H DECEDIT('0,') DATEDIT(*DMY.)
000200020219
000300020219      ****************************************************************
000400030404      ** TRUL28R ** Calcolo del CHECK DIGIT :                       **
000500030404      **            M10 - Segnacollo BARTOLINI                      **
000600020219      ****************************************************************
000700020219
000800020219      *
000900020219      * SCHIERE
001000020219      *
001100030404     D MSG             s             78    dim(04) ctdata perrcd(1)
001200020221      * Schiera per routine calcolo check digit
001300020221     D §SEG            s              1  0 dim(18) inz
001400020221
001500020219      *
001600020219      * DS definite ESTERNAMENTE
001700020219      *
001800020219     D TRUL28DS      e ds
001900020221
002000020219      *
002100020219      * DS definite INTERNAMENTE
002200020219      *
002300030618      * Segnacollo BARTOLINI
002400020221     D SSCBAR          DS            30    inz
002500020219     D  RADsegB                1      8s 0 inz
002600020219     D  PRGsegB                9     15s 0 inz
002700020221     D  SEGsegB                1     17s 0
002800020219     D  CKDsegB               18     18s 0 inz
002900020219      *
003000020219      * DS per routine calcolo check digit segnacollo Bartolini
003100030618     D                 ds
003200020221     D  §18A                   1     18
003300030618     D  §18N                   1     18  0 inz
003400020221     D                                     dim(18)
003500030618     D                 ds
003600020221     D  §2A                    1      2
003700030618     D  §2N                    1      2  0 inz
003800020219     D                                     dim(2)
003900020221
004000020219      *
004100020219      * VARIABILI
004200020219      *
004300020221     D $Err            s              1    inz(*off)
004400020219     D wCHKDGT         s              1  0 inz
004500020219     D wPOS            s              2  0 inz
004600030618     D wCHKDGTpos      s              2  0 inz
004700020219     D XX              s              2  0 inz
004800020219     D YY              s              2  0 inz(18)
004900020219     D §Max            s              3  0 inz(17)
005000020219     D §TT             s              3  0 inz
005100020219     D §TT2            s              3  0 inz
005200020219     D §CTR            s              3  0 inz
005300020219     D §RST            s              3  0 inz
005400020221
005500020219      *
005600020219      * COSTANTI
005700020219      *
005800020219     D Digits          c                   const('0123456789')
005900020219
006000020221      ****************************************************************
006100020221
006200020219      * Operazioni iniziali
006300020219     C                   exsr      OPEINI
006400020219      *
006500020219      * Controllo dati
006600020221     C                   exsr      CTRSTD
006700020219      *
006800020219      * Calcolo Check Digit
006900030404cas 1C     $ERR          caseq     *off          CHKDGT
007000030404e   1C                   endcs
007100020219      *
007200020221      * Fine
007300020219     C                   exsr      ENDPGM
007400020219
007500020219      ****************************************************************
007600020219      ** OPEINI ** Operazioni Iniziali                              **
007700020219      ****************************************************************
007800020219     C     OPEINI        BEGSR
007900020221      *
008000020221     C     *ENTRY        plist
008100030404     C                   parm                    TRUL28DS
008200020219      *
008300020219      * Inizializzazione campi di output
008400020219     C                   clear                   O28err
008500020219     C                   clear                   O28msg
008600020219     C                   clear                   O28ckd
008700020220     C                   clear                   O28cod
008800020221      *
008900020221     C                   reset                   $Err
009000020219      *
009100020219     C                   ENDSR
009200020219
009300020219      ****************************************************************
009400020221      ** CTRSTD ** Controlli standard                               **
009500020219      ****************************************************************
009600020221     C     CTRSTD        BEGSR
009700020219      *
009800020221     C                   eval      $Err = *off
009900030404     C                   clear                   §Max
010000030404      *
010100030404sel 1C                   select
010200030404      *
010300030404w   1C                   when          I28mod = *blanks
010400030404     C                             and I28cod = *blanks
010500030404     C                   movel     *on           $Err
010600020219      *
010700030618      * M10   - Segnacollo BARTOLINI: lunghezza variabile
010800030404w   1C                   when      I28mod = 'BAR'
010900030404     C                   exsr      CtrM10
011000020219      *
011100030404e   1C                   endsl
011200020221      *
011300020221if  1C                   if        O28err <> *blank
011400020221     C                   movel     *on           $Err
011500020221e   1C                   endif
011600020219      *
011700020221     C     ENDCTR        ENDSR
011800030618
011900030618      ****************************************************************
012000030618      ** CtrM10     ** Controlli standard su M10                    **
012100030618      **               (Segnacollo Bartolini)                       **
012200030618      ****************************************************************
012300030618     C     CtrM10        BEGSR
012400030618      *
012500030618      * Imposto lunghezza del modulo:
012600030618      *  Devono essere min 1 max 17 cifre (la 18a è il check digit)
012700030618     C     Digits        check     I28cod        wPOS
012800030618sel 1C                   SELECT
012900030618w   1C                   WHEN      wPOS =  1
013000030618     C                   movel     '2'           O28err
013100030618     C                   movel     MSG(2)        O28msg
013200030618     C                   goto      EndM10
013300030618w   1C                   WHEN      wPOS =  0
013400030618     C                   eval      §MAX = %len(I28cod)
013500030618x   1C                   OTHER
013600030618     C                   eval      §MAX = wPOS - 1
013700030618e   1C                   ENDSL
013800030618      *
013900030618     C                   clear                   CKDsegB
014000030618     C                   eval      wCHKDGTpos = §MAX + 1
014100030618      *
014200030618     C     EndM10        ENDSR
014300020219
014400020219      ****************************************************************
014500020221      ** CHKDGT ** Calcolo CHECK DIGIT                              **
014600020219      ****************************************************************
014700020221     C     CHKDGT        BEGSR
014800020219      *
014900020219     C                   clear                   §SEG
015000020221     C                   clear                   §18N
015100020219      *
015200030618     C                   eval      %subst(§18A   : 1 : §Max) =
015300030618     C                             %subst(I28cod : 1 : §Max)
015400020221     C                   z-add     §18N          §SEG
015500020219     C                   exsr      M10
015600020221      *
015700020221if  1C                   if        O28err = *blank
015800020221     C                   movel     wChkDgt       O28ckd
015900020221     C                   z-add     §SEG          §18N
016000030618     C                   eval      O28cod = %subst(§18A:1:wCHKDGTpos)
016100020221e   1C                   endif
016200020219      *
016300020219     C                   ENDSR
016400020219
016500020219      ****************************************************************
016600020219      ** M10    ** Calcolo cifra di controllo secondo modulo 10 IBM **
016700020219      ****************************************************************
016800020219     C     M10           BEGSR
016900020219      *
017000020219     C                   clear                   §TT
017100020219     C                   clear                   §TT2
017200020219     C                   clear                   §CTR
017300020219     C                   clear                   §RST
017400020219     C                   clear                   wCHKDGT
017500030618     C                   eval      YY   = wCHKDGTpos
017600020219      *
017700020219do  1C                   do        §MAX          XX
017800020219      *
017900020219     C                   sub       1             YY
018000020219     C     XX            div       2             §CTR
018100020219     C                   mvr                     §RST
018200020219      *
018300020219if  2C                   if        §RST > 0
018400020219      *
018500020219     C                   eval      §CTR = §SEG(YY) * 2
018600020221     C                   move      §CTR          §2A
018700020219     C                   xfoot     §2N           §CTR
018800020219      *
018900020219x   2C                   else
019000020219      *
019100020219     C                   eval      §CTR = §SEG(YY)
019200020219      *
019300020219e   2C                   endif
019400020219      *
019500020219     C                   add       §CTR          §TT
019600020219      *
019700020219e   1C                   enddo
019800020219      *
019900020219     C                   eval      §TT2    = §TT  + 9
020000020219     C                   move      0             §TT2
020100020219     C                   eval      wCHKDGT = §TT2 - §TT
020200020219      *
020300030618     C                   eval      §SEG(wCHKDGTpos) = wCHKDGT
020400020219      *
020500020219     C                   ENDSR
020600020219
020700020219      ****************************************************************
020800020219      ** ENDPGM ** Fine Programma
020900020219      ****************************************************************
021000020219     C     ENDPGM        BEGSR
021100020219      *
021200020219if  1C                   if        I28tla = *blank
021300020219     C                   movel     *on           *inrt
021400020219x   1C                   else
021500020219     C                   movel     *on           *inlr
021600020219e   1C                   endif
021700020219     C                   return
021800020219      *
021900020219     C                   ENDSR
022000020219
022100020219** MSG
022200020219TRUL28R - Codice errato                                                        01
022300020221TRUL28R - Codice errato: contiene caratteri non ammessi (o blank)              02
022400020221TRUL28R - Codice errato: contiene caratteri oltre la lunghezza massima         03
022500020221TRUL28R - Caratteri iniziali del barcode errati                                04
