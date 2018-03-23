000100020219     H DECEDIT('0,') DATEDIT(*DMY.)
000200020219
000300020219      ****************************************************************
000400030404      ** TRUL28R ** Calcolo del CHECK DIGIT :                       **
000500030404      **            M1031 - Segnacollo DPD                          **
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
002300020221      * Radice + Nr.Progressivo del Segnacollo DPD ("Chi Sono" BARTOLINI)
002400020221     D SSCchiB         ds            30    inz
002500020219     D  RADchiB                1      2    inz
002600020219     D  PRGchiB                3     13s 0 inz
002700020219     D  CKDchiB               14     14s 0 inz
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
009400020219      *
009500030404sel 1C                   select
009600030404      *
009700030404w   1C                   when          I28mod = *blanks
009800030404     C                             and I28cod = *blanks
009900030404     C                   eval      $Err = *on
010000020221      *
010100020221      * M1031 - Segnacollo DPD ("Chi Sono" BARTOLINI): AA/CC + 12 NUMERI
010200030404w   1C                   when      I28mod = 'DPD'
010300030404     C                   exsr      CtrM1031
010400030404      *
010500030404e   1C                   endsl
010600020221      *
010700020221if  1C                   if        O28err <> *blank
010800020221     C                   movel     *on           $Err
010900020221e   1C                   endif
011000020219      *
011100020221     C     ENDCTR        ENDSR
011200020221
011300020221      ****************************************************************
011400020221      ** CtrM1031   ** Controlli standard su M1031                  **
011500020221      **               (Segnacollo DPD - "Chi Sono" Bartolini)      **
011600020221      ****************************************************************
011700020221     C     CtrM1031      BEGSR
011800020221      *
011900020408      * Devono essere 11 cifre (la 12a è il check digit)
012000020408      * NON sono ammessi i codici iniziali "AA" o "CC".
012100020408     C     Digits        check     I28cod        wPOS
012200020408if  2C                   if        wPOS <  11
012300020221     C                   movel     '2'           O28err
012400020221     C                   movel     MSG(2)        O28msg
012500020221     C                   goto      EndM103
012600020221e   2C                   endif
012700020408      * dalla 12 pos. fino alla fine devono essere tutti *BLANKS
012800020408     C     ' '           check     I28cod:12     wPOS
012900020221if  2C                   if        wPOS > *zeros
013000020221     C                   movel     '3'           O28err
013100020221     C                   movel     MSG(3)        O28msg
013200020221     C                   goto      EndM103
013300020221e   2C                   endif
013400020408      *
013500020408     C                   clear                   SSCchiB
013600020408     C                   movel     I28cod        PRGchiB
013700020408     C                   clear                   CKDchiB
013800020221      *
013900020221      * imposto lunghezza del modulo
014000020221     C                   z-add     11            §MAX
014100020221      *
014200020221     C     EndM103       ENDSR
014300020219
014400020219      ****************************************************************
014500020221      ** CHKDGT ** Calcolo CHECK DIGIT                              **
014600020219      ****************************************************************
014700020221     C     CHKDGT        BEGSR
014800020219      *
014900020219      * controllo il check digit
015000020219     C                   clear                   §SEG
015100020221     C                   clear                   §18N
015200020219      *
015300020221     C                   movel     PRGchiB       §18A
015400020221     C                   z-add     §18N          §SEG
015500020219     C                   exsr      M1031
015600020221      *
015700020221if  1C                   if        O28err = *blank
015800020221     C                   movel     wChkDgt       O28ckd
015900020221     C                   z-add     §SEG          §18N
016000020221     C                   eval      O28cod = %subst(§18A:1:§MAX+1)
016100020221e   1C                   endif
016200020219      *
016300020219     C                   ENDSR
016400020219
016500020219      ****************************************************************
016600020221      ** M1031  ** Calcolo cifra di controllo secondo modulo M1031  **
016700020219      ****************************************************************
016800020221     C     M1031         BEGSR
016900020219      *
017000020219     C                   clear                   §TT
017100020219     C                   clear                   §TT2
017200020219     C                   clear                   §CTR
017300020219     C                   clear                   §RST
017400020219     C                   clear                   wCHKDGT
017500020219      *
017600020219      * Totalizzo i valori, quelli nelle posizioni dispari per 3
017700020219do  1C                   do        §MAX          XX
017800020219      *
017900020219     C     xx            div       2             §CTR
018000020219     C                   mvr                     §RST
018100020219if  2C                   if        §RST > 0
018200020219     C                   eval      §CTR = §SEG(xx) * 3
018300020219x   2C                   else
018400020219     C                   eval      §CTR = §SEG(xx)
018500020219e   2C                   endif
018600020219      *
018700020219     C                   add       §CTR          §TT
018800020219      *
018900020219e   1C                   enddo
019000020219      *
019100020219     C                   eval      §TT2    = §TT  + 9
019200020219     C                   move      0             §TT2
019300020219     C                   eval      wCHKDGT = §TT2 - §TT
019400020219      *
019500020305     C                   movel     wCHKDGT       §SEG(12)
019600020219      *
019700020219     C                   ENDSR
019800020221
019900020219      ****************************************************************
020000020219      ** ENDPGM ** Fine Programma
020100020219      ****************************************************************
020200020219     C     ENDPGM        BEGSR
020300020219      *
020400020219if  1C                   if        I28tla = *blank
020500020219     C                   movel     *on           *inrt
020600020219x   1C                   else
020700020219     C                   movel     *on           *inlr
020800020219e   1C                   endif
020900020219     C                   return
021000020219      *
021100020219     C                   ENDSR
021200020219
021300020219** MSG
021400020219TRUL28R - Codice errato                                                        01
021500020221TRUL28R - Codice errato: contiene caratteri non ammessi (o blank)              02
021600020221TRUL28R - Codice errato: contiene caratteri oltre la lunghezza massima         03
021700020221TRUL28R - Caratteri iniziali del barcode errati                                04
