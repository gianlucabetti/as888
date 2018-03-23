000100030404     H DECEDIT('0,') DATEDIT(*DMY.)
000200030404
000300030404      ****************************************************************
000400030404      ** TRUL28R ** Calcolo del CHECK DIGIT :                       **
000500030404      **            M???? - Segnacollo FEDEX MOD 7                  **
000600030404      ****************************************************************
000700030404
000800030404      *
000900030404      * SCHIERE
001000030404      *
001100030404     D MSG             s             78    dim(05) ctdata perrcd(1)
001200030404      * Schiera per routine calcolo check digit
001300030404     D §SEG            s              1  0 dim(18) inz
001400030404
001500030404      *
001600030404      * DS definite ESTERNAMENTE
001700030404      *
001800030404     D TRUL28DS      e ds
001900030404
002000030404      *
002100030404      * DS definite INTERNAMENTE
002200030404      *
002300030404      * Nr.Progressivo del Segnacollo FEDEX per test
002400030404     D SSCFED          ds            11    inz
002500030404     D  PRGfedA                1      2s 0 inz
002600030404     D  PRGfedZ                1     11s 0 inz
002700030404      *
002800030404      * Nr.Progressivo del Segnacollo FEDEX lungo 10
002900030404     D SSCFED10        ds            30    inz
003000030404     D  PRGfed0                1     10s 0 inz
003100030404     D  PRGfedX               11     11s 0 inz
003200030404     D  CKDfed0               12     12s 0 inz
003300030404      *
003400030404      * Nr.Progressivo del Segnacollo FEDEX lungo 11
003500030404     D SSCFED11        ds            30    inz
003600030404     D  PRGfedY                1      4s 0 inz
003700030404     D  PRGfed1                5     11s 0 inz
003800030404     D  CKDfed1               12     12s 0 inz
003900030404      *
004000030404      * DS per routine calcolo check digit segnacollo FEDEX Mod 7
004100030404     D                 ds
004200030404     D  §18A                   1     18
004300030404     D  §18N                   1     18  0
004400030404     D                                     dim(18)
004500030404
004600030404      *
004700030404      * VARIABILI
004800030404      *
004900030404     D $Err            s              1    inz(*off)
005000030404     D $Mod7x11        s              1    inz(*off)
005100030404     D wPRG            s             11  0 inz
005200030404     D wCHKDGT         s              1  0 inz
005300030404     D wPOS            s              2  0 inz
005400030404     D §Max            s              3  0 inz
005500030404     D §CKDpos         s              3  0 inz
005600030404     D §CTR            s              3  0 inz
005700030404     D §RST            s              3  0 inz
005800030404
005900030404      *
006000030404      * COSTANTI
006100030404      *
006200030404     D Digits          c                   const('0123456789')
006300030404
006400030404      ****************************************************************
006500030404
006600030404      * Operazioni iniziali
006700030404     C                   exsr      OPEINI
006800030404      *
006900030404      * Controllo dati
007000030404     C                   exsr      CTRSTD
007100030404      *
007200030404      * Calcolo Check Digit
007300030404cas 1C     $ERR          caseq     *off          CHKDGT
007400030404e   1C                   endcs
007500030404      *
007600030404      * Fine
007700030404     C                   exsr      ENDPGM
007800030404
007900030404      ****************************************************************
008000030404      ** OPEINI ** Operazioni Iniziali                              **
008100030404      ****************************************************************
008200030404     C     OPEINI        BEGSR
008300030404      *
008400030404     C     *ENTRY        plist
008500030404     C                   parm                    TRUL28DS
008600030404      *
008700030404      * Inizializzazione campi di output
008800030404     C                   clear                   O28err
008900030404     C                   clear                   O28msg
009000030404     C                   clear                   O28ckd
009100030404     C                   clear                   O28cod
009200030404      *
009300030404     C                   reset                   $Err
009400030404      *
009500030404     C                   ENDSR
009600030404
009700030404      ****************************************************************
009800030404      ** CTRSTD ** Controlli standard                               **
009900030404      ****************************************************************
010000030404     C     CTRSTD        BEGSR
010100030404      *
010200030404     C                   eval      $Err = *off
010300030404     C                   clear                   §Max
010400030404     C                   clear                   §CKDpos
010500030404     C                   reset                   $Mod7x11
010600030404      *
010700030404sel 1C                   select
010800030404      *
010900030404w   1C                   when          I28mod = *blanks
011000030404     C                             and I28cod = *blanks
011100030404     C                   eval      $Err = *on
011200030404      *
011300030404      * FedEx - Segnacollo FEDEX: 10 o 11 NUMERI
011400030404w   1C                   when      I28mod = 'FE7'
011500030404     C                   exsr      CtrMFedEx7
011600030404      *
011700030404e   1C                   endsl
011800030404      *
011900030404if  1C                   if        O28err <> *blank
012000030404     C                   movel     *on           $Err
012100030404e   1C                   endif
012200030404      *
012300030404     C     ENDCTR        ENDSR
012400030404
012500030404      ****************************************************************
012600030404      ** CtrMFedEx7 ** Controlli standard su M????                  **
012700030404      **               (Segnacollo FEDEX Mod 7)                     **
012800030404      ****************************************************************
012900030404     C     CtrMFedEx7    BEGSR
013000030404      *
013100030404      * imposto lunghezza del modulo e posizione del check digit
013200030404     C                   z-add     11            §MAX
013300030404     C                   z-add     12            §CKDpos
013400030404      *
013500030404      * Devono essere 11 cifre (la 12a è il check digit)
013600030404     C     Digits        check     I28cod        wPOS
013700030404if  1C                   if        wPOS < §CKDpos
013800030404     C                   movel     '2'           O28err
013900030404     C                   movel     MSG(2)        O28msg
014000030404     C                   goto      EndMFEDEX
014100030404e   1C                   endif
014200030404      * dalla 13 pos. fino alla fine devono essere tutti *BLANKS
014300030404     C     ' '           check     I28cod:§CKDposWpos
014400030404if  1C                   if        wPOS > *zeros
014500030404     C                   movel     '3'           O28err
014600030404     C                   movel     MSG(3)        O28msg
014700030404     C                   goto      EndMFEDEX
014800030404e   1C                   endif
014900030404      *
015000030404     C                   movel     I28cod        SSCFED
015100030404      *
015200030404      * Controllo la lunghezza effettiva (da cui dipende il controllo
015300030404      *  del Tracking Number)
015400030404if  1C                   if        PRGfedA  = *zeros
015500030404     C                   eval      $Mod7x11 = *off
015600030404     C                   movel     I28cod        SSCFED10
015700030404e   xC                   else
015800030404     C                   eval      $Mod7x11 = *on
015900030404     C                   movel     I28cod        SSCFED11
016000030404e   1C                   endif
016100030404      *
016200030404     C     EndMFEDEX     ENDSR
016300030404
016400030404      ****************************************************************
016500030404      ** CHKDGT ** Calcolo CHECK DIGIT                              **
016600030404      ****************************************************************
016700030404     C     CHKDGT        BEGSR
016800030404      *
016900030404      * controllo il check digit
017000030404     C                   clear                   §SEG
017100030404     C                   clear                   §18N
017200030404     C                   movel     SSCfed        §18A
017300030404     C                   z-add     §18N          §SEG
017400030404      *
017500030404if  1C                   if        $Mod7x11 = *off
017600030404     C                   z-add     PRGfed0       wPRG
017700030404x   1C                   else
017800030404     C                   z-add     PRGfed1       wPRG
017900030404e   1C                   endif
018000030404     C                   exsr      MFedEx7
018100030404      *
018200030404if  1C                   if        O28err = *blank
018300030404     C                   movel     wChkDgt       O28ckd
018400030404     C                   movea     §SEG          §18N
018500030404     C                   eval      O28cod = %subst(§18A:1:§CKDpos)
018600030404e   1C                   endif
018700030404      *
018800030404     C                   ENDSR
018900030404
019000030404      ****************************************************************
019100030404      ** MFedEx7 ** Calcolo cifra di controllo secondo modulo M???? **
019200030404      ****************************************************************
019300030404     C     MFedEx7       BEGSR
019400030404      *
019500030404     C                   clear                   §CTR
019600030404     C                   clear                   §RST
019700030404     C                   clear                   wCHKDGT
019800030404      *
019900030404      * Il check digit del codice a barre di FedEx va calcolato
020000030404      * da "base" diversa a seconda del codice a barre:
020100030404      * - partendo dal primo byte se i primi 2 byte sono "00",
020200030404      * - considerando gli ultimi 7 se i primi 2 sono > "00".
020300030404      * Tale codice va diviso per 7 e il resto è i check digit.
020400030404      * Esempio 1:
020500030404      * codice a barre ................:  0  0  1  2  3  4  5  6  7  8  9
020600030404      * check digit ...................:  12.345.678 / 7 = 1.763.668, resto: 2
020700030404      * Esempio 2:
020800030404      * codice a barre ................:  0  1  2  3  4  5  6  7  8  9  0
020900030404      * check digit ...................:  4.567.890 / 7 = 652.555, resto: 5
021000030404      *
021100030404      *
021200030404     C     wPRG          div       7             §CTR
021300030404     C                   mvr                     §RST
021400030404      *
021500030404     C                   move      §RST          wCHKDGT
021600030404      *
021700030404     C                   movel     wCHKDGT       §SEG(§CKDpos)
021800030404      *
021900030404     C                   ENDSR
022000030404
022100030404      ****************************************************************
022200030404      ** ENDPGM ** Fine Programma
022300030404      ****************************************************************
022400030404     C     ENDPGM        BEGSR
022500030404      *
022600030404if  1C                   if        I28tla = *blank
022700030404     C                   movel     *on           *inrt
022800030404x   1C                   else
022900030404     C                   movel     *on           *inlr
023000030404e   1C                   endif
023100030404     C                   return
023200030404      *
023300030404     C                   ENDSR
023400030404
023500030404** MSG
023600030404TRUL28R - Codice errato                                                        01
023700030404TRUL28R - Codice errato: contiene caratteri non ammessi (o blank)              02
023800030404TRUL28R - Codice errato: contiene caratteri oltre la lunghezza massima         03
023900030404TRUL28R - Caratteri iniziali del barcode errati                                04
