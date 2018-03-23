000100000420      /COPY QSYSINC/QRPGLESRC,QUSEC
000200000420
000300000420     D QuiLngTx01      S            256                                         Text string
000400000420     D QuiLngTx02      S              9B 0                                      Length text string
000500000420     D QuiLngTx03      S              7                                         Message ID
000600000420     D QuiLngTx04      S             20                                         Qualified MSGF
000700000420
000800030307     ** Testo.
000900030320     C                   EVAL      QuiLngTx01 = 'The Display Long Text -
001000030320     C                             (QUILNGTX) API displays a pop-up window -
001100030320     C                             containing the string of text that is passed-
001200030320     C                              it.'
001300030307
001400030307     ** Lunghezza del campo che contiene il testo.
001500000420     C                   EVAL      QuiLngTx02 = %SIZE(QuiLngTx01)
001600030307
001700030307     ** MSGID che contiene il titolo della finestra (facoltativo).
001800000420     C                   EVAL      QuiLngTx03 = 'BAS0009'
001900000420     C                   EVAL      QuiLngTx04 = 'QBASMSG   *LIBL'
002000030307
002100030307     ** Impostazione del parametro del messaggio di errore.
002200000420     C                   CLEAR                   Qusec
002300000420     C                   EVAL      Qusbprv = %SIZE(Qusec)
002400030307
002500000420     C                   CALL      'QUILNGTX'
002600000420     C                   PARM                    QuiLngTx01
002700000420     C                   PARM                    QuiLngTx02
002800000420     C                   PARM                    QuiLngTx03
002900000420     C                   PARM                    QuiLngTx04
003000000420     C                   PARM                    Qusec
003100030307
003200030307     ** A questo punto,
003300030307     **  se il campo QUSEI è vuoto, esecuzione corretta.
003400030307     **  se il campo QUSEI è pieno, contiene il MSGID di errore.
003500000420     C                   IF        Qusei <> *BLANK
003600030307     C*                  EXSR      Gestione errore
003700000420     C                   ENDIF
003800030307
003900000420     C                   EVAL      *INLR = *ON
