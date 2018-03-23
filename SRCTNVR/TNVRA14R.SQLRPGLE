000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090423     H* TNVRA14R *---------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700090423     C*=================================================================
000800090423     C     *ENTRY        PLIST
000900090423     C                   PARM                    AGGIORNA          1
001000090423     C*
001100090423     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
001200090423     C                   clear                   UDATE_8           8 0
001300090423     C                   MOVEL     *DATE         UDATE_8
001400090422     C*
001500081209     C                   SETON                                        LR
001600020418     C*
001700090422     C* Prima fa scadere tutte quelle tariffe che non hanno Autisti valorizzati
001800090422     C*  e non valorizzati nel 2009 mettendo la data scadenza tariffa "20090430"
001900090422     C*
002000090422     C*=================================================================
002100090422     C*
002200090422     C*   TARIFFE  DA  FAR  SCADERE  PRIMA   DELLA CONVERSIONE
002300090428     C* sia Reali che simulate poichè le Valorizzazioni guidano entrambe
002400090428     C*
002500090428     C*
002600090422     C*  Aggiorna la data SCADENZA al 20090430 di tutte le tariffe di quegli
002700090422     C*   AUT senza alcuna VALORIZZAZIONE in assoluto
002800090422     C*      ========
002900090422     C/EXEC SQL
003000090422     C+ update fifgt00f set fgtdst = 20090430
003100090422     C+ where fgtdst > 20090430
003200090422     C+ and (fgtddt < 20090401 or fgtddt > 20091231)
003300090422     C+ and fgttsr in ('C', 'R', 'G') and
003400090422     C+ fgtpdr NOT in (select fttpdr
003500090422     C+ from fiftt00f group by fttpdr )
003600090422     C/END-EXEC
003700090422     C*
003800090422     C*  Aggiorna la data SCADENZA al 20090430 di tutte le tariffe di quegli
003900090422     C*  AUT presenti sulle valorizzazioni ma che NON hanno Valorizzazioni nel 2009.
004000090422     C*      ========
004100090422     C/EXEC SQL
004200090422     C+ update fifgt00f set fgtdst = 20090430
004300090422     C+ where fgtdst > 20090430 and fgttsr in ('C', 'R', 'G')
004400090422     C+ and (fgtddt < 20090401 or fgtddt > 20091231)
004500090422     C+ and fgtpdr in (select fttpdr from fiftt00f
004600090422     C+ where fttddc <=20081231 and fttpdr NOT
004700090422     C+ in (select fttpdr from fiftt00f WHERE
004800090422     C+ fttddc > 20081231) group by fttpdr )
004900090422     C/END-EXEC
005000081110     C*
005100081110     C     FINE          TAG
005200081110     c                   return
005300081209     C*------------------------------------------------------------------------*
