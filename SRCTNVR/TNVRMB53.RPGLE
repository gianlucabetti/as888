000100090127     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200090127      // ----------------------------------------------------------------------
000300090127      //
000400090302      //         Modifica dati tabella LAC  ?
000500090127      //
000600090127      // ----------------------------------------------------------------------
000700090302     ftntbe01l  uf   e           k disk
000800130115     fcnaco00f  if   e           k disk
000900090127
001000090302     d ktbecod         s                   like(tbecod) inz('LAC')
001100090302     d savuni          s                   like(tbeuni)
001200090302     d $fine           s               n
001300090302
001400090302     d dlac          e ds
001500090127
001600090127      // ----------------------------------------------------------------------
001700090127
001800090127      /free
001900090128
002000090302         $fine = *off;
002100090302
002200090302         setll ktbecod tntbe01l;
002300090302
002400090302         dow not $fine;
002500090302           reade ktbecod tntbe01l;
002600090302           if %eof(tntbe01l);
002700090302             $fine = *on;
002800090302             leave;
002900090302           endif;
003000090302
003100130115           dlac = tbeuni;
003200130115
003300130115           acoksc = %dec(%subst(TBEke1:1:7):7:0);
003400130115           chain (1:151:acoksc) cnaco00f;
003500130115           if %found(cnaco00f);
003600130115             �lacrag = %subst(acorag:1:25);
003700130115           endif;
003800130115
003900090302           tbeuni = dlac;
004000090302           update tntbe000;
004100090302
004200090302         enddo;
004300090127
004400090127         *inlr = *on;
004500090128
004600090128      /end-free
