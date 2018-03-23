000100020920     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200020920
000300020920      * -------------------------------------------------------------*
000400020920      * Crea file note in GRU
000500020920      * -------------------------------------------------------------*
000600020920
000700020920     fExTfntc00fif   e             Disk    Rename(Tfntc:TfntcEx) prefix(Old_)
000800020920     fTncpo01l  if   e           k Disk
000900020920     fTfntc05l  if   e           k Disk    Rename(Tfntc:Tfntc05)
001000020920     fTfntc00f  o    e             Disk
001100020920
001200020920     d Old_Tfntc     e ds                  ExtName(Tfntc00f) Prefix(Old_)
001300020920     d New_Tfntc     e ds                  ExtName(Tfntc00f)
001400020920     d Trul06Ds      e ds
001500020920     d  Lin                    1     90  0 Dim(30)
001600020926     d Ut§dse0f      e ds
001700020926     d Cncr80        e ds
001800020920     d Kpjba         e ds
001900020920
002000020920     d Wcpo            s                   like(CpoCpo)
002100020920
002200020920     d                 ds
002300020920     d Wpo                     1      3  0
002400020920     d Wcli                    1      7  0
002500001215
002600020920      * Reperisco i dati utente
002700020926     c                   Z-Add     1             Codut
002800020926     c                   Call      'X§PARUT'
002900020926     c                   Parm                    Ut§dse0f
003000020926     c                   Movel     Rec80         Cncr80
003100020926
003200020920      * Reperisco tutti i p.o. del terminal di partenza
003300020920     c                   Clear                   Trul06Ds
003400020920     c                   Eval      D06Cod = '£1'
003500020920     c                   Movel     Simfel        D06Key
003600020920     c                   Movel     Trul06Ds      Kpjbu
003700020920     c                   Call      'TRUL06R'
003800020920     c                   Parm                    Kpjba
003900020920     c                   Movel     Kpjbu         Trul06Ds
004000020920
004100020920     c     Kntc05        Klist
004200020920     c                   Kfld                    Old_NtcApl
004300020920     c                   Kfld                    Old_NtcNk1
004400020920     c                   Kfld                    Old_NtcNk2
004500020920     c                   Kfld                    Old_NtcTnt
004600020920     c                   Kfld                    Old_NtcRnt
004700020920     c                   Kfld                    Old_NtcFlt
004800020920
004900020920     c                   Do        *hival
005000020920      * Leggo il file della 201/101
005100020920     c                   Read      ExTfntc00f
005200020920      * Fine file esco
005300020920     c                   If        %Eof(ExTfntc00f)
005400020920     c                   Leave
005500020920     c                   EndIf
005600020920      * Leggo solo le note non annullate
005700020920     c                   If        Old_NtcFlt = 'A'
005800020920     c                   Iter
005900020920     c                   EndIf
006000020920
006100020920     c                   Select
006200020920      * Se la nota è di tipo 'C' il cliente deve appartanere ad un p.o. del terminal di partenza
006300020920     c                   When      Old_NtcApl = 'C'
006400020920     c                   Move      Old_NtcNk1    Wcli
006500020920     c     Wpo           Lookup    Lin                                    30
006600020920     c                   If        Not *In30
006700020920     c                   Iter
006800020920     c                   EndIf
006900020920      * Se la nota è di tipo 'P' il p.o. del potenziale deve appartenere ad un p.o. del terminal
007000020920      * di partenza
007100020920     c                   When      Old_NtcApl = 'P'
007200020920     c                   Movel     Old_NtcNk1    Wcpo
007300020920     c     Wcpo          Chain     Tncpo01L
007400020920     c                   If        %found(Tncpo01l) and CpoAtb = *Blanks
007500020920     c     CpoFlt        Lookup    Lin                                    30
007600020920     c                   If        Not *In30
007700020920     c                   Iter
007800020920     c                   EndIf
007900020920     c                   EndIf
008000020920      * Se la nota è di tipo 'V' va sempre bene xchè ogni As ha le sue
008100020920     c                   EndSl
008200020920      * Se non è una nota Visita la slfaggo per la trasmissione
008300020924     c                   If        Old_NtcApl <> 'V'
008400020920     c                   Clear                   Old_NtcFlt
008500020920     c                   EndIf
008600020920      * Controllo se ho già scritto il record in GRU
008700020920     c     Kntc05        Chain     Tfntc05l
008800020920      * Se non l'ho ancora scritto lo scrivo
008900020920     c                   If        Not %Found(Tfntc05l)
009000020920     c                   Movel     Old_Tfntc     New_Tfntc
009100020920     c                   Write     Tfntc
009200020920     c                   EndIf
009300020920
009400020920     c                   EndDo
009500020920
009600020920     c                   Eval      *InLr = *On
