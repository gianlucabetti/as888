000100060626     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200920914     H DECEDIT('0,') DATEDIT(*DMY.)
000300950321?     *--------------------------------------------------------------*
000400080409?     *      controlli x PDA distinta consegne: flag e fasi          *
000500950321?     *--------------------------------------------------------------*
000600080409     FFidg44D   CF   E             WORKSTN
000700080409     ffidst09l  if   e           k disk
000800080409     ffidsf02l  if   e           k disk
000900061012     d kpjba         e ds
001000080409     d ddstflr       e ds
001100080409     d fidg44ds      e ds
001200061012     c*---------------------------------------------------------------*
001300061012     c* FLUSSO PRINCIPALE                                             *
001400061012     c*---------------------------------------------------------------*
001500080409     c* controllo flag pda consegne e fasi PDC, CUS
001600080409     c                   exsr      srctr
001700080409     c* se richiesto emetto la window
001800080428     c                   if        co44wnd = 'S' and co44fcus = 'N'
001900080409     c                   eval      w1fgs = co44fgs
002000080409     c                   eval      w1ndc = co44ndc
002100061012     C                   DO        *hival
002200061012     C                   EXFMT     LRZ1W01
002300080409     c   kl              leave
002400080409     c                   if        *inkf
002500080409     c                   eval      co44fCUS = 'S'
002600080409     C                   leave
002700080409     c                   end
002800061012     c                   enddo
002900080409     c                   end
003000061012     c*
003100080409     c                   eval      co44msg = wmsg
003200080409     C                   MOVEL     Fidg44ds      KPJBU
003300061012     C                   SETON                                        LR
003400080312     c*---------------------------------------------------------------*
003500080312     c     *inzsr        begsr
003600080312     c*---------------------------------------------------------------*
003700061012     c*  Operazioni iniziali
003800061012     C     *ENTRY        PLIST
003900061012     C                   PARM                    KPJBA
004000080409     C                   MOVEL     KPJBU         Fidg44ds
004100080409     c                   clear                   co44err
004200080409     c                   clear                   co44msg
004300080409     c                   clear                   co44fcus
004400080409     C     Kdst          KLIST
004500080409     C                   KFLD                    co44FGS
004600080409     C                   KFLD                    WNPG              1 0
004700080409     C                   KFLD                    co44ndc
004800080409     c                   eval      wnpg = 4
004900080409     C     Kdst4         KLIST
005000080409     C                   KFLD                    wnpg
005100080409     C                   KFLD                    co44ndc
005200080409     C                   KFLD                    co44fgs
005300080409     C                   KFLD                    dsftrd
005400061012     c                   endsr
005500080409     c*---------------------------------------------------------------*
005600080409     c     srctr         begsr
005700080409     c*---------------------------------------------------------------*
005800080409     c                   seton                                        01
005900080409     c                   clear                   wmsg
006000080409     c                   clear                   wmsg1
006100080409     c                   clear                   wmsg2
006200080409     C     KDST          chain     fidst09l
006300080409     c                   if        not %found(fidst09l)
006400080409     c                   eval      co44err = '1'
006500080409     c                   eval      wmsg = 'Distinta inesistente'
006600080409     c                   leavesr
006700080409     c                   else
006800080416     c                   if        co44td = 'E' or  co44td = 'C'
006900080409     c                   eval      ddstflr = dstflr
007000080409     c                   if        dstpda <> 'E' and dstpda <> 'C' and
007100080409     c                             §DSTTSTPDA <> 'E' and §DSTTSTPDA <> 'C'
007200080409     c                   eval      co44err = '1'
007300080409     c                   eval      wmsg = 'Distinta NO PDA consegne'
007400080409     c                   leavesr
007500080409     c                   end
007600080410     c                   end
007700080409     c                   end
007800080416     c* se non ho la fase PDC (preparato x pda con) emetto la window
007900080416     c* d'errore ma non lascio proseguire
008000080416     c                   if        co44td = 'E' or co44td = 'C'
008100080416     c                   eval      dsftrd = 'PDC'
008200080416     C     KDST4         chain     fidsf02l
008300080416     c                   if        not %found(fidsf02l)
008400080416     c                   eval      wmsg = 'Distinta non ancora scaricata a PDA'
008500080416     c                   eval      co44err = '1'
008600080416     c                   end
008700080416     c                   end
008800080409     c* se ho la fase CUS (ok da spc) imposto il flag di ritorno = N per
008900080409     c* dire che non è stato forzato. poi se emetto la window posso forzare
009000080409     c                   eval      dsftrd = 'CUS'
009100080409     C     KDST4         chain     fidsf02l
009200080409     c                   if        %found(fidsf02l)
009300080409     c                   setoff                                       01
009400080409     c                   eval      wmsg = 'Distinta già controllata in uscita'
009500080409     c                   eval      wmsg1= 'F6 per forzare'
009600080409     c                   eval      co44err = '1'
009700080409     c                   eval      co44fcus='N'
009800080409     c                   end
009900080409     c                   endsr
