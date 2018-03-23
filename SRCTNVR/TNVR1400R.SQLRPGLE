002104071012     Ffiftt06l  IF   E           K DISK
002105071012     Ffiatt06l  IF   E           K DISK
002107071012     Fansog08j  IF   E           K DISK
002200020415     F*--------
002300071012     Fwffrn00f  o    E             disk
003600951009?     *--------------------------------------------------------------*
003700951009?     *  DS                                                          *
003800951009?     *--------------------------------------------------------------*
003900951009     D KPJBA         E DS
003901071012     d conta           s              5  0
003902071012     c*
007601031119     C/EXEC SQL
007602071012     C+ DECLARE A1 CURSOR FOR SELECT SOGPARTIVA, count(*) FROM ansog08j
007603071012     C+ GROUP BY SOGPARTIVA HAVING count(*) > 1
007607071012     C/END-EXEC
007608071012     C/EXEC SQL
007609071012     C+ OPEN A1
007610071012     C/END-EXEC
009300020405
009400020405     C                   DOU       SqlCod <> 0
010100030827     C/EXEC SQL
010200071012     C+ FETCH NEXT FROM A1 INTO :sogpartiva, :conta
010300030827     C/END-EXEC
010500020405     c*
010600020405     C                   SELECT
010700020405     C                   WHEN      SqlCod = 0
010701071012     c     sogpartiva    setll     ansog08j
010702071012     c                   do        *hival
010703071012     c     sogpartiva    reade     ansog08j
010704071012     c                   if        %eof(ansog08j)
010705071012     c                   leave
010706071012     c                   end
010715071012     c     kfat          setll     fiftt06l                               01
010716071012     c     kfat          setll     fiatt06l                               02
010718071012     c* scrivo il wf
010719080909     c                   if        *in01 or *in02
010720071012     c                   eval      wfpartiva = sogpartiva
010721071012     c                   eval      wfcdfisc = sogcdfisc
010722071012     c                   eval      wfsocold = frnsocieta
010723071012     c                   eval      wfksc = frnksc
010724071012     c                   eval      wfdes = sogdes
010725080909     c                   clear                   wfsocnew
010726071012     c                   eval      wfforfatt= frnforfatt
010727071012     c                   if        *in01
010728071012     c                   eval      wfftt='S'
010729071012     c                   else
010730071012     c                   clear                   wfftt
010731071012     c                   end
010732071012     c                   if        *in02
010733071012     c                   eval      wfatt='S'
010734071012     c                   else
010735071012     c                   clear                   wfatt
010736071012     c                   end
010740071012     c                   clear                   wfctt
010742071012     c                   write     wffrn000
010743080909     c                   end
010744071012     c                   enddo
011100020405     C                   WHEN      SqlCod = 100
011200020405     C                   WHEN      SqlCod <> 0
011300020417      * Forzo la stampa del JOBLOG.
011400020417     C                   CALL      'X66CHGJOB'
011500020405     C                   ENDSL
011600020404     C                   ENDDO
011700020417     C/EXEC SQL
011800020417     C+ CLOSE A1
011900020417     C/END-EXEC
012200951009     C*
012201071012     c                   seton                                        lr
020800951011?     *--------------------------------------------------------------*
020900951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
021000951011?     *--------------------------------------------------------------*
021100951011     C     *INZSR        BEGSR
021200951011      *
021300951011     C     *ENTRY        PLIST
021400951011     C                   PARM                    KPJBA
021401071012     c     kfrn          klist
021402071012     c                   kfld                    sogpartiva
021403071012     c                   kfld                    frnsocieta
021404071012     c     kfat          klist
021405071012     c                   kfld                    frnsocieta
021406071012     c                   kfld                    frnksc
022500951012     C                   ENDSR
