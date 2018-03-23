002300071018     Ftntlz00f  uf   E             disk
002301071212     Fanfrn01l  if   E           k disk
003600951009?     *--------------------------------------------------------------*
003700951009?     *  DS                                                          *
003800951009?     *--------------------------------------------------------------*
003900951009     D KPJBA         E DS
003901071018     D tntlzds       E DS                  extname(tntlz00f)
003902071018     d rrntlz          s              9  0
003903071012     c*
003904071018     C     *ENTRY        PLIST
003905071018     C                   PARM                    KPJBA
003911071212     c* annullo codice non trovato in proj
007601031119     C/EXEC SQL
007602071212     C+ DECLARE A1 CURSOR FOR SELECT rrn(tntlz00f) FROM
007603071212     C+ tntlz00f exception join anfrn00f on tlzcsf = frnsocieta and
007604071212     C+ digits(tlzpdr) = substr(frnksc, 2, 7) and '000400' = frnkcc
007607071012     C/END-EXEC
007608071012     C/EXEC SQL
007609071012     C+ OPEN A1
007610071012     C/END-EXEC
009300020405
009400020405     C                   DOU       SqlCod <> 0
010100030827     C/EXEC SQL
010200071212     C+ FETCH NEXT FROM A1 INTO :rrntlz
010300030827     C/END-EXEC
010500020405     c*
010600020405     C                   SELECT
010700020405     C                   WHEN      SqlCod = 0
010702071018     c     rrntlz        chain     tntlz00f
010704071018     c                   if        %found(tntlz00f)
010706071212     c                   eval      tlzatb = '*'
010728071207     C                   UPDATE    tntlz000
010729071212     c                   end
011100020405     C                   WHEN      SqlCod = 100
011200020405     C                   WHEN      SqlCod <> 0
011300020417      * Forzo la stampa del JOBLOG.
011400020417     C                   CALL      'X66CHGJOB'
011500020405     C                   ENDSL
011600020404     C                   ENDDO
011700020417     C/EXEC SQL
011800020417     C+ CLOSE A1
011900020417     C/END-EXEC
012263071212     C*
012264071212     c* annullo codice non trovato in proj
012265071212     C/EXEC SQL
012266071212     C+ DECLARE A2 CURSOR FOR SELECT rrn(tntlz00f) FROM
012267071212     C+ tntlz00f exception join anrco00f on tlzcsf = rcosocieta and
012268071212     C+ digits(tlzpdr) = substr(rcoksc, 2, 7) and '000400' = rcokcc
012269071212     C+ where tlzatb = ' '
012270071212     C/END-EXEC
012271071212     C/EXEC SQL
012272071212     C+ OPEN A2
012273071212     C/END-EXEC
012274071212
012275071212     C                   DOU       SqlCod <> 0
012276071212     C/EXEC SQL
012277071212     C+ FETCH NEXT FROM A2 INTO :rrntlz
012278071212     C/END-EXEC
012279071212     c*
012280071212     C                   SELECT
012281071212     C                   WHEN      SqlCod = 0
012282071212     c     rrntlz        chain     tntlz00f
012283071212     c                   if        %found(tntlz00f)
012284071212     c                   eval      tlzatb = '*'
012285071212     C                   UPDATE    tntlz000
012286071212     c                   end
012287071212     C                   WHEN      SqlCod = 100
012288071212     C                   WHEN      SqlCod <> 0
012289071212      * Forzo la stampa del JOBLOG.
012290071212     C                   CALL      'X66CHGJOB'
012291071212     C                   ENDSL
012292071212     C                   ENDDO
012293071212     C/EXEC SQL
012294071212     C+ CLOSE A2
012295071212     C/END-EXEC
012296071012     c                   seton                                        lr
