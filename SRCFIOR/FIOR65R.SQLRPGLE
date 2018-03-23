000100070720     h DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200070205     h dftactgrp(*no) actgrp(*caller)
000300070720      * FIOR65R *----------------------------------------------------*
000400070720      *         Elimina riferimenti distinta da ORG se ORMNDC=0
000500070720      *--------------------------------------------------------------*
000600070913     fFior63p   o    e             Printer
000700070720     fFnorg01l  uf   e           k disk    commit infds(orginfds)
000800070720      *--------------------------------------------------------------*
000900070720      *   DS
001000070720      *--------------------------------------------------------------*
001100070720     d orginfds        ds
001200070720     d  orgnrr               397    400i 0
001300070720     d trul82ds      e ds
001400070720     d ddatiute      e ds
001500070720     d azuteds       e ds                  extname(AZUTE00F)
001600070720     d fnormds       e ds                  extname(fnorm00f)
001700070720     d tibs34ds      e ds                  inz
001800070720     d KPJBA         e ds
001900070720     d Fior66ds      e Ds
002000070720     ddtaiso           s               d
002100070720     ddtaeur           s               d   datfmt(*eur)
002200070720     d Errmsg          s             78    dim(1) ctdata perrcd(1)              MSG DI ERRORE
002300070720      *--------------------------------------------------------------*
002400070720      *  Ciclo principale
002500070720      *--------------------------------------------------------------*
002600070720      * leggo gli ORM  senza numero distinta per pulire i riferimenti da ORG
002700070720
002800070720     C/EXEC SQL
002900070720     C+ Declare A1 cursor for select  Fnorm01l.*
003000070720     C+ from fnorg01l join fnorm01l on
003100070720     C+ ormpoe = orgpoe and ormnsr = orgnsr and
003200070720     C+ ormnor = orgnor and ormnrv = orgnrv
003300070720     C+ WHERE ormndc = 0 and ormeti <> ' ' and
003400070720     C+ orgpocgi = :or66fgsi  order by
003500070720     C+ Fnorg01l.Orgpocgi , Fnorg01l.Orgcgi
003600070720     C/END-EXEC
003700070720
003800070720     C/EXEC SQL
003900070720     C+ OPEN a1
004000070720     C/END-EXEC
004100070720
004200070720    1c                   do        *hival
004300070720
004400070720     C/Exec sql
004500070720     C+ fetch next from a1 into  :fnormds
004600070720     C/end-exec
004700070720
004800070720    2c                   select
004900070720     c                   when      sqlcod = 100
005000070720     c                   leave
005100070720     c                   WHEN      SqlCod < 0
005200070720     c                   if        sqlcod <> -913
005300070720     c                   seton                                        H1
005400070720     c                   end
005500070720     c                   seton                                        83
005600070720     c                   exsr      srstampa
005700070720     c                   exsr      srfine
005800070720     c                   other
005900070720      * aggiorno ORG
006000070720     c     korg          chain(e)  fnorg01l
006100070720    3c                   if        %error
006200070720      * reperisco il RRN del file
006300070720     c     korg          chain(n)  fnorg01l
006400070720    4c                   if        %found(Fnorg01l)
006500070720     c                   exsr      geslck
006600070720    5c                   if        ul82§sts = 'A'
006700070720     c                   seton                                        83
006800070720     c                   exsr      srstampa
006900070720     c                   exsr      srfine
007000070720   x5c                   else
007100070720     c     korg          chain     Fnorg01l
007200070720    5c                   end
007300070720    4c                   end
007400070720    3c                   end
007500070720    3c                   if        %found(fnorg01l)
007600070720     c                   eval      orgndc = 0
007700070720     c                   eval      orgpdc = 0
007800070720     c                   eval      orgfgs = 0
007900070720     c                   eval      orgddc = 0
008000070720     c                   eval      orgnftl= 0
008100070720     c                   eval      orgslo = 0
008200070720     c                   eval      orgdtvdis= 0
008300070720     c                   eval      orghvdis= 0
008400070720     c                   update    fnorg000
008500070720    3c                   end
008600070720    2c                   endsl
008700070720    1c                   enddo
008800070720     C/EXEC SQL
008900070720     C+ CLOSE a1
009000070720     C/END-EXEC
009100070720
009200070720     c                   if        OR66cmti= 'S'
009300070720     c                   commit
009400070720     c                   end
009500070720     c                   eval      OR66pag = pages
009600070720     c                   eval      OR66riga= riga
009700070720
009800070720     c                   eval      kpjbu = fior66ds
009900070720     C                   SETON                                        LR
010000070720
010100070720      *--------------------------------------------------------------*
010200070720      *  Operazioni finali in caso di errore
010300070720      *--------------------------------------------------------------*
010400070720     C     srfine        BEGSR
010500070720     c                   if        or66cmti= 'S'
010600070720     c                   rolbk
010700070720     c                   end
010800070720     c                   eval      or66erro= '1'
010900070720     c                   eval      kpjbu = fior66ds
011000070720     c                   seton                                        lr
011100070720     c                   return
011200070720     c                   endsr
011300070720      *------------------------------------------------------------------------*
011400070720      * stampa
011500070720      *------------------------------------------------------------------------*
011600070720     c     srstampa      BEGSR
011700070720     c                   move      or66dfvi      dtaiso
011800070720     c                   move      dtaiso        dtaeur
011900070720     c                   move      dtaeur        prtdfv
012000070720      * PAGINA IN STAMPA
012100070720     c                   IF        *IN80 OR RIGA >=60
012200070720     c                   SETON                                        80
012300070720     c                   ADD       1             PAGES
012400070720     c                   Z-ADD     3             RIGA              3 0
012500070720     c                   END
012600070720
012700070720     c   80              write     testa
012800070720     c                   setoff                                       80
012900070720     c   83              write     riga3
013000070720     c   83              ADD       1             RIGA
013100070720     c                   setoff                                       83
013200070720
013300070720     c                   endsr
013400070720      *--------------------------------------------------------------*
013500070720      *  Operazioni iniziali
013600070720      *--------------------------------------------------------------*
013700070720     c     *INZSR        BEGSR
013800070720
013900070720     c     *ENTRY        PLIST
014000070720     c                   PARM                    KPJBA
014100070720     c                   MOVEL(P)  KPJBU         Fior66ds
014200070720     c                   clear                   or66erro
014300070720     c                   eval      pages = or66pag
014400070720     c                   eval      riga = or66riga
014500070720
014600070720     c     *dtaara       define    §azute        azuteds
014700070720     c     *dtaara       define    §datiute      ddatiute
014800070720     C                   in(E)     *dtaara
014900070720     C                   IF        %Error  or  RSUT = *blanks
015000070720     C                   call      'TIBS34R'
015100070720     C                   parm                    Tibs34Ds
015200070720     C                   in        *dtaara
015300070720     c                   ENDIF
015400070720      *  Definisco chiavi di accesso
015500070720     C     Korg          KLIST
015600070720     C                   KFLD                    ormpoe
015700070720     C                   KFLD                    ormnsr
015800070720     C                   KFLD                    ormnor
015900070720     C                   KFLD                    ormnrv
016000070720
016100070720     C                   ENDSR
016200070720      *----------------------------------------------------*
016300070720     c     geslck        begsr
016400070720      *----------------------------------------------------*
016500070720     c                   clear                   trul82ds
016600070720     c                   Eval      ul82§rrn = orgnrr
016700070720     c                   Eval      UL82§FIL = 'FNORG00F'
016800070720     c                   Eval      UL82§WIN = 'S'
016900070720     c                   Eval      UL82§num = 2
017000070720     c                   Eval      UL82§att = 2
017100070720     c                   eval      ul82§mss = ErrMsg(1)
017200070720     c                   Eval      UL82§msw = 'L''ORM '
017300070720     c                             + %editc(orgpoe:'Z') +  ' '
017400070720     c                             + %editc(orgnsr:'Z') +  ' '
017500070720     c                             + %editc(orgnor:'Z') +  ' '
017600070720     c                             + %editc(orgnrv:'Z') +  ' è già utilizzata -
017700070720     c                             da un altro lavoro e non è manutenzionabile'
017800070720      * Effettuo la chiamata al *PGM d utilità
017900070720     c                   Call(e)   'TRUL82R'
018000070720     c                   Parm                    Trul82ds
018100070720
018200070720     C                   ENDSR
018300070720** ErrMsg
018400070720Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
