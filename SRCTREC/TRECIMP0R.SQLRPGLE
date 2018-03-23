000100110401     h* crea l'archivio delle tariffe nella libreria  di OUTPUT (libout).
000200110401     h* Ci saranno quelle che decorrono (dtadec) nel 2013 (libin) + quelle
000300110401     h* di gaitra201. Quelle presenti in entrambi gli archivi verranno fatte
000400110401     h* scadere al 31/12/2012 (dtadec -1 gg)
000500100729     H DEBUG DECEDIT('0,') DATEDIT(*ymd/)
000600110401     fecevdout  if a e             disk    prefix(o) rename(ecevd000:ecevdo)
000700131128     fecevd06l  if   e           k disk
000800110420     d param           ds
000900110420     Dlibout                         10
001000110420     Dlibin                          10
001100110420     Dlib                            10
001200110420     Ddtadec                          8s 0
001300121122     Dctr                             1
001400121122     Dpercli                          1
001500110420     Ddtaiso           s               d
001600110420     Dcomdta           s                   like(dtadec)
001700110401     Dvoce             s                   like(evdvoc)
001800110401     Dprog             s                   like(evdprg)
001900110420     Dkpjba          e ds
002000110420     Ddsecevd        e ds                  extname(ecevd00f)
002100110401     Ddsecevdo       e ds                  extname(ecevd00f) prefix(o)
002200110401     c* ricevo la libreria sulla quale elaborare le tariffe
002300110401     c     *entry        plist
002400110420     c                   parm                    kpjba
002500110420     c                   movel     kpjbu         param
002600110420     c                   movel     dtadec        dtaiso
002700110401     c                   subdur    1:*d          dtaiso
002800110401     c                   movel     dtaiso        comdta
002900110401     c     kevd          klist
003000110401     c                   kfld                    voce
003100110401     c                   kfld                    prog
003200110401     C* per il momento è di sicurezza
003300110401     c                   if        libout = 'UNITRAGRU'
003400110401     c                   seton                                        lr
003500110401     c                   return
003600110401     c                   end
003700110401     c* libreria input e output uguali tengo buone solo le tariffe 2013
003800110401     c                   if        libin = libout
003900100324     C/EXEC SQL
004000110401     C+ DELETE FROM ECEVDOUT WHERE evdddt < :dtadec
004100100324     C/END-EXEC
004200110401     c* libreria input <> output pulisco il file di output e accodo le
004300110401     c* le tariffe del 2013
004400110401     c                   else
004500110401     C/EXEC SQL
004600110401     C+ DELETE FROM ECEVDOUT
004700110401     C/END-EXEC
004800110401     C/EXEC SQL
004900110401     C+ INSERT INTO ECEVDOUT SELECT * FROM ecevdin WHERE evdddt = :dtadec
005000110401     C/END-EXEC
005100110401     c                   end
005200110401     c*
005300110401     C/EXEC SQL
005400110401     C+ DECLARE E1 CURSOR FOR SELECT evdvoc, evdprg FROM ecevdout GROUP
005500110401     C+ BY evdvoc, evdprg
005600110401     C/END-EXEC
005700100324     c*
005800100324     C/EXEC SQL
005900100324     C+ OPEN e1
006000100324     C/END-EXEC
006100100324
006200100324     C                   DO        *HIVAL
006300100324
006400100324     C/EXEC SQL
006500110401     C+ FETCH NEXT FROM e1 INTO :voce, :prog
006600100324     C/END-EXEC
006700100324
006800100324     C                   IF        SQLCod = 100 or sqlcod < 0
006900100324     C                   LEAVE
007000100324     C                   ENDIF
007100110401     c* controllo che in 201 non esista un progressivo successivo per
007200110401     c* sicurezza
007300131128     c     voce          setgt     ecevd06l
007400131128     c     voce          readpe    ecevd06l
007500131128     c                   if        not %eof(ecevd06l) and evdprg > prog
007600110401     c                   seton                                        lr
007700110401     c                   return
007800110401     c                   end
007900121204     c* leggo tutte le tariffe della voce e controllo se devo far scdere
008000121204     c* le tariffe
008100131128     c     voce          setll     ecevd06l
008200110401     c                   do        *hival
008300131128     c     voce          reade     ecevd06l
008400131128     c                   if        %eof(ecevd06l)
008500110401     c                   leave
008600110401     c                   end
008700121122     c* esclude personalizzazioni clienti
008800121205     c                   if        (percli = 'N' and evdcli <> 0) or
008900121205     c                              evdatb <> ' '
009000121122     c                   iter
009100121122     c                   end
009200140922     c                   if         evdddt >= dtadec
009300140922     c                   iter
009400140922     c                   end
009500121122     c*
009600110401     c                   movel     dsecevd       dsecevdo
009700110404     c                   if        evddst >= dtadec
009800110404     c                   eval      oevddst = comdta
009900110404     c                   end
010000110401     c                   write     ecevdo
010100100324     C                   ENDDO
010200110404     C                   ENDDO
010300100324
010400100324     C/EXEC SQL
010500100324     C+ CLOSE e1
010600100324     C/END-EXEC
010700110401     c* inserisco tutte le voci progressivi che non ho già inserito
010800121129     c                   if        percli = 'N'
010900110401     C/EXEC SQL
011000121129     C+ INSERT INTO ECEVDOUT SELECT A.EVDVOC, A.EVDPRG, A.EVDTTV,
011100131203     C+ A.EVDFT1, A.EVDFT2, A.EVDCLI, A.EVDCTR, A.EVDCTS,
011200131203     C+ a.evdcap, A.EVDDDT, A.EVDDST, A.EVDSGI, A.EVDSGF, A.EVDITR,
011300131203     C+ A.EVDIMI, A.EVDIMA, A.EVDSEG, A.EVDPER, A.EVDRPV, A.EVDDIR,
011400131203     C+ A.EVDDUV, A.EVDFTR, A.EVDDTR, A.EVDATB, A.EVDNAZ, A.EVDCAD FROM
011500131203     C+ ecevd06l a exception join ecevdout o on a.evdvoc = o.evdvoc and
011600160908     C+ a.evdprg = o.evdprg WHERE A.EVDCLI = 0 or (a.evdcli <> 0 and
011601160908     C+ a.evdvoc between 350 and 359)
011700110401     C/END-EXEC
011800121129     c                   else
011900121129     C/EXEC SQL
012000131128     C+ INSERT INTO ECEVDOUT SELECT A.EVDVOC, A.EVDPRG, A.EVDTTV,
012100131203     C+ A.EVDFT1, A.EVDFT2, A.EVDCLI, A.EVDCTR, A.EVDCTS,
012200131203     C+ a.evdcap, A.EVDDDT, A.EVDDST, A.EVDSGI, A.EVDSGF, A.EVDITR,
012300131203     C+ A.EVDIMI, A.EVDIMA, A.EVDSEG, A.EVDPER, A.EVDRPV, A.EVDDIR,
012400131203     C+ A.EVDDUV, A.EVDFTR, A.EVDDTR, A.EVDATB, A.EVDNAZ, A.EVDCAD FROM
012500131203     C+ ecevd06l a exception join ecevdout o on a.evdvoc = o.evdvoc and
012600131203     C+ a.evdprg = o.evdprg
012700121129     C/END-EXEC
012800121129     c                   end
012900100324     c*
013000100324     c                   seton                                        lr
