000200051229     H DECEDIT('0,') DATEDIT(*YMD.)
000300011127     H*--------------------------------------------------------------*
000400051229     H*             - lancio pgm moltiplicatori -                    *
000500000000     H*--------------------------------------------------------------*
000501051229     D wrkGetLista     S           4096
000502051229     D                                     VARYING
001401051229     d dtaiso          s               d
001402051229     d oggi            s                   like(*date)
001403051228     d dtup          e ds
001404051229     d tntu05ds      e ds
001405051229     d tntbeds       e ds                  extname(tntbe00f)
001506051207     D kpjba         E DS
001803051228     C**************************************************************************
001804051228     C* compongo istruzione sql
001805051228     C**************************************************************************
001807051228     **?Imposto la parte fissa dell'istruzione.
001808051228     C                   EVAL      wrkGetLista =
001809051229     C                             'SELECT tbeke1, tbeuni from tntbe01l where  -
001810051229     C                             tbecod = ''TUP'' and tbeatb = '' '' -
001811051229     C                             and substr(tbeuni, 15, 1) = ''S'' -
001812051229     C                             and substr(tbeuni, 16, 1) = ''F'''
001819051228     **?Ordinamento.
001820051228     C                   EVAL      wrkGetLista = wrkGetLista +
001821051228     C                             ' ORDER BY substr(tbeuni, 12, 3)'
001822051228     **?Solo lettura.
001823051228     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
001824051228     C
001825051228     C/EXEC SQL
001826051228     C+ PREPARE s1 FROM :wrkGetLista
001827051228     C/END-EXEC
001828051228     C
001829051228     C/EXEC SQL
001830051228     C+ DECLARE A1 CURSOR FOR S1
001831051228     C/END-EXEC
001832051228     C
001833051228     C/EXEC SQL
001834051228     C+ OPEN a1
001835051228     C/END-EXEC
001841051228
001842051228     C                   DO        *hival
001843051228     C/EXEC SQL
001844051229     C+ FETCH NEXT FROM a1 INTO :tbeke1, :tbeuni
001845051228     C/END-EXEC
001846051228     C                   SELECT
001847051228     c*
001848051228     C                   WHEN      SqlCod = 100
001852051228     c                   leave
001853051228     C                   WHEN      SqlCod < 0
001854051228     C                   seton                                        H1
001855051228     c                   exsr      srfine
001856051228     c                   other
001857051229     c* sottometto azioni
001859051229     c                   if        tbeke1 <> ' '
001860051229     c                   exsr      srkpjba
001861051229     c                   end
001863051228     C                   ENDSL
001864051228      *
001865051228     C                   ENDDO
001866051228     C/EXEC SQL
001867051228     C+ CLOSE a1
001868051228     C/END-EXEC
001869051229     c                   seton                                        lr
055601051130     C******************************************************
055602051130     C** imposta kpjba
055603051130     C******************************************************
055604051130     C     srkpjba       BEGSR
055605051229     c                   clear                   tntu05ds
055606051229     c                   movel     tbeuni        dtup
055607051229     C* imposta data inizio = ultima elaborazione + 1
055609051229     c                   move      §tupdta       dtaiso
055610051229     c                   adddur    1:*d          dtaiso
055611051229     c                   movel     dtaiso        t05dti
055612051229     C* imposta data fine = oggi - gg da tabella
055613051229     c                   move      oggi          dtaiso
055614051229     c                   subdur    §tupgg:*d     dtaiso
055615051229     c                   movel     dtaiso        t05dtf
055616051229     c                   movel(p)  tntu05ds      kpjbu
055619051207     C                   movel     'TEMPI_UFF'   KNMEB
055638051229     c                   movel     tbeke1        kcoaz
055641051130     c                   call      'BCH10'
055642051130     c                   parm                    kpjba
055643051130     C                   ENDSR
055700011127     C******************************************************
055800011127     C     *INZSR        BEGSR
055900011127     C******************************************************
056000011127     C*
056001051229     c     *entry        plist
056002051229     c                   parm                    kpjba
056003051229     c* ho previsto di ricevere una data nel caso si voglia elaborare un
056004051229     c* periodo diverso (eventuale ricalcolo)
056005051229     c                   if        kpjbu <> *blanks
056006051229     c                   movel     kpjbu         oggi
056007051229     c                   else
056008051229     c                   move      *date         oggi
056009051229     c                   end
059500011127     C                   ENDSR
059600051229     C**************************************************************************
059700051229     C* esco con codice d'errore
059800051229     C**************************************************************************
059900051229     C     srfine        BEGSR
060000051229     c                   seton                                        lr
060100051229     c                   return
060200051229     c*
060300051229     c                   endsr
