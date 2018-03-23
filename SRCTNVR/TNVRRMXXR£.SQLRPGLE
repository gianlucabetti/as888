000100940224     H*--------------------------------------------------------------*
000200060512     Ffiar401l  UF A E           k DISK
000300060512     f                                     rename(fiar4000:fiar401)
000400060512     f                                     prefix(N_)
000500060512     d fiar4ds       e ds                  extname(fiar400f)
000600060512     D wrkGetLista     S           4096
000700060512     D                                     VARYING
000800060512     d dsbl4f        e ds
000900060512     d dsbl4I        e ds
001000060512     d dsbl4K        e ds
001100060512     D wtrc            S                   LIKE(ar4trc)
001200900131     C*---------------------------------------------------------------*
001300060512     c     kar4          klist
001400060512     c                   kfld                    ar4aas
001500060512     c                   kfld                    ar4lnp
001600060512     c                   kfld                    ar4nrs
001700060512     c                   kfld                    ar4nsp
001800060512     c                   kfld                    wtrc
001900060512     c
002000060512     **?Imposto la parte fissa dell'istruzione.
002100060512     C                   EVAL      wrkGetLista =
002200060512     C                             'SELECT * from fiar401l where  -
002300060512     C                             ar4trc = ''F'''
002400060512     **?Solo lettura.
002500060512     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
002600060512     C
002700060512     C/EXEC SQL
002800060512     C+ PREPARE s1 FROM :wrkGetLista
002900060512     C/END-EXEC
003000060512     C
003100060512     C/EXEC SQL
003200060512     C+ DECLARE A1 CURSOR FOR S1
003300060512     C/END-EXEC
003400060512     C
003500060512     C/EXEC SQL
003600060512     C+ OPEN a1
003700060512     C/END-EXEC
003800060512
003900060512     C                   DO        *hival
004000060512     C/EXEC SQL
004100060512     C+ FETCH NEXT FROM a1 INTO :fiar4Ds
004200060512     C/END-EXEC
004300060512     C                   SELECT
004400060512     c*
004500060512     C                   WHEN      SqlCod = 100
004600060512     c                   leave
004700060512     c                   other
004800060512     c                   movel     ar4not        dsbl4f
004900060512     c* DSBL4I
005000060512     c                   clear                   dsbl4i
005100060512     c                   movel     §b4fnp        §b4ipn
005200060512     c* tolgo il check digit dal parcel number
005300060512     c                   eval      %subst(§b4ipn:12:1)=' '
005400060512     c                   eval      §b4icd = %subst(§b4fnp:12:1)
005500060512     c* depot di arrivo --> muovo uno 0 in prima posizione
005600060517     c                   if        §b4rtn <> *blanks
005700060512     c                   movel     '0'           §b4ida
005800060517     c                   endif
005900060512     C                   MOVE      §b4RTN        §B4ida
006000060512     C                   MOVE      §b4ZON        §B4iza
006100060512     c                   eval      wtrc = 'I'
006200060512     c     kar4          chain     fiar401l
006300060512     c                   movel(P)  dsbl4i        N_ar4not
006400060512     c                   movel     AR4FLO        N_ar4flo
006500060512     c                   z-add     AR4DUV        N_ar4duv
006600060512     c                   movel     AR4FTR        N_ar4ftr
006700060512     c                   z-add     AR4DTR        N_ar4dtr
006800060512     c                   if        %found(fiar401l)
006900060512     c                   update    fiar401
007000060512     c                   else
007100060512     c                   z-add     ar4aas        N_ar4aas
007200060512     c                   z-add     ar4lnp        N_ar4lnp
007300060512     c                   z-add     ar4nrs        N_ar4nrs
007400060512     c                   z-add     ar4nsp        N_ar4nsp
007500060512     c                   movel     'I'           N_AR4TRC
007600060512     c                   write     fiar401
007700060512     c                   endif
007800060512     c* IMPOSTO DSBL4K
007900060512     c                   clear                   dsbl4k
008000060512     c                   movel     §b4fnr        §b4krt
008100060512     c                   if        §b4fnr <> *blanks
008200060512     c                   eval      wtrc = 'K'
008300060512     c     kar4          chain     fiar401l
008400060512     c                   movel(P)  dsbl4k        N_ar4not
008500060512     c                   movel     AR4FLO        N_ar4flo
008600060512     c                   z-add     AR4DUV        N_ar4duv
008700060512     c                   movel     AR4FTR        N_ar4ftr
008800060512     c                   z-add     AR4DTR        N_ar4dtr
008900060512     c                   if        %found(fiar401l)
009000060512     c                   update    fiar401
009100060512     c                   else
009200060512     c                   z-add     ar4aas        N_ar4aas
009300060512     c                   z-add     ar4lnp        N_ar4lnp
009400060512     c                   z-add     ar4nrs        N_ar4nrs
009500060512     c                   z-add     ar4nsp        N_ar4nsp
009600060512     c                   movel     'K'           N_AR4TRC
009700060512     c                   write     fiar401
009800060512     c                   endif
009900060512     c                   endif
010000060512     c*
010100060512     C                   ENDSL
010200060512      *
010300060512     C                   ENDDO
010400060512     C/EXEC SQL
010500060512     C+ CLOSE a1
010600060512     C/END-EXEC
010700060512     c
010800030205      *
010900000000     C                   SETON                                        LR
