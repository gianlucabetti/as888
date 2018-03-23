000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300090303     h* Il pgm pulisce l'archivio FNLRF70F x le quadratura dist.consegne
000400020409     h*****************************************************************
000401090303     ftabel00f  if   e           k disk
000700000607     D kpjba         e ds
000900090303     d pardt           s             14  0
000901090303     D DS5A2         E DS
000902090303     D WLBDAT          DS
000903090303     D  G02DAT                 1      8  0
000904090303     D  G02INV                 9     16  0
000905090303     D  G02ERR                17     17
000906090303     D  G02TGI                18     22  0
000907090303     D WGIDAT          DS
000908090303     D  GIODAT                 1      8  0
000909090303     D  GIOINV                 9     16  0
000910090303     D  GIOTGI                17     21  0
001100020702     c*
001200090303     c* cancello tutti i record che hanno data elaborazione<= data tabella
001201090303     c* 5a
001202040218     c*
001301031124     C/EXEC SQL
001302090303     C+ DELETE FROM FNLRF70F WHERE wf7tim <= :pardt
001304031124     C/END-EXEC
001305040218     c*
004000030213     C                   seton                                        lr
004100000607      *-----------------------------------------------
004200000607     c     *inzsr        begsr
004300000607      *-----------------------------------------------
004400000607     c
004500000607     c     *entry        plist
004600020205     c                   parm                    kpjba
004601090303     c                   clear                   pardt
004602090303     C     KTAB          KLIST
004603090303     C                   KFLD                    tblkut
004604090303     C                   KFLD                    tblcod
004605090303     C                   KFLD                    tblkey
004606090303     C* LA DATA DEL GIORNO LA PRENDO DA TIME
004607090303     C                   TIME                    W0120            14 0
004608090303     C                   MOVE      W0120         WDAT              8 0
004609090303     C*
004610090303     c                   clear                   wlbdat
004611090303     C                   Z-ADD     WDAT          G02DAT
004612090303     C                   MOVEL     *BLANK        G02ERR
004613090303     C                   CALL      'XSRDA8'
004614090303     C                   PARM                    WLBDAT
004701090303      * DATA PULIZIA: REPERISCO GIORNI DA DS5A2
004703090303     C                   Z-ADD     1             tblkut
004704090303     C                   MOVEL     '5A'          tblCOD
004705090303     C                   MOVEL     '2       '    tblKEY
004706090303     C     KTAB          CHAIN     TABEL                              43
004707090303     C  N43              MOVEL     TBLUNI        DS5A2
004708090303     C   43              MOVEL     *ZEROS        DS5A2
004709090303      *
004710090303      * SOTTRAGGO §5A$7  AI GIORNI
004711090303     C     G02TGI        SUB       §5A$7         GIOTGI
004712090303      *
004713090303     C                   CALL      'XSRGI8'
004714090303     C                   PARM                    WGIDAT
004715090303     C                   movel     GIOinv        pardt
005300020402     c*
005400020402     c                   endsr
