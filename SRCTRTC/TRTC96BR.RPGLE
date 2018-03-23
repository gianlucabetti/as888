000100960605      ***********************************************************************
000200960605      *   IMPORTA BOLLE DA PARTNER ESTERI            - E.D.I.   ESTERO -    *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500960605     F*---------------------------------------------------------------------*
000600960605     F*  DATA BASE                                                          *
000700960605     F*---------------------------------------------------------------------*
000800960607     FEDTAB01L  UF A E           K DISK
000900960605     D*---------------------------------------------------------------*
001000960605     D*  DS
001100960605     D*---------------------------------------------------------------*
001200960605     D KPJBA         E DS
001300050823     D DSGG          E DS                  EXTNAME(EDIDSGG)
001400050823     d kcod            s                   like(tabcod)
001500050823     d kkey            s                   like(tabkey)
001600960612     I*----------------------------------------------------------------
001700050823      * Definizione chiavi
001800050823     C     KTAB          KLIST
001900050823     C                   KFLD                    KCOD
002000050823     C                   KFLD                    KKEY
002100050823      *
002200050823     C     *ENTRY        PLIST
002300050823     C                   PARM                    KPJBA
002400050823     C                   MOVEL     kpjbu         DSgg
002500050823      **
002600050823     c                   move      *blank        datarcv           8
002700050823     c                   if        §ggDTR <> *zero
002800050823     c                   move      §ggDTR        datarcv           8
002900050823     c                   end
003000050823      **
003100050823     c                   move      *blank        datasnd           8
003200050823     c                   if        §ggDTS <> *zero
003300050823     c                   move      §ggDTS        datasnd           8
003400050823     c                   end
003500050823      **
003600050823     C* Caricamento dati tabella ER  monorecord
003700050823     c                   MOVEL     'GG'          KCOD
003800050823     C                   eval      KKEY = 'PULISNDRCV'
003900050823     C                   clear                   DSgg
004000050823     C     KTAB          CHAIN     EDTAB01L                           30
004100050823      **
004200050823     C     *IN30         IFEQ      '0'
004300050823     C                   MOVEL     TABUNI        DSgg
004400050823      **
004500050823      * x aggiornare ore:minuti o codice errore
004600050823     c                   if        datarcv <> *blank or datasnd <> *blank
004700050823      * data RCV
004800050823     c                   if        datarcv <> *blank
004900050823     c                   move      datarcv       §ggDTR
005000050823     c                   end
005100050823      * data SND
005200050823     c                   if        datasnd <> *blank
005300050823     c                   move      datasnd       §ggDTS
005400050823     c                   end
005500050823      *
005600050823     C                   MOVEL     DSgg          TABUNI
005700050823     c                   update    edtab000
005800050823     c                   else
005900050823      ** rilascia il file
006000050823     c                   unlock    edtab01l                             99
006100050823     c                   end
006200050823      **
006300050823     C                   END
006400050823     C*
006500050823     c                   clear                   kpjbu
006600050823     C                   MOVEL     DSgg          kpjbu
006700960605      * Fine Lavoro
006800960605     C     FINE          TAG
006900960605     C                   SETON                                        LR
007000960605     C*---------------------------------------------------------------*
