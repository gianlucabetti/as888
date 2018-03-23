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
001300050823     D DSER          E DS                  EXTNAME(EDIDSER)
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
002400050823     C                   MOVEL     kpjbu         DSER
002500050823      **
002600050823     c                   move      *blank        oramin            4
002700050823     c                   if        §erORA <> *blank
002800050823     c                   move      §erORA        oramin            4
002900050823     c                   end
003000050823      **
003100050823     c                   move      *blank        coderr            3
003200050823     c                   if        §erCER <> *blank
003300050823     c                   move      §erCER        coderr            3
003400050823     c                   end
003500050823      **
003600050823     C* Caricamento dati tabella ER  monorecord
003700050823     c                   MOVEL     'ER'          KCOD
003800050823     C                   MOVEL     'ERRMSG'      KKEY
003900050823     C                   clear                   DSER
004000050823     C     KTAB          CHAIN     EDTAB01L                           30
004100050823      **
004200050823     C     *IN30         IFEQ      '0'
004300050823     C                   MOVEL     TABUNI        DSER
004400050823      **
004500050823      * x aggiornare ore:minuti o codice errore
004600050823     c                   if        oramin <> *blank or coderr <> *blank
004700050823      * ore:minuti
004800050823     c                   if        oramin <> *blank
004900050823     c                   move      oramin        §erORA
005000050823     c                   end
005100050823      * cod:Errore
005200050823     c                   if        coderr <> *blank
005300050823     c                   move      coderr        §erCER
005400050823     c                   end
005500050823     C                   MOVEL     DSER          TABUNI
005600050823     c                   update    edtab000
005700050823     c                   else
005800050823      ** rilascia il file
005900050823     c                   unlock    edtab01l                             99
006000050823     c                   end
006100050823      **
006200050823     C                   END
006300050823     C*
006400050823     c                   clear                   kpjbu
006500050823     C                   MOVEL     DSER          kpjbu
006600960605      * Fine Lavoro
006700960605     C     FINE          TAG
006800960605     C                   SETON                                        LR
006900960605     C*---------------------------------------------------------------*
