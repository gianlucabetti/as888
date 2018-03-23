000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400120306     fTABEL00F  if   e           k disk
000500120306     D  dsATS        e DS                  extname(AIATS00F)
000600120306     D  ds3I         e DS
000700120306     D soctraini       S              3
000800120216      * utility data una partita IVA verifica se esiste nell'anagrafica certificazione traini di Pedini
000900020624      *
001000020624     D param           DS
001100120216     D  pariva                       16
001101120502     D  pardtin                       8s 0
001102120502     D  pardtfi                       8s 0
001200120216     d  esito                         1
001300030109     d  error                         1
001301120502     d  errmsg                       75
001401120502      * ?_______________________________________________________________________
001402120502      *?  in caso di nuovi errori possibili aggiungere nella legenda le specifiche
001403120502      *?
001404120502      * ?Legenda errori possibili di ritorno
001405120502     C*Legenda errori possibili di ritorno
001406120502     C* esito <> '1' non trovata anagrafica trazionista
001407120502     C* error =  '1' non trovata anagrafica trazionista
001408120502     C* error =  '2' Data decorrenza allegato inferiore a decorrenza contratto
001409120502     C* esito =  '3' Data scadenza allegato maggiore di data scadenza contratto
001410120502      * ?_______________________________________________________________________
001500020624     c     *entry        plist
001600030109     c                   parm                    param
001700120306     c                   parm                    dsATS
001800120306     c*
001900120306     c     kTBL          klist
002000120306     c                   kfld                    tblKUT
002100120306     c                   kfld                    tblCOD
002200120306     c                   kfld                    tblKEY
002300120306     c*
002400120307     c                   z-add     1             tblKUT
002500120306     c                   move      '3I'          tblCOD
002600120306     c                   clear                   tblKEY
002700120306     c                   movel     '1'           tblKEY
002800120306     c                   clear                   soctraini
002900120306     c     kTBL          chain     tabel00f
003000120306      **  Società che gestisce i TRAINI ricavata tramite il nome del S.I.
003100120306     c                   if        %Found(Tabel00f)
003200120306     c                   eval      ds3i = tblUNI
003300120306     c                   move      §3IBST        SISINFO          10
003400120306     c                   move      SISINFO       societa_0         4
003500120306     c                   movel     societa_0     soctraini
003600120306     c                   end
003700120306     c*
003800120216     c                   clear                   esito
003801120502     c                   clear                   errmsg
003900120216     c                   clear                   error
004000120306     c                   clear                   DSATS
004100030109
004400020313     C/EXEC SQL
004500120306     C+ SELECT '1', aiats00f.* into :esito, :DSats
004600120306     C+    FROM aiats00F WHERE atspiva = :pariva  and
004700120502     C+         atsSOC = :soctraini
004800120502     C+         and atsdec > 0  fetch first row only
004900020313     C/END-EXEC
005000030109      *
005100030109     c                   if        sqlcod < 0
005200030109     c                   move      '1'           error
005201120502     c                   eval      errmsg = 'Non trovata anagrafica -
005202120502     c                             come trazionista '
005300030109     c                   endif
005400030109     c
005401120502      * se lo trovo fra i trazionisti e ho inserito le date verifico anche periodo di validità
005402120502     c                   if        error = *blank and esito = '1'
005403120502     c                             and pardtin > 0
005404120502     c                   select
005405120502     c                   when      pardtin < atsdec
005406120502     c                   eval      error = '2'
005407120502     c                   eval      errmsg = 'Data decorrenza allegato -
005408120502     c                             minore di decorrenza contratto'
005409120508     c                   when      pardtfi > 0 and atsdfc > 0 and
005410120508     c                             pardtfi > atsdfc
005411120502     c                   eval      error = '3'
005412120502     c                   eval      errmsg = 'Data scadenza allegato -
005413120502     c                             maggiore di scadenza contratto'
005414120502     c                   endsl
005415120502     c                   endif
005416120502     c
005500020624      *
005600030109     c                   seton                                        lr
005700011129     C*--------------------------------------------------------------------
