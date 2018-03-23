000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200120518     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000300000000     H*--------------------------------------------------------------*
000400120306     fTABEL00F  if   e           k disk
000500120518     faiats03l  if   e           k disk
000501120518     D  dsats        e DS                  extname(aiats00f)
000600120306     D  ds3I         e DS
000700120518     D soctraini       S              3  0
000800120216      * utility data una partita IVA verifica se esiste nell'anagrafica certificazione traini di Pedini
000900020624      *
001000020624     D param           DS
001001120518     d  parsoc                        3  0
001100120216     D  pariva                       16
001101120502     D  pardtin                       8s 0
001102120502     D  pardtfi                       8s 0
001200120216     d  esito                         1
001301120502     d  errmsg                       75
001401120502      * ?_______________________________________________________________________
001402120502      *?  in caso di nuovi errori possibili aggiungere nella legenda le specifiche
001404120502      * ?Legenda errori possibili di ritorno
001405120502     C*Legenda errori possibili di ritorno
001406120604     C* esito =  ' ' non trovata anagrafica trazionista
001407120604     C* esito =  '1' trovata anagrafica trazionista ok
001408120604     C* esito =  '2' Data decorrenza contratto futura
001409120604     C* esito =  '3' Data scadenza contratto minore
001410120604     C* esito =  '4' Società fornitore diversa da società traini
001411120502      * ?_______________________________________________________________________
001500020624     c     *entry        plist
001600030109     c                   parm                    param
001700120306     c                   parm                    dsATS
001800120306     c*
001900120306     c     kTBL          klist
002000120306     c                   kfld                    tblKUT
002100120306     c                   kfld                    tblCOD
002200120306     c                   kfld                    tblKEY
002300120306     c*
002301120518     c     kats          klist
002302120528     c                   kfld                    soctraini
002303120518     c                   kfld                    pariva
002304120518     c                   kfld                    dtinik            8 0
002305120518     c     kats2         klist
002306120528     c                   kfld                    soctraini
002307120518     c                   kfld                    pariva
002308120518     c*
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
003701120518     c                   if        pardtin > 0
003702120518     c                   z-add     pardtin       dtinik
003703120518     c                   else
003704120518     c                   move      *date         dtinik
003705120604     c                   move      dtinik        pardtin
003706120518     c                   endif
003707120518     c                   if        pardtfi = 0
003708120518     c                   move      *date         pardtfi
003709120518     c                   endif
003710120518     c                   clear                   esito
003711120518     c                   clear                   errmsg
003713120518     c                   clear                   DSATS
004001120518     c     kats          setgt     aiats03l
004002120518     c     kats2         readpe    aiats03l
004003120518     c                   if        %eof(aiats03l)
004004120604     c                   move      ' '           esito
004005120518     c                   eval      errmsg = 'Non trovata anagrafica -
004006120518     c                             come trazionista '
004008120518     c                   else
004009121009     c                   if        atsdec = 0 or atsdfc < pardtin
004010120604     c                   eval      esito = ' '
004011120604     c                   eval      errmsg = 'Non trovata anagrafica -
004012120604     c                             come trazionista '
004013120604     c                   goto      fine
004014120604     c                   endif
005400030109     c
005401120518     c                   eval      esito = '1'
005402120502      * se lo trovo fra i trazionisti e ho inserito le date verifico anche periodo di validità
005403120604     c                   if        esito = '1'
005405120502     c                   select
005406120502     c                   when      pardtin < atsdec
005407120604     c                   eval      esito = '2'
005408120604     c                   eval      errmsg = 'Contratto con decorrenza -
005409120604     c                             futura'
005410120508     c                   when      pardtfi > 0 and atsdfc > 0 and
005411120508     c                             pardtfi > atsdfc
005412120604     c                   eval      esito = '3'
005413120604     c                   eval      errmsg = 'Contratto con data mino-
005414120604     c                             re alla data di fine passata'
005415120716      *società fornitore diversa da società traini
005416120716     c                   when      parsoc <> soctraini and parsoc <> 0
005417120716     c                   eval      esito = '4'
005418120716     c                   eval      errmsg = 'Società fornitore -
005419120716     c                             diversa da società traini'
005420120502     c                   endsl
005421120502     c                   endif
005422120502     c
005423120518     c                   endif
005500020624      *
005501120604     c     fine          tag
005600030109     c                   seton                                        lr
005700011129     C*--------------------------------------------------------------------
