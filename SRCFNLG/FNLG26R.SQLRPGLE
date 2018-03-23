000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200070808     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000300000000     H*--------------------------------------------------------------*
000400070808      * Il programma viene schedulato ogni inizio mese partendo dalla
000500070808      * udate toglie un mese per ricavare la data di confronto
000600070808      * Aggiorna le giacenze in fase =50 con data chiusura inferiore
000700070808      * alla data di confronto impostando fase = 60 la udate come data
000800070808      * di stampa e tipo stampa = N (non stampata)
000900070808      *
001000020624     d kpjba         e ds
001100070808     d dataiso         s               d   datfmt(*iso)
001200020624     D*
001300070808     d
001400011026
001500000000     C*---------------------------------------------------------------*
001600020624     c     *entry        plist
001700020624     c                   parm                    kpjba
001800070808     c                   move      *date         dataiso
001900070808     c                   move      dataiso       datau             8 0
002000070808     c     dataiso       subdur    1:*m          dataiso
002100070808     c                   move      dataiso       dataup            8 0
002200020624     C/EXEC SQL
002300070808     C+ update tigcp01l set gcpfas = 60, gcpdlm = :datau, gcptfg = 'N'
002400070808     c+ where gcpfas = 50 and gcpdcg < :dataup
002500020624     C/END-EXEC
002600020624      *
002700050207      *
002800020624     c                   seton                                        lr
002900011129     C*--------------------------------------------------------------------
