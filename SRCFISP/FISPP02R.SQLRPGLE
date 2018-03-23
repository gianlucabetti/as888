000100080620     H DATEDIT(*YMD.)
000200080620     h dftactgrp(*no) actgrp(*caller)
000300080620      //********************************************************************************************
000400080620      //
000500080620      // Definizione variabili.
000600080620      //
000700080620      //********************************************************************************************
000800080620     FFItr400f  o  a E             DISK
000900080620
001000080620     D codicepc        S              1
001100080620     D filiale         S              3
001200080620     D Spunta          S            256
001300080620     D RTNesito        S             10I 0
001400080620     D RTNopcode       S             10
001500080620     D RTNstatus       S             10I 0
001600080620     D Wdata           S              8  0
001700080620     D Wtime           S              6  0
001800080620     d dataiso         s               d   datfmt(*iso)
001900080620
002000080620      //********************************************************************************************
002100080620      //
002200080620      // Definizione parametri procedura.
002300080620      //
002400080620      //********************************************************************************************
002500080620     C     *ENTRY        PLIST
002600080620     C                   PARM                    codicepc
002700080620     C                   PARM                    filiale
002800080620     C                   PARM                    Spunta
002900080620     C                   PARM                    rtnesito
003000080620     C                   PARM                    rtnopcode
003100080620     C                   PARM                    rtnstatus
003200080620
003300080620       //*******************************************************************************************
003400080620
003500080620     c                   eval      *inlr = *on
003600080620
003700080620     C                   clear                   rtnesito
003800080620     C                   clear                   rtnopcode
003900080620     C                   clear                   rtnstatus
004000080620     C                   clear                   wtime
004100080620     C                   clear                   wdata
004200080620     C                   clear                   dataiso
004300080620     C                   clear                   FItr4
004400080620
004500080620     c                   movel     filiale       tr4fil
004600080620     c                   movel     codicepc      tr4pc
004700080623     c                   movel     Spunta        tr4DA1
004800080620     c                   time                    dataiso
004900080620     c                   move      dataiso       wdata
005000080620     c                   TIME                    Wtime
005100080620     c                   movel     Wdata         tr4datora
005200080620     c                   move      Wtime         tr4datora
005300080620
005400080620     c                   WRITE(e)  FItr4
005500080620
005600080620     C                   IF        %error
005700080620     c                   eval      RTNesito  = -1
005800080620     c                   eval      RTNstatus = %status
005900080620     c                   eval      RTNopcode = 'STATUS'
006000080620     c                   RETURN
006100080620     c                   ENDIF
006200080620
006300080620     c                   RETURN
